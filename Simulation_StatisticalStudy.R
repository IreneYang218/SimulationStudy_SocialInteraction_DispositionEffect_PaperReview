library(plyr)

####################
# Simulation data  #
####################

J <- 100
T <- 100
n <- c(1:100)
j <- c(1:J)
## select a random integer for each trader as the number of trades he excuted 
for (j in 1:J){
  set.seed(j)
  n[j] <- round(100*runif(1))
} 

N <- sum(n)

## simulate gain for trader j's trade i at time t
gain = round(runif(N*T,0,1))

##simulate error term
e  = rnorm(N*T,0,0.01)


##simulate trader fixed effect
gama  = rnorm(J,0,0.01)

## generate a data frame to save data
pData <- data.frame(id = rep(1:J, n*T),trade = rep(1:N, each = T),
                    time = rep(1:T, each = 1))


pData$gama = rep(gama, n*T)
pData$gain = gain

## Gnerate Post-myForexBook for trades
## firstly, I need to select N/2 trates which happen after entering myForexBook network
## in original paper, there are 59% trades happen after entering myForexBook. For simplicity, I use N/2
N_trade <- 1:N

post_trade_index <- sample(N_trade, round(N/2),replace = FALSE)

pData$postFB <- 0
P <- round(N/2)
for (p in 1:P){
  pData$postFB[which(pData$trade == post_trade_index[p])] = 1 
}

## calculate the possibility sale 
sale_p <- pData$gama + 0.0226*pData$gain  + 0.014*pData$postFB*pData$gain + e
## Since this paper is a causal inference study, it can not use to predit sale. I will calculate the mean, if probability of sale larger than mean*1.5, sale = 1
m<-mean(sale_p) 
pData$sale <- as.numeric(sale_p>m*1.5)

## Since each trade can only sale once, I need delet rows which lays after the first sale = 1 apear in each trade. Therefore, I can get different t for trade i
dpData <- pData# save original data

de_fun<- function(x){
  if (any(x$sale==1)==F){
    return(NULL)
  }
  else  {
    y<-x[c(1:which(x$sale==1)[1]),]
  }
}
dpData <- ddply(dpData,.variables =  .(trade),.fun=de_fun,.inform = T)# require "plyr"



#########################
# Subsample regression  #
#########################

## simulate the number of message send by trader i
beta_0 <- rnorm(J,mean=1,sd=1)

fit_fun <- function (x){
  if (empty(x)){
    return(NULL)
  }
  else {
    return(coef(lm(x$sale ~ x$gain))[2])
  }
}

beta_1 <- ddply(dpData,.(id), .fun = fit_fun)
traderDE <- beta_1[,-1]
#normalize traderDE as the author do in Table 7
traderDE <- (traderDE-mean(traderDE))/sqrt(var(traderDE))

log_mess1 <- beta_0 + (-0.0607)*traderDE + 0.0998*log(n)
message_num <- round((exp(log_mess1)-1))+1 # to let all message_num > 0

len_id <- c(1:J)
for (j in 1:J){
  len_id[j]<- length(which(dpData$id==j))
}
#generate subsample to do subsample regression according to different number of messeage send each trader
dpData$message <- rep(message_num, len_id)
Q <- quantile(message_num)
dpData_1 <- subset(dpData, (dpData$message <= Q[2]))
dpData_2 <- subset(dpData, (dpData$message > Q[2])&(dpData$message <= Q[3]))
dpData_3 <- subset(dpData, (dpData$message > Q[3])&(dpData$message <= Q[4]))
dpData_4 <- subset(dpData, (dpData$message > Q[4])&(dpData$message <= Q[5]))

fit <- lm (dpData$sale ~ dpData$gain*dpData$postFB-1 , data = dpData)#whole data set
fit1 <- lm(dpData_1$sale ~ dpData_1$gain*dpData_1$postFB-1, data = dpData_1)
fit2 <- lm(dpData_2$sale ~ dpData_2$gain*dpData_2$postFB-1, data = dpData_2)
fit3 <- lm(dpData_3$sale ~ dpData_3$gain*dpData_3$postFB-1, data = dpData_3)
fit4 <- lm(dpData_4$sale ~ dpData_4$gain*dpData_4$postFB-1, data = dpData_4)

#####################################
## logistic regression             ##
## Linear Discriminant Analysis    ##
## Quadratic Discriminant Analysis ##
#####################################
logitfit <- glm(dpData$sale ~ dpData$gain*dpData$postFB-1, family = binomial)

######################
# Create LaTax code  #
######################
# statistic description
stargazer(dpData,heap = F)
# subregression result
stargazer(fit,fit1,fit2,fit3,fit4, title = "Subsample Regression Results")
stargazer(fit,logitfit, title = "Logistic Regression Results")
