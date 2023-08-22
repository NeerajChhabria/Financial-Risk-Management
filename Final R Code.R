
setwd("C:/Users/nchha/OneDrive/Desktop/Final Portfolio/Financial Risk Management")
library(RPostgres)
library(data.table)
library(ggbiplot)
library(data.table)
library(tidyverse)
library(quantmod)

FundHolding <- fread(file.choose())   # Fund Holding.csv

#Importing Fund Holding.csv
#This file contains following four columns
#1- ticker: this is the ticker symbol of a stock
#2- weight: this is the weight of the stock in the portfolio
#3- permno: this is the “permno” for the stock in the CRSP database available on WRDS
#4- security_name: the name of the company

View(FundHolding)


#############  COMMON SOURCES OF RISK USING QUALITATIVE ANALYSIS  ############### 

Zacks<-fread(file.choose())
#Importing Zacks_2019-12-31.csv
#This file contains the following descriptors for each stock ticker:
#1- Ticker: Ticker symbol
#2- SharesOut: the number of shares outstanding (in millions)
#3- ZacksSector: the sector of the stock (see below for description)
#4- Beta: the CAPM beta
#5- Price: the closing price on 2019-12-31
#6- Name: the name of the company
#   (Note: there are 16 Zacks sectors, which are different from the 11 SPDR sectors) 

names(Zacks)[1]<-"ticker"
View(Zacks)


Holding_Final<-inner_join(Zacks, FundHolding, by="ticker")
sum(Holding_Final$weight)
View(Holding_Final)


#1: Diversification Via Market Capitalization


MarketCap<-Holding_Final$SharesOut*Holding_Final$Price
Holding_Final<-cbind(Holding_Final,MarketCap)

categorization<-function(x){
  if(x>=300000) {
    print("Mega")
  }else if(x>=10000) {
    print("Large")
  }else if(x>=2000) {
    print("Mid")
  }else if(x>=300){
    print("Small")
  }else if(x>50){
    print("Micro")
  }else{
    print("Nano")
  }
}
#This Function "Categorization" categorizes the stocks in this fund in Mega,
#Large, Mid, Small, Micro, Nano given their Market Capitalization


Category<-sapply(Holding_Final$MarketCap,categorization)
Holding_Final<-cbind(Holding_Final,Category)
View(Holding_Final)
#Run the above code to view Holding_Final. This time with addition of a new column
#named as Category. 

write.csv(Holding_Final, "FinalHolding.csv", row.names = TRUE)
#Holding_Final is inner join of file Fund Holding and Zacks. This is the Final 
#Dataframe that we'll be working for Qualitative Analysis


WeightsMarketCap<-Holding_Final%>%
  group_by(Category)%>%
  summarise(NumberofStocks=n(),WeightByMarketCap=sum(weight))

WeightsMarketCap
# This shows number of stocks and the weight of the mutual fund allocated to each
#of the six market cap categories.


#2: Diversification Via Zacks Sectors

WeightsZackSector<-Holding_Final%>%
  group_by(ZacksSector)%>%
  summarize(NumberofStocks=n(), WeightbyZacks=sum(weight))
WeightsZackSector

WeightsIntersection<-Holding_Final%>%
  group_by(Category, ZacksSector)%>%
  summarise(NumberofStocks=n(),WeightIntersection=sum(weight))
WeightsIntersection

View(WeightsIntersection)
# This shows number of stocks and the weight of the mutual fund allocated to each
#of the sixteen Zacks Sector Categories.


#3: CAPM Beta of this Fund

FundBeta<-Holding_Final%>%
  mutate(betaWeights=Beta*weight)

FundBeta<-sum(FundBeta$betaWeights)
FundBeta

########### COMMON SOURCES OF RISK USING QUANTITATIVE ANALYSIS (PCA) ###############

#Getting all the data 

# Taking into consideration 10 years of monthly discrete returns.

#############################################################################
Holding_Final[1,1]<-"AMZN"
Holding_Final[7,1]<-"BRK-A"
Holding_Final[5,1]<-"BAC"
Holding_Final[11,1]<-"META"
Holding_Final[18,1]<-"RTX"
#Adjusting the Tickers to retrieve data from Yahoo.Finance

Holding_Final

Tickers <- Holding_Final$ticker 
ntick <- length(Tickers)
Tickers
start_date <- "2010-01-01"   
end_date <-"2019-12-31"

num <- 0
for (i in 1:ntick) {
  tkr <- Tickers[i]
  dat <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
  print(dat[1,])
  if (num == 0) {
    All <- dat[,6]
    num <- 1
  } else {
    All <- merge(All,dat[,6],join="outer")
  }      
  print(tkr)
}
names(All) <- as.character(Tickers[1:ntick])
save(All,file='FundData')
View(All)


# The Stock BABA i.e AliBaba started trading on 2014-09-22. Hence we revise the 
#start date of retrieved data from Yahoo Finance.
class(All)

All <- All["2014-09-22/2019-12-31"]
length(All)
X <- diff(log(All))
X <- X[-1,]
length(X$AMZN)
# Calculating the Log Returns.

# X is the data table for returns of 19 stocks in the Portfolio


#Data for Style One

Tickers <- c("SPYV","SPYG","MDYV","MDYG","SLYV","SLYG")
ntick <- length(Tickers)
start_date <- "2014-09-22"
end_date <- "2019-12-31"

num <- 0
for (i in 1:ntick) {
  tkr <- Tickers[i]
  dat <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
  print(dat[1,])
  if (num == 0) {
    All <- dat[,6]
    num <- 1
  } else {
    All <- merge(All,dat[,6],join="outer")
  }      
  print(tkr)
}
names(All) <- as.character(Tickers[1:ntick])

Styles1 <- diff(log(All))
Styles1 <- Styles1[-1,]
length(Styles1$SPYV)

#Data for Styles II

Tickers <- c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")
ntick <- length(Tickers)
start_date <- "2014-09-22"
end_date <- "2019-12-31"

num <- 0
for (i in 1:ntick) {
  tkr <- Tickers[i]
  dat <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
  print(dat[1,])
  if (num == 0) {
    All <- dat[,6]
    num <- 1
  } else {
    All <- merge(All,dat[,6],join="outer")
  }      
  print(tkr)
}
names(All) <- as.character(Tickers[1:ntick])

Styles2 <- diff(log(All))
Styles2 <- Styles2[-1,]

load("rspy.rda")
SP500 <- rspy["2014-09-23/2019-12-30"]
# Log Returns for S&P 500 

#Defining Function "corfun"

corfun <- function(X,digit=2) {
  ncol  <- ncol(X)
  nobs  <- nrow(X)
  cor_X <- matrix(rep("",ncol*ncol),nrow=ncol,ncol=ncol)
  corx  <- cor(X)
  se    <- sqrt((1-corx)/(nobs-2))
  avg   <- 0
  for (i in 2:ncol) {
    avg <- avg+sum(abs(corx[i,1:(i-1)]))   #  average of |corr| 
    tst <- abs(corx[i,1:(i-1)]/se[i,1:(i-1)])
    cor_X[i,1:(i-1)] <- ifelse(tst>2,as.character(round(corx[i,1:(i-1)],digit)),"")
  }
  avg <- avg/(ncol*(ncol-1)/2)
  for (i in 1:ncol) {
    cor_X[i,i] <- "1"
  }
  rownames(cor_X) <- colnames(X)
  colnames(cor_X) <- colnames(X)
  corout <- list(noquote(cor_X),(sum(cor_X!="")-ncol),avg)
  return(corout)  
}
# corfun() returns a list of 3 objects:
#first object:  a correlation matrix, entries have |t|>2
#second object: the number of entries of the correlation matrix with |t|>2
#third object:  the average of the absolute values of the correlation coefficients

corout_X <- corfun(X)

noquote(sprintf(fmt="%s %i","no of correlation = ",ntick*(ntick-1)/2))
noquote(sprintf(fmt="%s %i","no of corr |t|>2  = ",corout_X[[2]]))
noquote(sprintf(fmt="%s %6.2f","avg |correlation| = ",corout_X[[3]]))

#correlation matrix for 19 Fund components
corout_X[[1]][1:19,1:19]


#Defining Function for Principal Component Analysis
pcfun <- function(X) {
  df <- as.data.frame(X)
  pc <- prcomp(X)
  vname <- seq(1:length(pc$sdev))
  eis <- data.frame( vname=vname,var=pc$sdev^2 / sum(pc$sdev^2) )
  if (nrow(eis)>10) { eis <- eis[1:10,] }
  vlevel <- as.character(seq(1:length(pc$sdev)))
  eis$vname <- factor(eis$vname,levels=vlevel)
  p <- ggplot(data=eis,aes(y=var)) +
    geom_bar(aes(x=vname),stat="identity",col="grey",alpha=0.7) + 
    ylim(0,1) +
    xlab("Principal Component")+
    ylab("Explained Variance") 
  pcx <- as.data.frame(pc$x[,1:5])
  cor_table <- matrix(rep(0,5*ncol(df)),nrow=ncol(df),ncol=5)
  nobs <- nrow(X)
  for (i in 1:ncol(df)) {
    cor_pc <- cor(df[,i],pcx)
    #    se_pc  <- sqrt((1-cor_pc)/(nobs-2))
    #    tst_pc <- abs(cor_pc/se_pc)>2
    cor_table[i,] <- cor_pc
    #    tst_table[i,] <- tst_pc
  }
  se_table <- sqrt((1-cor_table)/(nobs-2))
  tst_table<- cor_table/se_table
  cor_table <- ifelse(abs(tst_table)>2,cor_table,0)
  row.names(cor_table) <- colnames(X)
  colnames(cor_table)  <- c("pc1","pc2","pc3","pc4","pc5")
  pcout <- list(pc,eis,p,cor_table)
  return(pcout)
}

# pcfun returns a list with 4 objects
# first object:  the result of prcomp()
# second object: the fraction of variance explained by the first ten principal components
# third object:  the graph of the fraction of variance explained by the first ten principal components
# fourth object: the correlation table of each column of X against the first 5 PCs

pcout <- pcfun(X)
pcout[[3]]+ggtitle("PC of 19 Stocks in the Portfolio")

e1<-pcout[[2]]$var[1]
e1
# Variance Explained with the First Principal Component

e2<-pcout[[2]]$var[2]
e2
# Variance Explained with the Second Principal Component

e3<-pcout[[2]]$var[3]
e3
# Variance Explained with the Third Principal Component


pc_1 <- pcout[[1]]$x[,1]
length(pc_1)
z1 <- as.numeric(cor(pc_1,SP500))
noquote(sprintf(fmt="%s %6.2f","Corr(PC1,spy) = ",z1))

############### RESIDUALS OF ONE FACTOR MODEL  ######################
# regress stocks in Fund on spy and save residuals
resd <- matrix(0,nrow=length(SP500),ncol=ntick)
for (j in 1:ntick) {
  df <- data.frame(SP500,X[,j])
  names(df) <- c("xvar","yvar")
  reg <- lm(yvar~xvar,df)
  resd[,j] <- reg$residuals
}
colnames(resd) <- colnames(X)
cor_res <- corfun(resd)
cor_res[[1]][1:19,1:19]

noquote(sprintf(fmt="%s %6.2f","avg of |correlation| = ",cor_res[[3]]))
noquote(sprintf(fmt="%s %i","no of correlation = ",ntick*(ntick-1)/2))
noquote(sprintf(fmt="%s %i","no of corr |t|>2 = ",cor_res[[2]]))


pcout_res <- pcfun(resd)
pcout_res[[3]]+ggtitle("PC of residuals of 19 Fund Stock stocks")
e1_res<-pcout_res[[2]]$var[1]
eres <- pcout_res[[2]]$var
round(e1_res,2)
noquote(sprintf(fmt="%s %6.2f","Variance explained by PC1 = ",e1_res))



round(pcout_res[[4]],2)             ##################Important############## 

#Analysis of Residuals of Principal Component Analysis
Styles1
as.numeric(cor(pcout_res[[1]]$x[,1],SP500))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles1$SPYV))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles1$SPYG))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles1$MDYV))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles1$MDYG))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles1$SLYV))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles1$SLYG))

Styles2
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLB))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLE))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLF))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLI))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLK))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLU))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLP))
as.numeric(cor(pcout_res[[1]]$x[,1],Styles2$XLV))

#Second Principal Component Aanlaysis

Styles1
as.numeric(cor(pcout_res[[1]]$x[,2],SP500))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles1$SPYV))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles1$SPYG))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles1$MDYV))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles1$MDYG))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles1$SLYV))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles1$SLYG))

Styles2
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLB))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLE))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLF))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLI))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLK))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLU))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLP))
as.numeric(cor(pcout_res[[1]]$x[,2],Styles2$XLV))

# Third Principal Component

Styles1
as.numeric(cor(pcout_res[[1]]$x[,3],SP500))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles1$SPYV))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles1$SPYG))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles1$MDYV))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles1$MDYG))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles1$SLYV))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles1$SLYG))

Styles2
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLB))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLE))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLF))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLI))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLK))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLU))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLP))
as.numeric(cor(pcout_res[[1]]$x[,3],Styles2$XLV))

############################## 3 PRO FORMA RETURN OF THIS FUND PORTFOLIO ####################################

MatrixStocks<-as.matrix(All)
MatrixWeights<-as.matrix(Holding_Final$weight)

ProFormaReturn<-as.vector(MatrixStocks%*%MatrixWeights)
ProFormaReturn

logret <- diff(log(ProFormaReturn))
length(X$AMZN)
length(logret)

# CALCULATION OF VAR AND ES OF THE PORTFOLIO

library(data.table)
library(ggplot2)
library(MASS)
library(metRology)
library(quantmod)
library(xts)

#normal distribution
n_fit <- fitdistr(logret,"normal")
mu <- n_fit$estimate[1]
sig<- n_fit$estimate[2]
fmt <- "%s %9.6f"
noquote(sprintf (fmt,"Mean: ",mu))
noquote(sprintf (fmt,"SD:  ",sig))
noquote(sprintf ("%s %9.2f","Loglik:",n_fit$loglik))

#t distribution
t_fit <- fitdistr(logret,"t")
round(t_fit$estimate,6)
round(t_fit$loglik,2)
m <- t_fit$estimate[1]
s <- t_fit$estimate[2]
tdf <- t_fit$estimate[3]

#AIC comparison
AIC.n <- 2*length(n_fit$estimate) - 2*n_fit$loglik
AIC.t <- 2*length(t_fit$estimate) - 2*t_fit$loglik   
noquote(sprintf(fmt="%s %8.0f","AIC of normal = ",AIC.n))
noquote(sprintf(fmt="%s %8.0f","AIC of Students t = ",AIC.t))

#GARCH MODEL
library(rugarch)
names(logret) <- index(All[-1,])
logret

uspec_n <- ugarchspec(variance.model = list(model = "sGARCH", 
                                            garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(1,0), 		
                                        include.mean = TRUE), 		
                      distribution.model = "norm")

garch_n <- ugarchfit(spec = uspec_n, data = logret, solver="hybrid")
round(garch_n@fit$coef,6)
AIC.garchn <- 2*length(garch_n@fit$coef) - 2*garch_n@fit$LLH
round(AIC.garchn,0)

#AR(1)--GARCH(1,1) model with Student t innovations
uspec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
                      distribution.model = "std") # Student t innovations
garch_t <- ugarchfit(spec = uspec_t, data = logret, solver="hybrid")
round(garch_t@fit$coef,6)
AIC.garcht <- 2*length(garch_t@fit$coef) - 2*garch_t@fit$LLH
round(AIC.garcht,0)


#AR(1,1)--GARCH(1,1) model with Student t innovations
uspec_t11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                        distribution.model = "std") # Student t innovations
garch_t11 <- ugarchfit(spec = uspec_t11, data = logret, solver="hybrid")
round(garch_t11@fit$coef,6)
AIC.garcht11 <- 2*length(garch_t11@fit$coef) - 2*garch_t11@fit$LLH
round(AIC.garcht11,0)
#AIC -8735

#AR(2)--GARCH(1,1) model with Student t innovations
uspec_t2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(2,0), include.mean = TRUE),
                       distribution.model = "std") # Student t innovations

garch_t2 <- ugarchfit(spec = uspec_t2, data = logret, solver="hybrid")

round(garch_t2@fit$coef,6)
AIC.garcht2 <- 2*length(garch_t2@fit$coef) - 2*garch_t2@fit$LLH
round(AIC.garcht2,0)
#AIC -8736


#AR(2,2)--GARCH(1,1) model with Student t innovations
uspec_t22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                        distribution.model = "std") # Student t innovations

garch_t22 <- ugarchfit(spec = uspec_t22, data = logret, solver="hybrid")

round(garch_t22@fit$coef,6)
AIC.garcht22 <- 2*length(garch_t22@fit$coef) - 2*garch_t22@fit$LLH
round(AIC.garcht22,0)
model_table <- data.table(model=c("IID~n","IID~t","AR(1)-GARCH(1,1)~n","AR(1)-GARCH(1,1)~t", "AR(1,1)-GARCH(1,1)~t", "AR(2)-GARCH(1,1)~t", "AR(2,2)-GARCH(1,1)~t"),
                          aic  = rep(0,7) )
n_fit <- fitdistr(logret,"normal")
t_fit <- fitdistr(logret,"t")
AIC.n <- 2*length(n_fit$estimate) - 2*n_fit$loglik
AIC.t <- 2*length(t_fit$estimate) - 2*t_fit$loglik  
model_table$aic <- c(AIC.n,AIC.t,AIC.garchn,AIC.garcht, AIC.garcht11, AIC.garcht2, AIC.garcht22)
model_table


min(model_table$aic)
ModelToChoose <- model_table$model[model_table$aic == min(model_table$aic)]
ModelToChoose

################## TEN DAYS AHEAD VAR SIMULATION OF PORTFOLIO#################


CalcVaRES <- function(r,alpha) {
  VaR <- quantile(r,1-alpha)
  ES  <- mean(r[r<VaR])
  VaR_ES <- c(VaR,ES)
  names(VaR_ES) <- c("VaR","ES")
  return(VaR_ES)
}
set.seed(123789)
nper <- 10
nsim <- 100000

boot_garch <- ugarchboot(garch_t,
                         method="Partial",
                         sampling="raw",
                         n.ahead=nper,
                         cluster=cl,
                         n.bootpred=nsim)
simmat <- boot_garch@fseries
sim <- apply(simmat,1,sum)
alpha <- 0.95
VaR_ES <- CalcVaRES(sim,alpha)
round(VaR_ES,6)


################################ 4. ALPHA RESEARCH AND TRADING STYLES ##################################

class(Styles1)
data <-as.data.table(Styles1)
data<-data[,FundReturn:=logret]

data<-data[,FundReturns:=logret]
data

StyleAnalysis <- lm(FundReturns ~ SPYV + SPYG + MDYV + MDYG + SLYV + SLYG, data=data)
summary(StyleAnalysis)


data1<-as.data.table(Styles2)
data1<-data1[,FundReturns:=logret]
StyleAnalysisII<-lm(FundReturns ~ XLB + XLE + XLF + XLI + XLK + XLP + XLU, data=data1)
summary(StyleAnalysisII)
