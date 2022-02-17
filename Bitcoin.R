#(i) & #(ii)
var1 <- fread("SP500.csv")
var2 <- fread("GOLDAMGBD228NLBM.csv")
var3 <- fread("DEXUSEU.csv")
var4 <- fread("DCOILWTICO.csv")
var5 <- fread("CBBTCUSD.csv")

table1 <- var1[(var1$SP500 !='.'),]
table2 <- var2[(var2$GOLDAMGBD228NLBM != '.'),]
table3 <- var3[(var3$DEXUSEU != '.'),]
table4 <- var4[(var4$DCOILWTICO != '.'),]
table5 <- var5[(var5$CBBTCUSD != '.'),]

#(iii)
merge_1 <- merge(table1,table2,all=FALSE)
merge_2 <- merge(merge_1,table3,all=FALSE)
merge_3 <- merge(merge_2,table4,all=FALSE)
merge_4 <- merge(merge_3,table5,all=FALSE)

#(iv)
ts.plot(merge_4$SP500)
ts.plot(merge_4$GOLDAMGBD228NLBM)
ts.plot(merge_4$DEXUSEU)
ts.plot(merge_4$DCOILWTICO)
ts.plot(merge_4$CBBTCUSD)

#(v)
model_ts1 <- lm(CBBTCUSD~ SP500 + GOLDAMGBD228NLBM + DEXUSEU + DCOILWTICO , data = merge_4)
summary(model_ts1)

#(vi)
merge_4$SP500 <- as.numeric(merge_4$SP500)
merge_4$GOLDAMGBD228NLBM <- as.numeric(merge_4$GOLDAMGBD228NLBM)
merge_4$DEXUSEU <- as.numeric(merge_4$DEXUSEU)
merge_4$DCOILWTICO <- as.numeric(merge_4$DCOILWTICO)
merge_4$CBBTCUSD <- as.numeric(merge_4$CBBTCUSD)

rep.kpss <- function(series,alpha=0.05,dmax=5){
  diff <- 0
  for(i in 1:dmax){
    suppressWarnings(pval <- kpss.test(series,null="Level")$p.value)
    if(pval>=alpha){
      return(c(diff,0,pval))
    }
    suppressWarnings(pval <- kpss.test(series,null="Trend")$p.value)
    if(pval>=alpha){
      return(c(diff,1,pval))
    }
    diff <- diff + 1
    series <- diff(series)
  }
  return(NULL)
}
rep.kpss(log(merge_4$SP500))
rep.kpss(log(merge_4$GOLDAMGBD228NLBM))
rep.kpss(log(merge_4$DEXUSEU))
rep.kpss(log(merge_4$DCOILWTICO))
rep.kpss(log(merge_4$CBBTCUSD))

#(vii)
model_ts2 <- lm(diff(CBBTCUSD)~ diff(SP500) + diff(GOLDAMGBD228NLBM) + diff(DEXUSEU) + diff(DCOILWTICO) , data = merge_4)
summary(model_ts2)

#(viii)
value2017 <- merge_4[(merge_4$DATE >= '2017-01-01'),]

ts.plot(value2017$SP500)
ts.plot(value2017$GOLDAMGBD228NLBM)
ts.plot(value2017$DEXUSEU)
ts.plot(value2017$DCOILWTICO)
ts.plot(value2017$CBBTCUSD)

#(ix)
acf(diff(log(value2017$CBBTCUSD)))
pacf(diff(log(value2017$CBBTCUSD)))

#(X)
model <- arima(log(value2017$CBBTCUSD),c(2,3,2))
aic_test <- AIC(model)
aic_test

#(xi)
future <- forecast(model,h=30)
future
plot(future)

#(Xii)
library(TSA)
periodogram(log(value2017$CBBTCUSD))
periodogram(diff(log(value2017$CBBTCUSD)))

#(Xiii)

diff <- diff(value2017$CBBTCUSD)
value2017$DATE <- as.Date(value2017$DATE)
value2017$weekday <- weekdays(value2017$DATE)


n <- nrow(value2017)
value2017$weekday <-  as.factor(value2017$weekday)

model_ts3 <- lm(diff~weekday [2:n], data= value2017)
model_ts3

residuals(model_ts3)
periodogram(residuals(model_ts3))

#(xiv)
x<- value2017
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}

x <- value2017%>%  dplyr::select(DCOILWTICO,CBBTCUSD,DEXUSEU,GOLDAMGBD228NLBM,SP500) %>% log %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC

model_ts4 <- VAR(x,p=1,type="both")
summary(model_ts4)

#(xv)
forcast_price <- predict(model_ts4, n.ahead = 30 ,dumvar = NULL)
plot(forcast_price)
forcast_price[1]$fcst