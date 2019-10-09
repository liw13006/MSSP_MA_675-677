library(readr)
pacman::p_load(pracma,lubridate)
#FSPTX <- read_csv("FSPTX.csv")
#NASDAQ <- read_csv("NASDAQ.csv")
#SP500 <- read_csv("SP500^GSPC.csv")
library(tidyverse)
library(dplyr)

## this function requires a dataframe input that has the daily close price named:Closed and a date column named: Date with format as: floating point and "xxxx(year)-xx(month)-xx(day)"
## We trimmed the data from 1990-10-07 because 1990-10-08 is a Monday and Stock market closed during weekend
dailynlogReturn <- function(Date1,DataFrame){
  DataFrame = mutate(DataFrame, dailyReturn = (Close-lag(Close))/Close)%>%mutate(perc_dailyRe = round(dailyReturn*100.0,3))%>%mutate(log.Close = log(Close))%>%filter(Date >= Date1)%>%filter(Date <= as.Date("2018-12-31"))
}
## This function returns a projection value of the fund from the start date and assuming 10k investment from the start and reinvest all earnings
getProjectionValue <- function(DF){
  P0 = pull(filter(DF, Date == pull(top_n(DF["Date"],-1)))%>%select(Close))
  DF = mutate(DF,ProjValper10k = (Close*10000)/P0)
}

sqerr <- function(x,y){
  z = x - y
  z = sqrt(dot(z,z)/length(y))
  return(z)
}

standardizedNAV = function(DF){
  return(mutate(DF,Close.z = (Close-mean(Close))/sd(Close)))
}
StartDate = as.Date("2014-01-01")
FSPTX = dailynlogReturn(StartDate,read_csv("FSPTX.csv"))
NASDAQ = dailynlogReturn(StartDate,read_csv("NASDAQ.csv"))
SnP500 = dailynlogReturn(StartDate,read_csv("^GSPC.csv"))
SnPMID = dailynlogReturn(StartDate,read_csv("^MID.csv"))
SnPSML = dailynlogReturn(StartDate,read_csv("^SML.csv"))
RUSSELL2000 = dailynlogReturn(StartDate,read_csv("^RUT.csv"))
VGT = dailynlogReturn(StartDate,read_csv("VGT.csv"))
VIGAX = dailynlogReturn(StartDate,read_csv("VIGAX.csv"))
VTSAX = dailynlogReturn(StartDate,read_csv("VTSAX.csv"))

## Standardize NAV
FSPTX = standardizedNAV(FSPTX)
NASDAQ = standardizedNAV(NASDAQ)
SnP500 = standardizedNAV(SnP500)
SnPMID = standardizedNAV(SnPMID)
SnPSML = standardizedNAV(SnPSML)
RUSSELL2000 = standardizedNAV(RUSSELL2000)
VGT = standardizedNAV(VGT)
VIGAX = standardizedNAV(VIGAX)
VTSAX = standardizedNAV(VTSAX)


#DaysBeatNasdaq = FSPTX$dailyReturn-NASDAQ$dailyReturn
ggplot(FSPTX)+geom_line(mapping = aes(x = Date,y = Close.z,color = "FSPTX"))+geom_line(mapping = aes(x = NASDAQ$Date,y = NASDAQ$Close.z,color = "NASDAQ"),alpha = .4)+geom_line(mapping = aes(x = SnPMID$Date,y = SnPMID$Close.z,color = "S&PmidCAP"),alpha = .4)+geom_line(mapping = aes(x = SnPSML$Date,y = SnPSML$Close.z,color = "S&PsmlCAP"),alpha = .4)+geom_line(mapping = aes(x = RUSSELL2000$Date,y = RUSSELL2000$Close.z,color = "Russell2000"),alpha = .4)+ylab("standardized NAV")

## Check Correlation
cor(FSPTX$Close.z,NASDAQ$Close.z)
cor(FSPTX$Close.z,RUSSELL2000$Close.z)
cor(FSPTX$Close.z,SnPMID$Close.z)
cor(FSPTX$Close.z,SnPSML$Close.z)
cordat = cbind(FSPTX$Close.z,NASDAQ$Close.z,SnP500$Close.z,RUSSELL2000$Close.z,SnPMID$Close.z,SnPSML$Close.z,VGT$Close.z,VIGAX$Close.z,VTSAX$Close.z)
colnames(cordat) = c("FSPTX","NASDAQ","SnP500","RUSSELL2000","SnPMID","SnPSML","VGT(IT ETF)","VIGAX(LargeCAP)","VTSAX(TotalMarket)")
cordat = data.frame(cordat)
cor(cordat,cordat)
#pull(filter(FSPTX, Date == StartDate)%>%select(dailyReturn))


#Project future value

FSPTX = getProjectionValue(FSPTX)
NASDAQ = getProjectionValue(NASDAQ)
SnP500 = getProjectionValue(SnP500)
VGT = getProjectionValue(VGT)
VIGAX = getProjectionValue(VIGAX)
VTSAX = getProjectionValue(VTSAX)
SnPMID = getProjectionValue(SnPMID)
SnPSML = getProjectionValue(SnPSML)
RUSSELL2000 = getProjectionValue(RUSSELL2000)

#P0 = pull(filter(FSPTX, Date == StartDate)%>%select(Close))
#FSPTX = mutate(FSPTX,ProjValper10k = (Close*10000)/P0)

#P0 = pull(filter(NASDAQ, Date == StartDate)%>%select(Close))
#NASDAQ = mutate(NASDAQ,ProjValper10k = (Close*10000)/P0)

#length(DaysBeatNasdaq[DaysBeatNasdaq>0])/length(DaysBeatNasdaq)


ggplot()+geom_line(mapping = aes(x = FSPTX$Date,y = FSPTX$ProjValper10k,color = 'FSPTX'))+geom_line(mapping = aes(x = NASDAQ$Date,y = NASDAQ$ProjValper10k,color = 'NASDAQ'),size = 1.5,alpha = .6)+geom_line(mapping = aes(x = SnP500$Date,y = SnP500$ProjValper10k,color = 'S&P500'),size = 1.5,alpha = .6)+geom_line(mapping = aes(x = VGT$Date,y = VGT$ProjValper10k,color = 'VGT(ITsecETF)'),size = 1,alpha = .2)+geom_line(mapping = aes(x = VIGAX$Date,y = VIGAX$ProjValper10k,color = 'VIGAX(LARGECAPGROWTH)'),size = 1,alpha = .2)+geom_line(mapping = aes(x = VTSAX$Date,y = VTSAX$ProjValper10k,color = 'VTSAX(TOTALSTOCK)'),size = 1,alpha = .2)+geom_line(mapping = aes(x = SnPMID$Date,y = SnPMID$ProjValper10k,color = 'S&PmidCAP'),size = 1,alpha = .2)+geom_line(mapping = aes(x = SnPSML$Date,y = SnPSML$ProjValper10k,color = 'S&PsmlCAP'),size = 1,alpha = .2)+xlab("Date")+ylab("HypoGrowth of 10,000")

## Check distance between lines
print("pointwise variance between taget fund and NASDAQ")
print(sqerr(FSPTX$ProjValper10k,NASDAQ$ProjValper10k))
#ks.test()
print("pointwise variance between taget fund and S&P500")
print(sqerr(FSPTX$ProjValper10k,SnP500$ProjValper10k))
print("pointwise variance between taget fund and IT sector ETF")
print(sqerr(FSPTX$ProjValper10k,VGT$ProjValper10k))
print("pointwise variance between taget fund and Large cap growth index fund")
print(sqerr(FSPTX$ProjValper10k,VIGAX$ProjValper10k))
print("pointwise variance between taget fund and Total Stock market index fund")
print(sqerr(FSPTX$ProjValper10k,VTSAX$ProjValper10k))
#ggplot(FSPTX)+aes(x = Date , y=perc_dailyRe) + geom_line()

## Compare daily returns
dailyReturnComp = cbind(as.Date(FSPTX$Date),FSPTX$dailyReturn,NASDAQ$dailyReturn,SnP500$dailyReturn,VGT$dailyReturn,VIGAX$dailyReturn,VTSAX$dailyReturn)
colnames(dailyReturnComp) = c("Date","FSPTX","NASDAQ","SnP500","VGT","VIGAX","VTSAX")
epsilon = 0.000000000000000001
dailyReturnComp = data.frame(dailyReturnComp)%>%mutate(Date = as_date(Date),vsNASDAQ = ifelse(NASDAQ*NASDAQ<=epsilon,FSPTX,FSPTX/NASDAQ),vsSnP500 = ifelse(SnP500*SnP500<=epsilon,FSPTX,FSPTX/SnP500),vsVGT = ifelse(VGT*VGT<=epsilon,FSPTX,FSPTX/VGT),vsVIGAX = ifelse(VIGAX*VIGAX<=epsilon,FSPTX,FSPTX/VIGAX),vsVTSAX = ifelse(VTSAX*VTSAX<=epsilon,FSPTX,FSPTX/VTSAX))%>%mutate(minusNASDAQ = FSPTX-NASDAQ,minusSnP500 = FSPTX-SnP500,minusVGT = FSPTX-VGT,minusVIGAX= FSPTX-VIGAX,minusVTSAX = FSPTX-VTSAX)

print("Average ratio of dailyReturn(FSPTX/NASDAQ")
print(sum(sqrt(dailyReturnComp$vsNASDAQ*dailyReturnComp$vsNASDAQ)/length(dailyReturnComp$vsNASDAQ)))
print("Average ratio of dailyReturn(FSPTX/S&P500")
print(sum(sqrt(dailyReturnComp$vsSnP500*dailyReturnComp$vsSnP500)/length(dailyReturnComp$vsSnP500)))
print("Average ratio of dailyReturn(FSPTX/VGT")
print(sum(sqrt(dailyReturnComp$vsVGT*dailyReturnComp$vsVGT)/length(dailyReturnComp$vsVGT)))
print("Average ratio of dailyReturn(FSPTX/VIGAX")
print(sum(sqrt(dailyReturnComp$vsVIGAX*dailyReturnComp$vsVIGAX)/length(dailyReturnComp$vsVIGAX)))
print("Average ratio of dailyReturn(FSPTX/NASDAQ")
print(sum(sqrt(dailyReturnComp$vsVTSAX*dailyReturnComp$vsVTSAX)/length(dailyReturnComp$vsVTSAX)))

dailyReturnSTD = select(dailyReturnComp,contains("minus"))%>%summarise_all(funs(sd))
ggplot(dailyReturnComp)+aes(x = Date,y = minusNASDAQ)+geom_point(alpha = .1)+geom_smooth(method = "loess",se = TRUE)
ggplot(dailyReturnComp)+aes(x = Date,y = minusSnP500)+geom_point(alpha = .1)+geom_smooth(method = "loess",se = TRUE)
ggplot(dailyReturnComp)+aes(x = Date,y = minusVGT)+geom_point(alpha = .1)+geom_smooth(method = "loess",se = TRUE)
ggplot(dailyReturnComp)+aes(x = Date,y = minusVIGAX)+geom_point(alpha = .1)+geom_smooth(method = "loess",se = TRUE)
ggplot(dailyReturnComp)+aes(x = Date,y = minusVTSAX)+geom_point(alpha = .1)+geom_smooth(method = "loess",se = TRUE)
ggplot(dailyReturnComp)+aes(x = FSPTX,y = NASDAQ)+geom_point()+xlim(c(-0.15,0.15))+ylim(c(-0.15,0.15))+geom_abline(intercept = 0.0,slope = 1.0)
ggplot(dailyReturnComp)+aes(x = FSPTX,y = SnP500)+geom_point()+xlim(c(-0.15,0.15))+ylim(c(-0.15,0.15))+geom_abline(intercept = 0.0,slope = 1.0)
ggplot(dailyReturnComp)+aes(x = FSPTX,y = VGT)+geom_point()+xlim(c(-0.15,0.15))+ylim(c(-0.15,0.15))+geom_abline(intercept = 0.0,slope = 1.0)
ggplot(dailyReturnComp)+aes(x = FSPTX,y = VIGAX)+geom_point()+xlim(c(-0.15,0.15))+ylim(c(-0.15,0.15))+geom_abline(intercept = 0.0,slope = 1.0)
ggplot(dailyReturnComp)+aes(x = FSPTX,y = VTSAX)+geom_point()+xlim(c(-0.15,0.15))+ylim(c(-0.15,0.15))+geom_abline(intercept = 0.0,slope = 1.0)


#as.integer(as.Date("2010-10-04"))

getTS = function(DF){
  tsFSPTX = select(DF,Date,ProjValper10k)%>%drop_na()
  tsFSPTX1 = ts(tsFSPTX,frequency = 1)
}
#%>%column_to_rownames(var = "Date")
tsFSPTX1 = getTS(FSPTX)
d_tsFSPTX = decompose(tsFSPTX1)
tsNASDAQ = getTS(NASDAQ)
plot(tsFSPTX1)
plot(tsNASDAQ)
ts.plot(tsFSPTX1, tsNASDAQ, gpars = list(col = c("black", "red")))

selectDate = function(DF,startdate,enddate){
  DF = select(DF,Date,Close.z)%>%filter(Date>=startdate,Date<=enddate)
  return(DF)
}

adequateTStable = function(DF){
  
}
for (i in 2014:2018){
  date1 = as.Date(paste0(toString(i),"-01-01"))
  date2 = as.Date(paste0(toString(i),"-12-31"))
  #print(date1)
  #print(date2)
  
}
