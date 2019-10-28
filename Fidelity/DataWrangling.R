pacman::p_load(pracma,lubridate,tidyr,dplyr,corrplot,ggplot2,readr)
SnP500Info <- read_csv("SnP500Info.csv")
SnP500Info <-SnP500Info%>%select(1,4)
SnP500Info <- SnP500Info%>%separate(Date,into = c("Month","Day","Year"),sep = "/")%>%mutate(Date = paste0("20",Year,"-",Month,"-",Day))%>%select(Date,4)

colnames(SnP500Info) <- c("Date","SnP500Info")
SnP500Info <- SnP500Info%>%mutate(Date = as.Date(Date,format = "%Y-%m-%d"))%>%drop_na()

write_csv(SnP500Info,"SnP500Info.csv")

SnPNATECH <- read_csv("SnPNATECH.csv")

SnPNATECH <- SnPNATECH%>%select(1,3)%>%separate(Date,into = c("Month","Day","Year"),sep = "/")%>%mutate(Date = paste0("20",Year,"-",Month,"-",Day))%>%select(Date,4)
colnames(SnPNATECH) <- c("Date","SnPNATECH")
SnPNATECH <- SnPNATECH%>%mutate(Date = as.Date(Date,format = "%Y-%m-%d"))%>%drop_na()
write_csv(SnPNATECH,"SnPNATECH_clean.csv")

SnP500Info <- read_csv("SnP500Info.csv")
SnPNATECH <- read_csv("SnPNATECH_clean.csv")
colnames(SnP500Info) <- c("Date","Close")
colnames(SnPNATECH) <- c("Date","Close")
write_csv(SnPNATECH,"SnPNATECH_clean.csv")
write_csv(SnP500Info,"SnP500Info.csv")
