rm(list = ls())
library(ez)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(Matrix)
library(lme4)

filename1 <- "play_a_001"
path <- getwd()

##### load files #####
Inputdir <- paste(path,"/",filename1,".csv",sep = "")
Data <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")

Data$click <-NA
Data$click_loc <-NA

## Delete filter and practice trial
subdata <- subset(Data, Condition!="filler" & Condition!="Practice" & AOI!= "0")

## Keep only one data point for each trial
subdata1 <- subdata[!duplicated(subdata$TrialID),]

## Mouse click
# LR=20 ; TL=21 ; LL=22 ; TR=23
lengttrial <- length(subdata1$TrialID)

for (i in 1:lengttrial) {
  if (subdata1$Clickx[i] < 960 && subdata1$Clicky[i] < 540) {
    subdata1$click[i] = "TL"
  } else if (subdata1$Clickx[i] < 960 && subdata1$Clicky[i] > 540) {
    subdata1$click[i] = "LL"
  } else if (subdata1$Clickx[i] > 960 && subdata1$Clicky[i] < 540) {
    subdata1$click[i] = "TR"
  } else if (subdata1$Clickx[i] > 960 && subdata1$Clicky[i] > 540) {
    subdata1$click[i] = "LR"
  } else {
  }
}


write.csv(subdata1,file = "play_a_001_mouseclick.csv",row.names=T)