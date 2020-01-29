#rm(list = ls())
library(ez)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(Matrix)
library(lme4)

filename1 <- "play_a_027"
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

subdata1 <- subdata1 %>%
  mutate(click_loc = ifelse(Tiloc == click, "tiloc", ifelse(
    Tloc == click, "tloc", ifelse(Cloc == click, "cloc", "ciloc")
  ))) %>%
  as.data.frame()

subdata1$click_loc <- replace(subdata1$click_loc, subdata1$click_loc=='tloc', "ta")
subdata1$click_loc <- replace(subdata1$click_loc, subdata1$click_loc=='tiloc', "ti")
subdata1$click_loc <- replace(subdata1$click_loc, subdata1$click_loc=='cloc', "da")
subdata1$click_loc <- replace(subdata1$click_loc, subdata1$click_loc=='ciloc', "di")



write.csv(subdata1,file = "play_a_027_mouseclick.csv",row.names=T)

