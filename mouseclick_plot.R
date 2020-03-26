#rm(list = ls())
library(plyr)
library(dplyr)
library(Matrix)

filename1 <- "play_a_020"
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


write.csv(subdata1,file = "play_a_020_mouseclick.csv",row.names=T)
# ##################### Combine all subjects' data #########################################
# ##### load files #####
# filename1 <- "play_a_001_mouseclick"
# filename2 <- "play_a_002_mouseclick"
# filename3 <- "play_a_003_mouseclick"
# filename4 <- "play_a_004_mouseclick"
# filename5 <- "play_a_005_mouseclick"
# filename6 <- "play_a_006_mouseclick"
# filename7 <- "play_a_007_mouseclick"
# filename8 <- "play_a_008_mouseclick"
# filename9 <- "play_a_009_mouseclick"
# filename10 <- "play_a_010_mouseclick"
# filename11 <- "play_a_011_mouseclick"
# filename12 <- "play_a_012_mouseclick"
# filename13 <- "play_a_013_mouseclick"
# filename14 <- "play_a_015_mouseclick"
# filename15 <- "play_a_016_mouseclick"
# filename16 <- "play_a_017_mouseclick"
# filename17 <- "play_a_018_mouseclick"
# filename18 <- "play_a_019_mouseclick"
# filename19 <- "play_a_020_mouseclick"
# filename20 <- "play_a_022_mouseclick"
# filename21 <- "play_a_023_mouseclick"
# filename22 <- "play_a_024_mouseclick"
# filename23 <- "play_a_025_mouseclick"
# filename24 <- "play_a_026_mouseclick"
# filename25 <- "play_a_027_mouseclick"
# path <- getwd()
# 
# 
# Inputdir <- paste(path,"/",filename1,".csv",sep = "")
# Data1 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename2,".csv",sep = "")
# Data2 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename3,".csv",sep = "")
# Data3 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename4,".csv",sep = "")
# Data4 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename5,".csv",sep = "")
# Data5 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename6,".csv",sep = "")
# Data6 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename7,".csv",sep = "")
# Data7 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename8,".csv",sep = "")
# Data8 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename9,".csv",sep = "")
# Data9 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename10,".csv",sep = "")
# Data10 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename11,".csv",sep = "")
# Data11 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename12,".csv",sep = "")
# Data12 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename13,".csv",sep = "")
# Data13 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename14,".csv",sep = "")
# Data14 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename15,".csv",sep = "")
# Data15 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename16,".csv",sep = "")
# Data16 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename17,".csv",sep = "")
# Data17 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename18,".csv",sep = "")
# Data18 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename19,".csv",sep = "")
# Data19 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename20,".csv",sep = "")
# Data20 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename21,".csv",sep = "")
# Data21 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename22,".csv",sep = "")
# Data22 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename23,".csv",sep = "")
# Data23 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename24,".csv",sep = "")
# Data24 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Inputdir <- paste(path,"/",filename25,".csv",sep = "")
# Data25 <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
# 
# Data <- rbind(Data1,Data2,Data3,Data4,Data5,Data6,Data7,Data8,Data9,Data10,Data11,Data12,Data13,Data14,Data15,
#               Data16,Data17,Data18,Data19,Data20,Data21,Data22,Data23,Data24,Data25)
# 
# 
# write.csv(Data,file = "play_allsub_mouseclick.csv",row.names=T)
# 
####### Making plots #######
filename <- "play_click"
path <- getwd()
Inputdir <- paste(path,"/",filename,".csv",sep = "")
Data <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")

Data$cond <- replace(Data$cond, Data$cond=='inst', "Instrument-biased")
Data$cond <- replace(Data$cond, Data$cond=='mod', "Modifier-biased")
Data$cond <- replace(Data$cond, Data$cond=='equi', "Equi-biased")

library("ggplot2")
library("dplyr")
library("Rmisc")
library("wesanderson")
library("tidyverse")

# Summarize data for plotting
#df0 <- summarySE(Data, measurevar="value", groupvars=c("cond"))

Data <- subset(Data, click=="ti") 
positions <- c("Instrument-biased", "Equi-biased", "Modifier-biased")
cols <- c("Equi-biased" = "#999999", "Modifier-biased" = "#E69F00", "Instrument-biased" = "#56B4E9")

plot1 <- ggplot(Data, aes(cond, mean)) +
  geom_bar(aes(), data = Data, stat = "identity",position = position_dodge(1), width = 0.9, fill=cols)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), data = Data, width = 0.1, position = position_dodge(1))+
  ylab("Proportion of trials with first click on the instrument (%)")+
  xlab(" ")+
  scale_x_discrete(limits = positions)+
  ylim(0, 1)+
  geom_signif(comparisons = list(c("Equi-biased", "Modifier-biased")), 
              annotations = "*", y_position = 0.65)+
  geom_signif(y_position = 0.9, comparisons = list(c("Instrument-biased", "Modifier-biased")),
              annotation = "*")+
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 0.4),
    axis.line.y = element_line(colour = "black", size = 0.4))

  
