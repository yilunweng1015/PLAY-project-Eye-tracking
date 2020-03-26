rm(list = ls())
library(lme4)
library(lmerTest)
library(ez)
library(plyr)
library(dplyr)

filename <- "play_eyefixation"
path <- getwd()
Inputdir <- paste(path,"/",filename,".csv",sep = "")
data <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = TRUE, na.strings = "NA")

## separate into 3 time windows
timewind1 <- subset(data, timewindow == 1)
timewind2 <- subset(data, timewindow == 2)
timewind3 <- subset(data, timewindow == 3)

## Timewindow 1 analysis
my.helmert = matrix(c(1/3, -2/3, 1/3, -1/2, 0, 1/2), ncol = 2)
contrasts(timewind1$cond) = my.helmert
# Target instrument region
t1_ti_result <- glmer(tilooks ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = timewind1)
summary(t1_ti_result)
# Target aninal region
t1_ta_result <- glmer(talooks ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = timewind1)
summary(t1_ta_result)


## Timewindow 2 analysis
my.helmert = matrix(c(1/3, -2/3, 1/3, -1/2, 0, 1/2), ncol = 2)
contrasts(timewind2$cond) = my.helmert
# Target instrument region
t2_ti_result <- glmer(tilooks ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = timewind2)
summary(t2_ti_result)
# Target aninal region
t2_ta_result <- glmer(talooks ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = timewind2)
summary(t2_ta_result)


## Timewindow 3 analysis
my.helmert = matrix(c(1/3, -2/3, 1/3, -1/2, 0, 1/2), ncol = 2)
contrasts(timewind3$cond) = my.helmert
# Target instrument region
t3_ti_result <- glmer(tilooks ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = timewind3)
summary(t3_ti_result)
# Target aninal region
t3_ta_result <- glmer(talooks ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = timewind3)
summary(t3_ta_result)
