#### Eye movement data analysis
#### Yi-Lun Weng, 2019.07.16

rm(list = ls())
library(tidyverse)

## load files 
filename <- "play_027_ev"
path <- getwd()
Inputdir <- paste(path,"/",filename,".txt",sep = "")
data <- read.table(Inputdir,header = TRUE,sep = "", stringsAsFactors = FALSE)


## Delete random event codes
subdata <- subset(data, ecode==10|ecode==110|ecode==111|ecode==112|ecode==20|
                    ecode==120|ecode==121|ecode==122|ecode==30|ecode==130|ecode==131|ecode==132)


## Save the file and check the event codes manually
write.csv(subdata,file = "play_a_027_ev.csv",row.names=T)


####################### After clean the event codes #######################

rm(list = ls())
library(dplyr)
library(tidyverse)

## load files 
filename <- "play_a_026_mouseclick"
path <- getwd()
Inputdir <- paste(path,"/",filename,".csv",sep = "")
mclick <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
duptimes <- c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)
click <- data.frame(mclick$click_loc)

idx <- rep(1:nrow(click), duptimes)
dupclick <- click[idx, ]
dupclick <- data.frame(dupclick)
write.csv(dupclick, file = "play_a_026_dupclick.csv",row.names=T)



####################### Change the EEG event code #######################
## change event codes
## load files 
rm(list = ls())
library(dplyr)
library(tidyverse)

filename <- "play_a_027_ev"
path <- getwd()
Inputdir <- paste(path,"/",filename,".csv",sep = "")
data <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
datalength <- length(data$item)


for (i in 1:datalength) {
  if (data$ecode[i] == 10 && data$click[i] =="ta") {
    data$newcode[i] = 10
  } else if (data$ecode[i] == 110 && data$click[i] =="ta") {
    data$newcode[i] = 50
  } else if (data$ecode[i] == 111 && data$click[i] =="ta") {
    data$newcode[i] = 110
  } else if (data$ecode[i] == 112 && data$click[i] =="ta") {
    data$newcode[i] = 150
  } else if (data$ecode[i] == 20 && data$click[i] =="ta") {
    data$newcode[i] = 20
  } else if (data$ecode[i] == 120 && data$click[i] =="ta") {
    data$newcode[i] = 60
  } else if (data$ecode[i] == 121 && data$click[i] =="ta") {
    data$newcode[i] = 120
  } else if (data$ecode[i] == 122 && data$click[i] =="ta") {
    data$newcode[i] = 160
  } else if (data$ecode[i] == 30 && data$click[i] =="ta") {
    data$newcode[i] = 30
  } else if (data$ecode[i] == 130 && data$click[i] =="ta") {
    data$newcode[i] = 70
  } else if (data$ecode[i] == 131 && data$click[i] =="ta") {
    data$newcode[i] = 130
  } else if (data$ecode[i] == 132 && data$click[i] =="ta") {
    data$newcode[i] = 170
  } else if (data$ecode[i] == 10 && data$click[i] =="ti") {
    data$newcode[i] = 11
  } else if (data$ecode[i] == 110 && data$click[i] =="ti") {
    data$newcode[i] = 51
  } else if (data$ecode[i] == 111 && data$click[i] =="ti") {
    data$newcode[i] = 111
  } else if (data$ecode[i] == 112 && data$click[i] =="ti") {
    data$newcode[i] = 151
  } else if (data$ecode[i] == 20 && data$click[i] =="ti") {
    data$newcode[i] = 21
  } else if (data$ecode[i] == 120 && data$click[i] =="ti") {
    data$newcode[i] = 61
  } else if (data$ecode[i] == 121 && data$click[i] =="ti") {
    data$newcode[i] = 121
  } else if (data$ecode[i] == 122 && data$click[i] =="ti") {
    data$newcode[i] = 161
  } else if (data$ecode[i] == 30 && data$click[i] =="ti") {
    data$newcode[i] = 31
  } else if (data$ecode[i] == 130 && data$click[i] =="ti") {
    data$newcode[i] = 71
  } else if (data$ecode[i] == 131 && data$click[i] =="ti") {
    data$newcode[i] = 131
  } else if (data$ecode[i] == 132 && data$click[i] =="ti") {
    data$newcode[i] = 171
  } else if (data$ecode[i] == 10 && data$click[i] =="da") {
    data$newcode[i] = 12
  } else if (data$ecode[i] == 110 && data$click[i] =="da") {
    data$newcode[i] = 52
  } else if (data$ecode[i] == 111 && data$click[i] =="da") {
    data$newcode[i] = 112
  } else if (data$ecode[i] == 112 && data$click[i] =="da") {
    data$newcode[i] = 152
  } else if (data$ecode[i] == 20 && data$click[i] =="da") {
    data$newcode[i] = 22
  } else if (data$ecode[i] == 120 && data$click[i] =="da") {
    data$newcode[i] = 62
  } else if (data$ecode[i] == 121 && data$click[i] =="da") {
    data$newcode[i] = 122
  } else if (data$ecode[i] == 122 && data$click[i] =="da") {
    data$newcode[i] = 162
  } else if (data$ecode[i] == 30 && data$click[i] =="da") {
    data$newcode[i] = 32
  } else if (data$ecode[i] == 130 && data$click[i] =="da") {
    data$newcode[i] = 72
  } else if (data$ecode[i] == 131 && data$click[i] =="da") {
    data$newcode[i] = 132
  } else if (data$ecode[i] == 132 && data$click[i] =="da") {
    data$newcode[i] = 172
  } else if (data$ecode[i] == 10 && data$click[i] =="di") {
    data$newcode[i] = 13
  } else if (data$ecode[i] == 110 && data$click[i] =="di") {
    data$newcode[i] = 53
  } else if (data$ecode[i] == 111 && data$click[i] =="di") {
    data$newcode[i] = 113
  } else if (data$ecode[i] == 112 && data$click[i] =="di") {
    data$newcode[i] = 153
  } else if (data$ecode[i] == 20 && data$click[i] =="di") {
    data$newcode[i] = 23
  } else if (data$ecode[i] == 120 && data$click[i] =="di") {
    data$newcode[i] = 63
  } else if (data$ecode[i] == 121 && data$click[i] =="di") {
    data$newcode[i] = 123
  } else if (data$ecode[i] == 122 && data$click[i] =="di") {
    data$newcode[i] = 163
  } else if (data$ecode[i] == 30 && data$click[i] =="di") {
    data$newcode[i] = 33
  } else if (data$ecode[i] == 130 && data$click[i] =="di") {
    data$newcode[i] = 73
  } else if (data$ecode[i] == 131 && data$click[i] =="di") {
    data$newcode[i] = 133
  } else if (data$ecode[i] == 132 && data$click[i] =="di") {
    data$newcode[i] = 173
  } else {
  }
}

write.csv(data, file = "play_a_027_ev.csv",row.names=T)



####################### Add event code for the first fixation #######################
## change event codes
## load files 
rm(list = ls())
library(dplyr)
library(tidyverse)

filename <- "play_a_013_firstfix"
path <- getwd()
Inputdir <- paste(path,"/",filename,".csv",sep = "")
data <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
datalength <- length(data$TrialID)



for (i in 1:datalength) {
  if (data$Condition[i] == "mod" && data$click_loc[i] =="ta") {
    data$addcode[i] = 208
  } else if (data$Condition[i] == "Equi" && data$click_loc[i] =="ta") {
    data$addcode[i] = 200
  } else if (data$Condition[i] == "Inst" && data$click_loc[i] =="ta") {
    data$addcode[i] = 216
  } else if (data$Condition[i] == "mod" && data$click_loc[i] =="ti") {
    data$addcode[i] = 209
  } else if (data$Condition[i] == "Equi" && data$click_loc[i] =="ti") {
    data$addcode[i] = 201
  } else if (data$Condition[i] == "Inst" && data$click_loc[i] =="ti") {
    data$addcode[i] = 217
  } else if (data$Condition[i] == "mod" && data$click_loc[i] =="da") {
    data$addcode[i] = 210
  } else if (data$Condition[i] == "Equi" && data$click_loc[i] =="da") {
    data$addcode[i] = 202
  } else if (data$Condition[i] == "Inst" && data$click_loc[i] =="da") {
    data$addcode[i] = 218
  } else if (data$Condition[i] == "mod" && data$click_loc[i] =="di") {
    data$addcode[i] = 211
  } else if (data$Condition[i] == "Equi" && data$click_loc[i] =="di") {
    data$addcode[i] = 203
  } else if (data$Condition[i] == "Inst" && data$click_loc[i] =="di") {
    data$addcode[i] = 219
  } else {
  }
}

write.csv(data, file = "play_a_012_firstfix.csv",row.names=T)




# ######################################
# 
# ## Change sec to msec 
# onset$animal = onset$animal*1000
# onset$verb = onset$verb*1000
# onset$with = onset$with*1000
# onset$object = onset$object*1000
# 
# ## Create new columns
# data$timelen<-NA
# data$timeacc<-NA
# data$timewindow<-NA
# data$eyeloc<-NA
# data$pre_eyeloc<-NA
# data$ta<-NA
# data$ti<-NA
# data$da<-NA
# data$di<-NA
# data$fix_pro_timewindow<-NA
# data$eventcode <- NA
# 
# 
# names(data)[1] <-"Recording_timestamp"
# 
# ## Delete filter and practice trial
# Data <- subset(data, Condition!="filler" & Condition!="Practice" & AOI!= "0")
# 
# 
# ## Find the location of eye fixation
# Data <- Data %>%
#   mutate(eyeloc = ifelse(Tiloc == AOI, "tiloc", ifelse(
#     Tloc == AOI, "tloc", ifelse(Cloc == AOI, "cloc", "ciloc")
#   ))) %>%
#   as.data.frame()
# 
# ## change characters into numbers
# ## tloc=20 ; tiloc=21 ; cloc=22 ; ciloc=23
# Data$eyeloc <- replace(Data$eyeloc, Data$eyeloc=='tloc', 20)
# Data$eyeloc <- replace(Data$eyeloc, Data$eyeloc=='tiloc', 21)
# Data$eyeloc <- replace(Data$eyeloc, Data$eyeloc=='cloc', 22)
# Data$eyeloc <- replace(Data$eyeloc, Data$eyeloc=='ciloc', 23)
# 
# 
# 
# ##### t1 setting #####
# t1 <- Data[which(Data$TrialID==1),]
# lengt <- length(t1$Condition)
# 
# ### time length
# for (j in 1:lengt) {
#   t1$timelen[j] = t1$Recording_timestamp[j+1] - t1$Recording_timestamp[j]
# }
# t1$timelen = t1$timelen/1000
# t1$timelen[max(j)] <- t1$timelen[max(j-1)]
# 
# ### time accumulation
# t1$timeacc[1] <- 0
# t1$timeacc[2] = t1$timelen[1] + t1$timelen[2]
# for (i in 1:lengt) {
#   t1$timeacc[i+2] = t1$timelen[i+2] + t1$timeacc[i+1]
# }
# 
# #t1$timeacc[lengt] = t1$timelen[lengt] + t1$timeacc[lengt+1]
# 
# ### time window
# for (i in 1:lengt) {
#   if (t1$timeacc[i] < onset$animal[1]) {
#     t1$timewindow[i] = 1
#   } else if (t1$timeacc[i] > onset$animal[1] && t1$timeacc[i] < onset$object[1]) {
#     t1$timewindow[i] = 2
#   } else if (t1$timeacc[i] > onset$object[1]) {
#     t1$timewindow[i] = 3
#   } else {
#   }
# }
# 
# 
# ## for "ta" column
# ## tloc=20 ; tiloc=21 ; cloc=22 ; ciloc=23
# for (i in 1:lengt) {
#   if (t1$eyeloc[i] == 20) {
#     t1$ta[i] = 1
#   } else if (t1$eyeloc[i] == 21) {
#     t1$ta[i] = 0
#   } else if (t1$eyeloc[i] == 22) {
#     t1$ta[i] = 0
#   } else if (t1$eyeloc[i] == 23) {
#     t1$ta[i] = 0
#   } else if (t1$eyeloc[i] == "na") {
#     t1$ta[i] = 0
#   } else {
#   }
# }
# 
# 
# ## for "ti" column
# ## tloc=20 ; tiloc=21 ; cloc=22 ; ciloc=23
# for (i in 1:lengt) {
#   if (t1$eyeloc[i] == 20) {
#     t1$ti[i] = 0
#   } else if (t1$eyeloc[i] == 21) {
#     t1$ti[i] = 1
#   } else if (t1$eyeloc[i] == 22) {
#     t1$ti[i] = 0
#   } else if (t1$eyeloc[i] == 23) {
#     t1$ti[i] = 0
#   } else if (t1$eyeloc[i] == "na") {
#     t1$ti[i] = 0
#   } else {
#   }
# }
# 
# 
# 
# ## for "da" column
# ## tloc=20 ; tiloc=21 ; cloc=22 ; ciloc=23
# for (i in 1:lengt) {
#   if (t1$eyeloc[i] == 20) {
#     t1$da[i] = 0
#   } else if (t1$eyeloc[i] == 21) {
#     t1$da[i] = 0
#   } else if (t1$eyeloc[i] == 22) {
#     t1$da[i] = 1
#   } else if (t1$eyeloc[i] == 23) {
#     t1$da[i] = 0
#   } else if (t1$eyeloc[i] == "na") {
#     t1$da[i] = 0
#   } else {
#   }
# }
# 
# ## for "di" column
# ## tloc=20 ; tiloc=21 ; cloc=22 ; ciloc=23
# for (i in 1:lengt) {
#   if (t1$eyeloc[i] == 20) {
#     t1$di[i] = 0
#   } else if (t1$eyeloc[i] == 21) {
#     t1$di[i] = 0
#   } else if (t1$eyeloc[i] == 22) {
#     t1$di[i] = 0
#   } else if (t1$eyeloc[i] == 23) {
#     t1$di[i] = 1
#   } else if (t1$eyeloc[i] == "na") {
#     t1$di[i] = 0
#   } else {
#   }
# }
# 
# ## previous fixation
# #t1$pre_eyeloc[2] <- t1$eyeloc[1]
# for (i in 1:lengt) {
#   t1$pre_eyeloc[i+1] = t1$eyeloc[i]
# }
# t1$pre_eyeloc[1] <- 0
# 
# 
# ## calculate proportion of fixations across three conditions
# for (i in 1:lengt) {
#   if (t1$timeacc[i] < 50) {
#     t1$fix_pro_timewindow[i] = 1
#   } else if (t1$timeacc[i] > 50 && t1$timeacc[i] < 50*2) {
#     t1$fix_pro_timewindow[i] = 2
#   } else if (t1$timeacc[i] > 50*2 && t1$timeacc[i] < 50*3) {
#     t1$fix_pro_timewindow[i] = 3
#   } else if (t1$timeacc[i] > 50*3 && t1$timeacc[i] < 50*4) {
#     t1$fix_pro_timewindow[i] = 4
#   } else if (t1$timeacc[i] > 50*4 && t1$timeacc[i] < 50*5) {
#     t1$fix_pro_timewindow[i] = 5
#   } else if (t1$timeacc[i] > 50*5 && t1$timeacc[i] < 50*6) {
#     t1$fix_pro_timewindow[i] = 6
#   } else if (t1$timeacc[i] > 50*6 && t1$timeacc[i] < 50*7) {
#     t1$fix_pro_timewindow[i] = 7
#   } else if (t1$timeacc[i] > 50*7 && t1$timeacc[i] < 50*8) {
#     t1$fix_pro_timewindow[i] = 8
#   } else if (t1$timeacc[i] > 50*8 && t1$timeacc[i] < 50*9) {
#     t1$fix_pro_timewindow[i] = 9
#   } else if (t1$timeacc[i] > 50*9 && t1$timeacc[i] < 50*10) {
#     t1$fix_pro_timewindow[i] = 10
#   } else if (t1$timeacc[i] > 50*10 && t1$timeacc[i] < 50*11) {
#     t1$fix_pro_timewindow[i] = 11
#   } else if (t1$timeacc[i] > 50*11 && t1$timeacc[i] < 50*12) {
#     t1$fix_pro_timewindow[i] = 12
#   } else if (t1$timeacc[i] > 50*12 && t1$timeacc[i] < 50*13) {
#     t1$fix_pro_timewindow[i] = 13
#   } else if (t1$timeacc[i] > 50*13 && t1$timeacc[i] < 50*14) {
#     t1$fix_pro_timewindow[i] = 14
#   } else if (t1$timeacc[i] > 50*14 && t1$timeacc[i] < 50*15) {
#     t1$fix_pro_timewindow[i] = 15
#   } else if (t1$timeacc[i] > 50*15 && t1$timeacc[i] < 50*16) {
#     t1$fix_pro_timewindow[i] = 16
#   } else if (t1$timeacc[i] > 50*16 && t1$timeacc[i] < 50*17) {
#     t1$fix_pro_timewindow[i] = 17
#   } else if (t1$timeacc[i] > 50*17 && t1$timeacc[i] < 50*18) {
#     t1$fix_pro_timewindow[i] = 18
#   } else if (t1$timeacc[i] > 50*18 && t1$timeacc[i] < 50*19) {
#     t1$fix_pro_timewindow[i] = 19
#   } else if (t1$timeacc[i] > 50*19 && t1$timeacc[i] < 50*20) {
#     t1$fix_pro_timewindow[i] = 20
#   } else if (t1$timeacc[i] > 50*20 && t1$timeacc[i] < 50*21) {
#     t1$fix_pro_timewindow[i] = 21
#   } else if (t1$timeacc[i] > 50*21 && t1$timeacc[i] < 50*22) {
#     t1$fix_pro_timewindow[i] = 22
#   } else if (t1$timeacc[i] > 50*22 && t1$timeacc[i] < 50*23) {
#     t1$fix_pro_timewindow[i] = 23
#   } else if (t1$timeacc[i] > 50*23 && t1$timeacc[i] < 50*24) {
#     t1$fix_pro_timewindow[i] = 24
#   } else if (t1$timeacc[i] > 50*24 && t1$timeacc[i] < 50*25) {
#     t1$fix_pro_timewindow[i] = 25
#   } else if (t1$timeacc[i] > 50*25 && t1$timeacc[i] < 50*26) {
#     t1$fix_pro_timewindow[i] = 26
#   } else if (t1$timeacc[i] > 50*26 && t1$timeacc[i] < 50*27) {
#     t1$fix_pro_timewindow[i] = 27
#   } else if (t1$timeacc[i] > 50*27 && t1$timeacc[i] < 50*28) {
#     t1$fix_pro_timewindow[i] = 28
#   } else if (t1$timeacc[i] > 50*28 && t1$timeacc[i] < 50*29) {
#     t1$fix_pro_timewindow[i] = 29
#   } else if (t1$timeacc[i] > 50*29 && t1$timeacc[i] < 50*30) {
#     t1$fix_pro_timewindow[i] = 30
#   } else if (t1$timeacc[i] > 50*30 && t1$timeacc[i] < 50*31) {
#     t1$fix_pro_timewindow[i] = 31
#   } else if (t1$timeacc[i] > 50*31 && t1$timeacc[i] < 50*32) {
#     t1$fix_pro_timewindow[i] = 32
#   } else if (t1$timeacc[i] > 50*32 && t1$timeacc[i] < 50*33) {
#     t1$fix_pro_timewindow[i] = 33
#   } else if (t1$timeacc[i] > 50*33 && t1$timeacc[i] < 50*34) {
#     t1$fix_pro_timewindow[i] = 34
#   } else if (t1$timeacc[i] > 50*34 && t1$timeacc[i] < 50*35) {
#     t1$fix_pro_timewindow[i] = 35
#   } else if (t1$timeacc[i] > 50*35 && t1$timeacc[i] < 50*36) {
#     t1$fix_pro_timewindow[i] = 36
#   } else if (t1$timeacc[i] > 50*36 && t1$timeacc[i] < 50*37) {
#     t1$fix_pro_timewindow[i] = 37
#   } else if (t1$timeacc[i] > 50*37 && t1$timeacc[i] < 50*38) {
#     t1$fix_pro_timewindow[i] = 38
#   } else if (t1$timeacc[i] > 50*38 && t1$timeacc[i] < 50*39) {
#     t1$fix_pro_timewindow[i] = 39
#   } else if (t1$timeacc[i] > 50*39 && t1$timeacc[i] < 50*40) {
#     t1$fix_pro_timewindow[i] = 40
#   } else if (t1$timeacc[i] > 50*40 && t1$timeacc[i] < 50*41) {
#     t1$fix_pro_timewindow[i] = 41
#   } else if (t1$timeacc[i] > 50*41 && t1$timeacc[i] < 50*42) {
#     t1$fix_pro_timewindow[i] = 42
#   } else if (t1$timeacc[i] > 50*42 && t1$timeacc[i] < 50*43) {
#     t1$fix_pro_timewindow[i] = 43
#   } else if (t1$timeacc[i] > 50*43 && t1$timeacc[i] < 50*44) {
#     t1$fix_pro_timewindow[i] = 44
#   } else if (t1$timeacc[i] > 50*44 && t1$timeacc[i] < 50*45) {
#     t1$fix_pro_timewindow[i] = 45
#   } else if (t1$timeacc[i] > 50*45 && t1$timeacc[i] < 50*46) {
#     t1$fix_pro_timewindow[i] = 46
#   } else if (t1$timeacc[i] > 50*46 && t1$timeacc[i] < 50*47) {
#     t1$fix_pro_timewindow[i] = 47
#   } else if (t1$timeacc[i] > 50*47 && t1$timeacc[i] < 50*48) {
#     t1$fix_pro_timewindow[i] = 48
#   } else if (t1$timeacc[i] > 50*48 && t1$timeacc[i] < 50*49) {
#     t1$fix_pro_timewindow[i] = 49
#   } else if (t1$timeacc[i] > 50*49 && t1$timeacc[i] < 50*50) {
#     t1$fix_pro_timewindow[i] = 50
#   } else if (t1$timeacc[i] > 50*50 && t1$timeacc[i] < 50*51) {
#     t1$fix_pro_timewindow[i] = 51
#   } else if (t1$timeacc[i] > 50*51 && t1$timeacc[i] < 50*52) {
#     t1$fix_pro_timewindow[i] = 52
#   } else if (t1$timeacc[i] > 50*52 && t1$timeacc[i] < 50*53) {
#     t1$fix_pro_timewindow[i] = 53
#   } else if (t1$timeacc[i] > 50*53 && t1$timeacc[i] < 50*54) {
#     t1$fix_pro_timewindow[i] = 54
#   } else if (t1$timeacc[i] > 50*54 && t1$timeacc[i] < 50*55) {
#     t1$fix_pro_timewindow[i] = 55
#   } else if (t1$timeacc[i] > 50*55 && t1$timeacc[i] < 50*56) {
#     t1$fix_pro_timewindow[i] = 56
#   } else if (t1$timeacc[i] > 50*56 && t1$timeacc[i] < 50*57) {
#     t1$fix_pro_timewindow[i] = 57
#   } else if (t1$timeacc[i] > 50*57 && t1$timeacc[i] < 50*58) {
#     t1$fix_pro_timewindow[i] = 58
#   } else if (t1$timeacc[i] > 50*58 && t1$timeacc[i] < 50*59) {
#     t1$fix_pro_timewindow[i] = 59
#   } else if (t1$timeacc[i] > 50*59 && t1$timeacc[i] < 50*60) {
#     t1$fix_pro_timewindow[i] = 60
#   } else if (t1$timeacc[i] > 50*60 && t1$timeacc[i] < 50*61) {
#     t1$fix_pro_timewindow[i] = 61
#   } else if (t1$timeacc[i] > 50*61 && t1$timeacc[i] < 50*62) {
#     t1$fix_pro_timewindow[i] = 62
#   } else if (t1$timeacc[i] > 50*62 && t1$timeacc[i] < 50*63) {
#     t1$fix_pro_timewindow[i] = 63
#   } else if (t1$timeacc[i] > 50*63 && t1$timeacc[i] < 50*64) {
#     t1$fix_pro_timewindow[i] = 64
#   } else if (t1$timeacc[i] > 50*64 && t1$timeacc[i] < 50*65) {
#     t1$fix_pro_timewindow[i] = 65
#   } else if (t1$timeacc[i] > 50*65 && t1$timeacc[i] < 50*66) {
#     t1$fix_pro_timewindow[i] = 66
#   } else if (t1$timeacc[i] > 50*66 && t1$timeacc[i] < 50*67) {
#     t1$fix_pro_timewindow[i] = 67
#   } else if (t1$timeacc[i] > 50*67 && t1$timeacc[i] < 50*68) {
#     t1$fix_pro_timewindow[i] = 68
#   } else if (t1$timeacc[i] > 50*68 && t1$timeacc[i] < 50*69) {
#     t1$fix_pro_timewindow[i] = 69
#   } else if (t1$timeacc[i] > 50*69 && t1$timeacc[i] < 50*70) {
#     t1$fix_pro_timewindow[i] = 70
#   } else if (t1$timeacc[i] > 50*70 && t1$timeacc[i] < 50*71) {
#     t1$fix_pro_timewindow[i] = 71
#   } else if (t1$timeacc[i] > 50*71 && t1$timeacc[i] < 50*72) {
#     t1$fix_pro_timewindow[i] = 72
#   } else if (t1$timeacc[i] > 50*72 && t1$timeacc[i] < 50*73) {
#     t1$fix_pro_timewindow[i] = 73
#   } else if (t1$timeacc[i] > 50*73 && t1$timeacc[i] < 50*74) {
#     t1$fix_pro_timewindow[i] = 74
#   } else if (t1$timeacc[i] > 50*74 && t1$timeacc[i] < 50*75) {
#     t1$fix_pro_timewindow[i] = 75
#   } else if (t1$timeacc[i] > 50*75 && t1$timeacc[i] < 50*76) {
#     t1$fix_pro_timewindow[i] = 76
#   } else if (t1$timeacc[i] > 50*76 && t1$timeacc[i] < 50*77) {
#     t1$fix_pro_timewindow[i] = 77
#   } else if (t1$timeacc[i] > 50*77 && t1$timeacc[i] < 50*78) {
#     t1$fix_pro_timewindow[i] = 78
#   } else if (t1$timeacc[i] > 50*78 && t1$timeacc[i] < 50*79) {
#     t1$fix_pro_timewindow[i] = 79
#   } else if (t1$timeacc[i] > 50*79 && t1$timeacc[i] < 50*80) {
#     t1$fix_pro_timewindow[i] = 80
#   } else if (t1$timeacc[i] > 50*80) {
#     t1$fix_pro_timewindow[i] = 81
#   } else {
#   }
# }
# 
# ## Find the eventcode
# t1 <- t1 %>%
#   mutate( eventcode = ifelse(eyeloc != lag(eyeloc), row_number(), NA)
#   ) %>%
#   as.data.frame()
# 
# t1$eventcode[1] = 1
# t1_code <- t1 %>% drop_na(eventcode)
# t1_code <- t1_code %>%
#   filter(lead(eventcode) - eventcode >=15)
# 
# 
# 
# 
# 
