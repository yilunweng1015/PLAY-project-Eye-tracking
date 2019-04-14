rm(list = ls())
library(ez)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)

filename1 <- "play_et_a_001"
filename2 <- "onsets"
path <- getwd()

##### load files #####
Inputdir <- paste(path,"/",filename1,".txt",sep = "")
Data <- read.table(Inputdir, header = TRUE,sep = "", fill = TRUE, stringsAsFactors = FALSE)

Inputdir <- paste(path,"/",filename2,".txt",sep = "")
onset <- read.table(Inputdir,header = TRUE,sep = "", stringsAsFactors = FALSE)

## Transform time to millisecond 
Data[,1] = Data[,1]/1000000
Data[,2] = NULL

## Create new columns
Data$timelen<-NA
Data$timeacc<-NA
Data$timewindow<-NA
Data$AOI_object<-NA
Data$ta<-NA
Data$ti<-NA
Data$da<-NA
Data$di<-NA

## Delete fillers and practice trials
subdata <- subset(Data, Condition!="filler" & Condition!="Practice")


##### calculate each trial#####
### t1
t1 <- Data[which(Data$TrialID==1),]
lengt <- length(t1$Condition)

### time length
for (j in 1:lengt) {
  t1$timelen[j] = t1$Recording_timestamp[j+1] - t1$Recording_timestamp[j]
 }


### time accumulation
t1$timeacc[1] <- 0
t1$timeacc[2] = t1$timelen[1] + t1$timelen[2]
for (i in 1:lengt) {
  t1$timeacc[i+2] = t1$timelen[i+2] + t1$timeacc[i+1]
}

t1$timeacc[lengt] = t1$timelen[lengt] + t1$timeacc[lengt+1]


### time window
for (i in 1:lengt) {
if (t1$timeacc[i] < onset$animal[1]) {
      t1$timewindow[i] = 1
    } else if (t1$timeacc[i] > onset$animal[1] && t1$timeacc[i] < onset$object[1]) {
      t1$timewindow[i] = 2
    } else if (t1$timeacc[i] > onset$object[1]) {
      t1$timewindow[i] = 3
    } else {
    }
}

## Find the location info of objects for each condition
# ta <- t1[t1$Tloc[1],20]
# ti <- t1[t1$Tiloc[1],21]
# da <- t1[t1$Cloc[1],22]
# di <- t1[t1$Ciloc[1],23]

## Find the location info of objects for each condition
## LR=20 ; TL=21 ; LL=22 ; TR=23
t1$Tloc <- replace(t1$Tloc, t1$Tloc=='LR', 20)
t1$Tloc <- replace(t1$Tloc, t1$Tloc=='TL', 21)
t1$Tloc <- replace(t1$Tloc, t1$Tloc=='LL', 22)
t1$Tloc <- replace(t1$Tloc, t1$Tloc=='TR', 23)

t1$Tiloc <- replace(t1$Tiloc, t1$Tiloc=='LR', 20)
t1$Tiloc <- replace(t1$Tiloc, t1$Tiloc=='TL', 21)
t1$Tiloc <- replace(t1$Tiloc, t1$Tiloc=='LL', 22)
t1$Tiloc <- replace(t1$Tiloc, t1$Tiloc=='TR', 23)

t1$Cloc <- replace(t1$Cloc, t1$Cloc=='LR', 20)
t1$Cloc <- replace(t1$Cloc, t1$Cloc=='TL', 21)
t1$Cloc <- replace(t1$Cloc, t1$Cloc=='LL', 22)
t1$Cloc <- replace(t1$Cloc, t1$Cloc=='TR', 23)

t1$Ciloc <- replace(t1$Ciloc, t1$Ciloc=='LR', 20)
t1$Ciloc <- replace(t1$Ciloc, t1$Ciloc=='TL', 21)
t1$Ciloc <- replace(t1$Ciloc, t1$Ciloc=='LL', 22)
t1$Ciloc <- replace(t1$Ciloc, t1$Ciloc=='TR', 23)

t1$AOI[is.na(t1$AOI)] <- 0

t1$AOI <- replace(t1$AOI, t1$AOI=='LR', 20)
t1$AOI <- replace(t1$AOI, t1$AOI=='TL', 21)
t1$AOI <- replace(t1$AOI, t1$AOI=='LL', 22)
t1$AOI <- replace(t1$AOI, t1$AOI=='TR', 23)


ta1 <- t1$Tloc[1]
ti1 <- t1$Tiloc[1]
da1 <- t1$Cloc[1]
di1 <- t1$Ciloc[1]


for (i in 1:lengt) {
  if (t1$AOI[i] == ta1) {
    t1$AOI_object[i] = "ta"
  } else if (t1$AOI[i] == ti1) {
    t1$AOI_object[i] = "ti"
  } else if (t1$AOI[i] == da1) {
    t1$AOI_object[i] = "da"
  } else if (t1$AOI[i] == di1) {
    t1$AOI_object[i] = "di"
  } else if (t1$AOI[i] == 0) {
    t1$AOI_object[i] = "na"
  } else {
  }
}

 
## for "ta" column
for (i in 1:lengt) {
  if (t1$AOI_object[i] == "ta") {
    t1$ta[i] = 1
  } else if (t1$AOI_object[i] == "ti") {
    t1$ta[i] = 0
  } else if (t1$AOI_object[i] == "da") {
    t1$ta[i] = 0
  } else if (t1$AOI_object[i] == "di") {
    t1$ta[i] = 0
  } else if (t1$AOI_object[i] == "na") {
    t1$ta[i] = 0
  } else {
  }
}


## for "ti" column
for (i in 1:lengt) {
  if (t1$AOI_object[i] == "ta") {
    t1$ti[i] = 0
  } else if (t1$AOI_object[i] == "ti") {
    t1$ti[i] = 1
  } else if (t1$AOI_object[i] == "da") {
    t1$ti[i] = 0
  } else if (t1$AOI_object[i] == "di") {
    t1$ti[i] = 0
  } else if (t1$AOI_object[i] == "na") {
    t1$ti[i] = 0
  } else {
  }
}


## for "da" column
for (i in 1:lengt) {
  if (t1$AOI_object[i] == "ta") {
    t1$da[i] = 0
  } else if (t1$AOI_object[i] == "ti") {
    t1$da[i] = 0
  } else if (t1$AOI_object[i] == "da") {
    t1$da[i] = 1
  } else if (t1$AOI_object[i] == "di") {
    t1$da[i] = 0
  } else if (t1$AOI_object[i] == "na") {
    t1$da[i] = 0
  } else {
  }
}


## for "di" column
for (i in 1:lengt) {
  if (t1$AOI_object[i] == "ta") {
    t1$di[i] = 0
  } else if (t1$AOI_object[i] == "ti") {
    t1$di[i] = 0
  } else if (t1$AOI_object[i] == "da") {
    t1$di[i] = 0
  } else if (t1$AOI_object[i] == "di") {
    t1$di[i] = 1
  } else if (t1$AOI_object[i] == "na") {
    t1$di[i] = 0
  } else {
  }
}


### t2
### time length
t2 <- Data[which(Data$TrialID==2),]
lengt <- length(t2$Condition)
for (j in 1:lengt) {
  t2$timelen[j] = t2$Recording_timestamp[j+1] - t2$Recording_timestamp[j]
}

### time accumulation
t2$timeacc[1] <- 0
t2$timeacc[2] = t2$timelen[1] + t2$timelen[2]
for (i in 1:lengt) {
  t2$timeacc[i+2] = t2$timelen[i+2] + t2$timeacc[i+1]
}

#t2$timeacc[686] = t2$timelen[686] + t2$timeacc[685]

### time window
for (i in 1:lengt) {
  if (t2$timeacc[i] < onset$animal[2]) {
    t2$timewindow[i] = 1
  } else if (t2$timeacc[i] > onset$animal[2] && t2$timeacc[i] < onset$object[2]) {
    t2$timewindow[i] = 2
  } else if (t2$timeacc[i] > onset$object[2]) {
    t2$timewindow[i] = 3
  } else {
  }
}


## Find the location info of objects for each condition
## LR=20 ; TL=21 ; LL=22 ; TR=23
t2$Tloc <- replace(t2$Tloc, t2$Tloc=='LR', 20)
t2$Tloc <- replace(t2$Tloc, t2$Tloc=='TL', 21)
t2$Tloc <- replace(t2$Tloc, t2$Tloc=='LL', 22)
t2$Tloc <- replace(t2$Tloc, t2$Tloc=='TR', 23)

t2$Tiloc <- replace(t2$Tiloc, t2$Tiloc=='LR', 20)
t2$Tiloc <- replace(t2$Tiloc, t2$Tiloc=='TL', 21)
t2$Tiloc <- replace(t2$Tiloc, t2$Tiloc=='LL', 22)
t2$Tiloc <- replace(t2$Tiloc, t2$Tiloc=='TR', 23)

t2$Cloc <- replace(t2$Cloc, t2$Cloc=='LR', 20)
t2$Cloc <- replace(t2$Cloc, t2$Cloc=='TL', 21)
t2$Cloc <- replace(t2$Cloc, t2$Cloc=='LL', 22)
t2$Cloc <- replace(t2$Cloc, t2$Cloc=='TR', 23)

t2$Ciloc <- replace(t2$Ciloc, t2$Ciloc=='LR', 20)
t2$Ciloc <- replace(t2$Ciloc, t2$Ciloc=='TL', 21)
t2$Ciloc <- replace(t2$Ciloc, t2$Ciloc=='LL', 22)
t2$Ciloc <- replace(t2$Ciloc, t2$Ciloc=='TR', 23)

t2$AOI[is.na(t2$AOI)] <- 0

t2$AOI <- replace(t2$AOI, t2$AOI=='LR', 20)
t2$AOI <- replace(t2$AOI, t2$AOI=='TL', 21)
t2$AOI <- replace(t2$AOI, t2$AOI=='LL', 22)
t2$AOI <- replace(t2$AOI, t2$AOI=='TR', 23)


ta1 <- t2$Tloc[1]
ti1 <- t2$Tiloc[1]
da1 <- t2$Cloc[1]
di1 <- t2$Ciloc[1]


for (i in 1:lengt) {
  if (t2$AOI[i] == ta1) {
    t2$AOI_object[i] = "ta"
  } else if (t2$AOI[i] == ti1) {
    t2$AOI_object[i] = "ti"
  } else if (t2$AOI[i] == da1) {
    t2$AOI_object[i] = "da"
  } else if (t2$AOI[i] == di1) {
    t2$AOI_object[i] = "di"
  } else if (t2$AOI[i] == 0) {
    t2$AOI_object[i] = "na"
  } else {
  }
}


## for "ta" column
for (i in 1:lengt) {
  if (t2$AOI_object[i] == "ta") {
    t2$ta[i] = 1
  } else if (t2$AOI_object[i] == "ti") {
    t2$ta[i] = 0
  } else if (t2$AOI_object[i] == "da") {
    t2$ta[i] = 0
  } else if (t2$AOI_object[i] == "di") {
    t2$ta[i] = 0
  } else if (t2$AOI_object[i] == "na") {
    t2$ta[i] = 0
  } else {
  }
}


## for "ti" column
for (i in 1:lengt) {
  if (t2$AOI_object[i] == "ta") {
    t2$ti[i] = 0
  } else if (t2$AOI_object[i] == "ti") {
    t2$ti[i] = 1
  } else if (t2$AOI_object[i] == "da") {
    t2$ti[i] = 0
  } else if (t2$AOI_object[i] == "di") {
    t2$ti[i] = 0
  } else if (t2$AOI_object[i] == "na") {
    t2$ti[i] = 0
  } else {
  }
}


## for "da" column
for (i in 1:lengt) {
  if (t2$AOI_object[i] == "ta") {
    t2$da[i] = 0
  } else if (t2$AOI_object[i] == "ti") {
    t2$da[i] = 0
  } else if (t2$AOI_object[i] == "da") {
    t2$da[i] = 1
  } else if (t2$AOI_object[i] == "di") {
    t2$da[i] = 0
  } else if (t2$AOI_object[i] == "na") {
    t2$da[i] = 0
  } else {
  }
}


## for "di" column
for (i in 1:lengt) {
  if (t2$AOI_object[i] == "ta") {
    t2$di[i] = 0
  } else if (t2$AOI_object[i] == "ti") {
    t2$di[i] = 0
  } else if (t2$AOI_object[i] == "da") {
    t2$di[i] = 0
  } else if (t2$AOI_object[i] == "di") {
    t2$di[i] = 1
  } else if (t2$AOI_object[i] == "na") {
    t2$di[i] = 0
  } else {
  }
}





### t3
### time length
t3 <- Data[which(Data$TrialID==3),]
lengt <- length(t3$Condition)
for (j in 1:lengt) {
  t3$timelen[j] = t3$Recording_timestamp[j+1] - t3$Recording_timestamp[j]
}

### time accumulation
t3$timeacc[1] <- 0
t3$timeacc[2] = t3$timelen[1] + t3$timelen[2]
for (i in 1:lengt) {
  t3$timeacc[i+2] = t3$timelen[i+2] + t3$timeacc[i+1]
}

t3$timeacc[1468] = t3$timelen[1468] + t3$timeacc[1469]

### time window
for (i in 1:lengt) {
  if (t3$timeacc[i] < onset$animal[3]) {
    t3$timewindow[i] = 1
  } else if (t3$timeacc[i] > onset$animal[3] && t3$timeacc[i] < onset$object[3]) {
    t3$timewindow[i] = 2
  } else if (t3$timeacc[i] > onset$object[3]) {
    t3$timewindow[i] = 3
  } else {
  }
}


### t4
### time length
t4 <- Data[which(Data$TrialID==4),]
lengt <- length(t4$Condition)
for (j in 1:lengt) {
  t4$timelen[j] = t4$Recording_timestamp[j+1] - t4$Recording_timestamp[j]
}

### time accumulation
t4$timeacc[1] <- 0
t4$timeacc[2] = t4$timelen[1] + t4$timelen[2]
for (i in 1:lengt) {
  t4$timeacc[i+2] = t4$timelen[i+2] + t4$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t4$timeacc[i] < onset$animal[4]) {
    t4$timewindow[i] = 1
  } else if (t4$timeacc[i] > onset$animal[4] && t4$timeacc[i] < onset$object[4]) {
    t4$timewindow[i] = 2
  } else if (t4$timeacc[i] > onset$object[4]) {
    t4$timewindow[i] = 3
  } else {
  }
}


### t5
### time length
t5 <- Data[which(Data$TrialID==5),]
lengt <- length(t5$Condition)
for (j in 1:lengt) {
  t5$timelen[j] = t5$Recording_timestamp[j+1] - t5$Recording_timestamp[j]
}

### time accumulation
t5$timeacc[1] <- 0
t5$timeacc[2] = t5$timelen[1] + t5$timelen[2]
for (i in 1:lengt) {
  t5$timeacc[i+2] = t5$timelen[i+2] + t5$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t5$timeacc[i] < onset$animal[5]) {
    t5$timewindow[i] = 1
  } else if (t5$timeacc[i] > onset$animal[5] && t5$timeacc[i] < onset$object[5]) {
    t5$timewindow[i] = 2
  } else if (t5$timeacc[i] > onset$object[5]) {
    t5$timewindow[i] = 3
  } else {
  }
}



### t6
### time length
t6 <- Data[which(Data$TrialID==6),]
lengt <- length(t5$Condition)
for (j in 1:lengt) {
  t6$timelen[j] = t6$Recording_timestamp[j+1] - t6$Recording_timestamp[j]
}

### time accumulation
t6$timeacc[1] <- 0
t6$timeacc[2] = t6$timelen[1] + t6$timelen[2]
for (i in 1:lengt) {
  t6$timeacc[i+2] = t6$timelen[i+2] + t6$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t6$timeacc[i] < onset$animal[6]) {
    t6$timewindow[i] = 1
  } else if (t6$timeacc[i] > onset$animal[6] && t6$timeacc[i] < onset$object[6]) {
    t6$timewindow[i] = 2
  } else if (t6$timeacc[i] > onset$object[6]) {
    t6$timewindow[i] = 3
  } else {
  }
}



### t7
### time length
t7 <- Data[which(Data$TrialID==7),]
lengt <- length(t7$Condition)
for (j in 1:lengt) {
  t7$timelen[j] = t7$Recording_timestamp[j+1] - t7$Recording_timestamp[j]
}

### time accumulation
t7$timeacc[1] <- 0
t7$timeacc[2] = t7$timelen[1] + t7$timelen[2]
for (i in 1:lengt) {
  t7$timeacc[i+2] = t7$timelen[i+2] + t7$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t7$timeacc[i] < onset$animal[7]) {
    t7$timewindow[i] = 1
  } else if (t7$timeacc[i] > onset$animal[7] && t7$timeacc[i] < onset$object[7]) {
    t7$timewindow[i] = 2
  } else if (t7$timeacc[i] > onset$object[7]) {
    t7$timewindow[i] = 3
  } else {
  }
}


### t8
### time length
t8 <- Data[which(Data$TrialID==8),]
lengt <- length(t8$Condition)
for (j in 1:lengt) {
  t8$timelen[j] = t8$Recording_timestamp[j+1] - t8$Recording_timestamp[j]
}

### time accumulation
t8$timeacc[1] <- 0
t8$timeacc[2] = t8$timelen[1] + t8$timelen[2]
for (i in 1:lengt) {
  t8$timeacc[i+2] = t8$timelen[i+2] + t8$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t8$timeacc[i] < onset$animal[8]) {
    t8$timewindow[i] = 1
  } else if (t8$timeacc[i] > onset$animal[8] && t8$timeacc[i] < onset$object[8]) {
    t8$timewindow[i] = 2
  } else if (t8$timeacc[i] > onset$object[8]) {
    t8$timewindow[i] = 3
  } else {
  }
}


### t9
### time length
t9 <- Data[which(Data$TrialID==9),]
lengt <- length(t9$Condition)
for (j in 1:lengt) {
  t9$timelen[j] = t9$Recording_timestamp[j+1] - t9$Recording_timestamp[j]
}

### time accumulation
t9$timeacc[1] <- 0
t9$timeacc[2] = t9$timelen[1] + t9$timelen[2]
for (i in 1:lengt) {
  t9$timeacc[i+2] = t9$timelen[i+2] + t9$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t9$timeacc[i] < onset$animal[9]) {
    t9$timewindow[i] = 1
  } else if (t9$timeacc[i] > onset$animal[9] && t9$timeacc[i] < onset$object[9]) {
    t9$timewindow[i] = 2
  } else if (t9$timeacc[i] > onset$object[9]) {
    t9$timewindow[i] = 3
  } else {
  }
}


### t10
### time length
t10 <- Data[which(Data$TrialID==10),]
lengt <- length(t10$Condition)
for (j in 1:lengt) {
  t10$timelen[j] = t10$Recording_timestamp[j+1] - t10$Recording_timestamp[j]
}

### time accumulation
t10$timeacc[1] <- 0
t10$timeacc[2] = t10$timelen[1] + t10$timelen[2]
for (i in 1:lengt) {
  t10$timeacc[i+2] = t10$timelen[i+2] + t10$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t10$timeacc[i] < onset$animal[10]) {
    t10$timewindow[i] = 1
  } else if (t10$timeacc[i] > onset$animal[10] && t10$timeacc[i] < onset$object[10]) {
    t10$timewindow[i] = 2
  } else if (t10$timeacc[i] > onset$object[10]) {
    t10$timewindow[i] = 3
  } else {
  }
}


### t11
### time length
t11 <- Data[which(Data$TrialID==11),]
lengt <- length(t11$Condition)
for (j in 1:lengt) {
  t11$timelen[j] = t11$Recording_timestamp[j+1] - t11$Recording_timestamp[j]
}

### time accumulation
t11$timeacc[1] <- 0
t11$timeacc[2] = t11$timelen[1] + t11$timelen[2]
for (i in 1:lengt) {
  t11$timeacc[i+2] = t11$timelen[i+2] + t11$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t11$timeacc[i] < onset$animal[11]) {
    t11$timewindow[i] = 1
  } else if (t11$timeacc[i] > onset$animal[11] && t11$timeacc[i] < onset$object[11]) {
    t11$timewindow[i] = 2
  } else if (t11$timeacc[i] > onset$object[11]) {
    t11$timewindow[i] = 3
  } else {
  }
}


### t12
### time length
t12 <- Data[which(Data$TrialID==12),]
lengt <- length(t12$Condition)
for (j in 1:lengt) {
  t12$timelen[j] = t12$Recording_timestamp[j+1] - t12$Recording_timestamp[j]
}

### time accumulation
t12$timeacc[1] <- 0
t12$timeacc[2] = t12$timelen[1] + t12$timelen[2]
for (i in 1:lengt) {
  t12$timeacc[i+2] = t12$timelen[i+2] + t12$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t12$timeacc[i] < onset$animal[12]) {
    t12$timewindow[i] = 1
  } else if (t12$timeacc[i] > onset$animal[12] && t12$timeacc[i] < onset$object[12]) {
    t12$timewindow[i] = 2
  } else if (t12$timeacc[i] > onset$object[12]) {
    t12$timewindow[i] = 3
  } else {
  }
}


### t13
### time length
t13 <- Data[which(Data$TrialID==13),]
lengt <- length(t13$Condition)
for (j in 1:lengt) {
  t13$timelen[j] = t13$Recording_timestamp[j+1] - t13$Recording_timestamp[j]
}

### time accumulation
t13$timeacc[1] <- 0
t13$timeacc[2] = t13$timelen[1] + t13$timelen[2]
for (i in 1:lengt) {
  t13$timeacc[i+2] = t13$timelen[i+2] + t13$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t13$timeacc[i] < onset$animal[13]) {
    t13$timewindow[i] = 1
  } else if (t13$timeacc[i] > onset$animal[13] && t13$timeacc[i] < onset$object[13]) {
    t13$timewindow[i] = 2
  } else if (t13$timeacc[i] > onset$object[13]) {
    t13$timewindow[i] = 3
  } else {
  }
}


### t14
### time length
t14 <- Data[which(Data$TrialID==14),]
lengt <- length(t14$Condition)
for (j in 1:lengt) {
  t14$timelen[j] = t14$Recording_timestamp[j+1] - t14$Recording_timestamp[j]
}

### time accumulation
t14$timeacc[1] <- 0
t14$timeacc[2] = t14$timelen[1] + t14$timelen[2]
for (i in 1:lengt) {
  t14$timeacc[i+2] = t14$timelen[i+2] + t14$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t14$timeacc[i] < onset$animal[14]) {
    t14$timewindow[i] = 1
  } else if (t14$timeacc[i] > onset$animal[14] && t14$timeacc[i] < onset$object[14]) {
    t14$timewindow[i] = 2
  } else if (t14$timeacc[i] > onset$object[14]) {
    t14$timewindow[i] = 3
  } else {
  }
}



### t15
### time length
t15 <- Data[which(Data$TrialID==15),]
lengt <- length(t15$Condition)
for (j in 1:lengt) {
  t15$timelen[j] = t15$Recording_timestamp[j+1] - t15$Recording_timestamp[j]
}

### time accumulation
t15$timeacc[1] <- 0
t15$timeacc[2] = t15$timelen[1] + t15$timelen[2]
for (i in 1:lengt) {
  t15$timeacc[i+2] = t15$timelen[i+2] + t15$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t15$timeacc[i] < onset$animal[15]) {
    t15$timewindow[i] = 1
  } else if (t15$timeacc[i] > onset$animal[15] && t15$timeacc[i] < onset$object[15]) {
    t15$timewindow[i] = 2
  } else if (t15$timeacc[i] > onset$object[15]) {
    t15$timewindow[i] = 3
  } else {
  }
}



### t16
### time length
t16 <- Data[which(Data$TrialID==16),]
lengt <- length(t16$Condition)
for (j in 1:lengt) {
  t16$timelen[j] = t16$Recording_timestamp[j+1] - t16$Recording_timestamp[j]
}

### time accumulation
t16$timeacc[1] <- 0
t16$timeacc[2] = t16$timelen[1] + t16$timelen[2]
for (i in 1:lengt) {
  t16$timeacc[i+2] = t16$timelen[i+2] + t16$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t16$timeacc[i] < onset$animal[16]) {
    t16$timewindow[i] = 1
  } else if (t16$timeacc[i] > onset$animal[16] && t16$timeacc[i] < onset$object[16]) {
    t16$timewindow[i] = 2
  } else if (t16$timeacc[i] > onset$object[16]) {
    t16$timewindow[i] = 3
  } else {
  }
}


### t17
### time length
t17 <- Data[which(Data$TrialID==17),]
lengt <- length(t17$Condition)
for (j in 1:lengt) {
  t17$timelen[j] = t17$Recording_timestamp[j+1] - t17$Recording_timestamp[j]
}

### time accumulation
t17$timeacc[1] <- 0
t17$timeacc[2] = t17$timelen[1] + t17$timelen[2]
for (i in 1:lengt) {
  t17$timeacc[i+2] = t17$timelen[i+2] + t17$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t17$timeacc[i] < onset$animal[17]) {
    t17$timewindow[i] = 1
  } else if (t17$timeacc[i] > onset$animal[17] && t17$timeacc[i] < onset$object[17]) {
    t17$timewindow[i] = 2
  } else if (t17$timeacc[i] > onset$object[17]) {
    t17$timewindow[i] = 3
  } else {
  }
}


### t18
### time length
t18 <- Data[which(Data$TrialID==18),]
lengt <- length(t18$Condition)
for (j in 1:lengt) {
  t18$timelen[j] = t18$Recording_timestamp[j+1] - t18$Recording_timestamp[j]
}

### time accumulation
t18$timeacc[1] <- 0
t18$timeacc[2] = t18$timelen[1] + t18$timelen[2]
for (i in 1:lengt) {
  t18$timeacc[i+2] = t18$timelen[i+2] + t18$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t18$timeacc[i] < onset$animal[18]) {
    t18$timewindow[i] = 1
  } else if (t18$timeacc[i] > onset$animal[18] && t18$timeacc[i] < onset$object[18]) {
    t18$timewindow[i] = 2
  } else if (t18$timeacc[i] > onset$object[18]) {
    t18$timewindow[i] = 3
  } else {
  }
}



### t19
### time length
t19 <- Data[which(Data$TrialID==19),]
lengt <- length(t19$Condition)
for (j in 1:lengt) {
  t19$timelen[j] = t19$Recording_timestamp[j+1] - t19$Recording_timestamp[j]
}

### time accumulation
t19$timeacc[1] <- 0
t19$timeacc[2] = t19$timelen[1] + t19$timelen[2]
for (i in 1:lengt) {
  t19$timeacc[i+2] = t19$timelen[i+2] + t19$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t19$timeacc[i] < onset$animal[19]) {
    t19$timewindow[i] = 1
  } else if (t19$timeacc[i] > onset$animal[19] && t19$timeacc[i] < onset$object[19]) {
    t19$timewindow[i] = 2
  } else if (t19$timeacc[i] > onset$object[19]) {
    t19$timewindow[i] = 3
  } else {
  }
}


### t20
### time length
t20 <- Data[which(Data$TrialID==20),]
lengt <- length(t20$Condition)
for (j in 1:lengt) {
  t20$timelen[j] = t20$Recording_timestamp[j+1] - t20$Recording_timestamp[j]
}

### time accumulation
t20$timeacc[1] <- 0
t20$timeacc[2] = t20$timelen[1] + t20$timelen[2]
for (i in 1:lengt) {
  t20$timeacc[i+2] = t20$timelen[i+2] + t20$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t20$timeacc[i] < onset$animal[20]) {
    t20$timewindow[i] = 1
  } else if (t20$timeacc[i] > onset$animal[20] && t20$timeacc[i] < onset$object[20]) {
    t20$timewindow[i] = 2
  } else if (t20$timeacc[i] > onset$object[20]) {
    t20$timewindow[i] = 3
  } else {
  }
}


### t21
### time length
t21 <- Data[which(Data$TrialID==21),]
lengt <- length(t21$Condition)
for (j in 1:lengt) {
  t21$timelen[j] = t21$Recording_timestamp[j+1] - t21$Recording_timestamp[j]
}

### time accumulation
t21$timeacc[1] <- 0
t21$timeacc[2] = t21$timelen[1] + t21$timelen[2]
for (i in 1:lengt) {
  t21$timeacc[i+2] = t21$timelen[i+2] + t21$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t21$timeacc[i] < onset$animal[21]) {
    t21$timewindow[i] = 1
  } else if (t21$timeacc[i] > onset$animal[21] && t21$timeacc[i] < onset$object[21]) {
    t21$timewindow[i] = 2
  } else if (t21$timeacc[i] > onset$object[21]) {
    t21$timewindow[i] = 3
  } else {
  }
}



### t22
### time length
t22 <- Data[which(Data$TrialID==22),]
lengt <- length(t22$Condition)
for (j in 1:lengt) {
  t22$timelen[j] = t22$Recording_timestamp[j+1] - t22$Recording_timestamp[j]
}

### time accumulation
t22$timeacc[1] <- 0
t22$timeacc[2] = t22$timelen[1] + t22$timelen[2]
for (i in 1:lengt) {
  t22$timeacc[i+2] = t22$timelen[i+2] + t22$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t22$timeacc[i] < onset$animal[22]) {
    t22$timewindow[i] = 1
  } else if (t22$timeacc[i] > onset$animal[22] && t22$timeacc[i] < onset$object[22]) {
    t22$timewindow[i] = 2
  } else if (t22$timeacc[i] > onset$object[22]) {
    t22$timewindow[i] = 3
  } else {
  }
}


### t23
### time length
t23 <- Data[which(Data$TrialID==23),]
lengt <- length(t23$Condition)
for (j in 1:lengt) {
  t23$timelen[j] = t23$Recording_timestamp[j+1] - t23$Recording_timestamp[j]
}

### time accumulation
t23$timeacc[1] <- 0
t23$timeacc[2] = t23$timelen[1] + t23$timelen[2]
for (i in 1:lengt) {
  t23$timeacc[i+2] = t23$timelen[i+2] + t23$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t23$timeacc[i] < onset$animal[23]) {
    t23$timewindow[i] = 1
  } else if (t23$timeacc[i] > onset$animal[23] && t23$timeacc[i] < onset$object[23]) {
    t23$timewindow[i] = 2
  } else if (t23$timeacc[i] > onset$object[23]) {
    t23$timewindow[i] = 3
  } else {
  }
}


### t24
### time length
t24 <- Data[which(Data$TrialID==24),]
lengt <- length(t24$Condition)
for (j in 1:lengt) {
  t24$timelen[j] = t24$Recording_timestamp[j+1] - t24$Recording_timestamp[j]
}

### time accumulation
t24$timeacc[1] <- 0
t24$timeacc[2] = t24$timelen[1] + t24$timelen[2]
for (i in 1:lengt) {
  t24$timeacc[i+2] = t24$timelen[i+2] + t24$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t24$timeacc[i] < onset$animal[24]) {
    t24$timewindow[i] = 1
  } else if (t24$timeacc[i] > onset$animal[24] && t24$timeacc[i] < onset$object[24]) {
    t24$timewindow[i] = 2
  } else if (t24$timeacc[i] > onset$object[24]) {
    t24$timewindow[i] = 3
  } else {
  }
}


### t25
### time length
t25 <- Data[which(Data$TrialID==25),]
lengt <- length(t25$Condition)
for (j in 1:lengt) {
  t25$timelen[j] = t25$Recording_timestamp[j+1] - t25$Recording_timestamp[j]
}

### time accumulation
t25$timeacc[1] <- 0
t25$timeacc[2] = t25$timelen[1] + t25$timelen[2]
for (i in 1:lengt) {
  t25$timeacc[i+2] = t25$timelen[i+2] + t25$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t25$timeacc[i] < onset$animal[25]) {
    t25$timewindow[i] = 1
  } else if (t25$timeacc[i] > onset$animal[25] && t25$timeacc[i] < onset$object[25]) {
    t25$timewindow[i] = 2
  } else if (t25$timeacc[i] > onset$object[25]) {
    t25$timewindow[i] = 3
  } else {
  }
}


### t26
### time length
t26 <- Data[which(Data$TrialID==26),]
lengt <- length(t26$Condition)
for (j in 1:lengt) {
  t26$timelen[j] = t26$Recording_timestamp[j+1] - t26$Recording_timestamp[j]
}

### time accumulation
t26$timeacc[1] <- 0
t26$timeacc[2] = t26$timelen[1] + t26$timelen[2]
for (i in 1:lengt) {
  t26$timeacc[i+2] = t26$timelen[i+2] + t26$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t26$timeacc[i] < onset$animal[26]) {
    t26$timewindow[i] = 1
  } else if (t26$timeacc[i] > onset$animal[26] && t26$timeacc[i] < onset$object[26]) {
    t26$timewindow[i] = 2
  } else if (t26$timeacc[i] > onset$object[26]) {
    t26$timewindow[i] = 3
  } else {
  }
}


### t27
### time length
t27 <- Data[which(Data$TrialID==27),]
lengt <- length(t27$Condition)
for (j in 1:lengt) {
  t27$timelen[j] = t27$Recording_timestamp[j+1] - t27$Recording_timestamp[j]
}

### time accumulation
t27$timeacc[1] <- 0
t27$timeacc[2] = t27$timelen[1] + t27$timelen[2]
for (i in 1:lengt) {
  t27$timeacc[i+2] = t27$timelen[i+2] + t27$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t27$timeacc[i] < onset$animal[27]) {
    t27$timewindow[i] = 1
  } else if (t27$timeacc[i] > onset$animal[27] && t27$timeacc[i] < onset$object[27]) {
    t27$timewindow[i] = 2
  } else if (t27$timeacc[i] > onset$object[27]) {
    t27$timewindow[i] = 3
  } else {
  }
}


### t28
### time length
t28 <- Data[which(Data$TrialID==28),]
lengt <- length(t28$Condition)
for (j in 1:lengt) {
  t28$timelen[j] = t28$Recording_timestamp[j+1] - t28$Recording_timestamp[j]
}

### time accumulation
t28$timeacc[1] <- 0
t28$timeacc[2] = t28$timelen[1] + t28$timelen[2]
for (i in 1:lengt) {
  t28$timeacc[i+2] = t28$timelen[i+2] + t28$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t28$timeacc[i] < onset$animal[28]) {
    t28$timewindow[i] = 1
  } else if (t28$timeacc[i] > onset$animal[28] && t28$timeacc[i] < onset$object[28]) {
    t28$timewindow[i] = 2
  } else if (t28$timeacc[i] > onset$object[28]) {
    t28$timewindow[i] = 3
  } else {
  }
}


### t29
### time length
t29 <- Data[which(Data$TrialID==29),]
lengt <- length(t29$Condition)
for (j in 1:lengt) {
  t29$timelen[j] = t29$Recording_timestamp[j+1] - t29$Recording_timestamp[j]
}

### time accumulation
t29$timeacc[1] <- 0
t29$timeacc[2] = t29$timelen[1] + t29$timelen[2]
for (i in 1:lengt) {
  t29$timeacc[i+2] = t29$timelen[i+2] + t29$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t29$timeacc[i] < onset$animal[29]) {
    t29$timewindow[i] = 1
  } else if (t29$timeacc[i] > onset$animal[29] && t29$timeacc[i] < onset$object[29]) {
    t29$timewindow[i] = 2
  } else if (t29$timeacc[i] > onset$object[29]) {
    t29$timewindow[i] = 3
  } else {
  }
}

### t30
### time length
t30 <- Data[which(Data$TrialID==30),]
lengt <- length(t30$Condition)
for (j in 1:lengt) {
  t30$timelen[j] = t30$Recording_timestamp[j+1] - t30$Recording_timestamp[j]
}

### time accumulation
t30$timeacc[1] <- 0
t30$timeacc[2] = t30$timelen[1] + t30$timelen[2]
for (i in 1:lengt) {
  t30$timeacc[i+2] = t30$timelen[i+2] + t30$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t30$timeacc[i] < onset$animal[30]) {
    t30$timewindow[i] = 1
  } else if (t30$timeacc[i] > onset$animal[30] && t30$timeacc[i] < onset$object[30]) {
    t30$timewindow[i] = 2
  } else if (t30$timeacc[i] > onset$object[30]) {
    t30$timewindow[i] = 3
  } else {
  }
}

### t31
### time length
t31 <- Data[which(Data$TrialID==31),]
lengt <- length(t31$Condition)
for (j in 1:lengt) {
  t31$timelen[j] = t31$Recording_timestamp[j+1] - t31$Recording_timestamp[j]
}

### time accumulation
t31$timeacc[1] <- 0
t31$timeacc[2] = t31$timelen[1] + t31$timelen[2]
for (i in 1:lengt) {
  t31$timeacc[i+2] = t31$timelen[i+2] + t31$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t31$timeacc[i] < onset$animal[31]) {
    t31$timewindow[i] = 1
  } else if (t31$timeacc[i] > onset$animal[31] && t31$timeacc[i] < onset$object[31]) {
    t31$timewindow[i] = 2
  } else if (t31$timeacc[i] > onset$object[31]) {
    t31$timewindow[i] = 3
  } else {
  }
}


### t32
### time length
t32 <- Data[which(Data$TrialID==32),]
lengt <- length(t32$Condition)
for (j in 1:lengt) {
  t32$timelen[j] = t32$Recording_timestamp[j+1] - t32$Recording_timestamp[j]
}

### time accumulation
t32$timeacc[1] <- 0
t32$timeacc[2] = t32$timelen[1] + t32$timelen[2]
for (i in 1:lengt) {
  t32$timeacc[i+2] = t32$timelen[i+2] + t32$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t32$timeacc[i] < onset$animal[32]) {
    t32$timewindow[i] = 1
  } else if (t32$timeacc[i] > onset$animal[32] && t32$timeacc[i] < onset$object[32]) {
    t32$timewindow[i] = 2
  } else if (t32$timeacc[i] > onset$object[32]) {
    t32$timewindow[i] = 3
  } else {
  }
}


### t33
### time length
t33 <- Data[which(Data$TrialID==33),]
lengt <- length(t33$Condition)
for (j in 1:lengt) {
  t33$timelen[j] = t33$Recording_timestamp[j+1] - t33$Recording_timestamp[j]
}

### time accumulation
t33$timeacc[1] <- 0
t33$timeacc[2] = t33$timelen[1] + t33$timelen[2]
for (i in 1:lengt) {
  t33$timeacc[i+2] = t33$timelen[i+2] + t33$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t33$timeacc[i] < onset$animal[33]) {
    t33$timewindow[i] = 1
  } else if (t33$timeacc[i] > onset$animal[33] && t33$timeacc[i] < onset$object[33]) {
    t33$timewindow[i] = 2
  } else if (t33$timeacc[i] > onset$object[33]) {
    t33$timewindow[i] = 3
  } else {
  }
}


### t34
### time length
t34 <- Data[which(Data$TrialID==34),]
lengt <- length(t34$Condition)
for (j in 1:lengt) {
  t34$timelen[j] = t34$Recording_timestamp[j+1] - t34$Recording_timestamp[j]
}

### time accumulation
t34$timeacc[1] <- 0
t34$timeacc[2] = t34$timelen[1] + t34$timelen[2]
for (i in 1:lengt) {
  t34$timeacc[i+2] = t34$timelen[i+2] + t34$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t34$timeacc[i] < onset$animal[34]) {
    t34$timewindow[i] = 1
  } else if (t34$timeacc[i] > onset$animal[34] && t34$timeacc[i] < onset$object[34]) {
    t34$timewindow[i] = 2
  } else if (t34$timeacc[i] > onset$object[34]) {
    t34$timewindow[i] = 3
  } else {
  }
}

### t35
### time length
t35 <- Data[which(Data$TrialID==35),]
lengt <- length(t35$Condition)
for (j in 1:lengt) {
  t35$timelen[j] = t35$Recording_timestamp[j+1] - t35$Recording_timestamp[j]
}

### time accumulation
t35$timeacc[1] <- 0
t35$timeacc[2] = t35$timelen[1] + t35$timelen[2]
for (i in 1:lengt) {
  t35$timeacc[i+2] = t35$timelen[i+2] + t35$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t35$timeacc[i] < onset$animal[35]) {
    t35$timewindow[i] = 1
  } else if (t35$timeacc[i] > onset$animal[35] && t35$timeacc[i] < onset$object[35]) {
    t35$timewindow[i] = 2
  } else if (t35$timeacc[i] > onset$object[35]) {
    t35$timewindow[i] = 3
  } else {
  }
}

### t36
### time length
t36 <- Data[which(Data$TrialID==36),]
lengt <- length(t36$Condition)
for (j in 1:lengt) {
  t36$timelen[j] = t36$Recording_timestamp[j+1] - t36$Recording_timestamp[j]
}

### time accumulation
t36$timeacc[1] <- 0
t36$timeacc[2] = t36$timelen[1] + t36$timelen[2]
for (i in 1:lengt) {
  t36$timeacc[i+2] = t36$timelen[i+2] + t36$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t36$timeacc[i] < onset$animal[36]) {
    t36$timewindow[i] = 1
  } else if (t36$timeacc[i] > onset$animal[36] && t36$timeacc[i] < onset$object[36]) {
    t36$timewindow[i] = 2
  } else if (t36$timeacc[i] > onset$object[36]) {
    t36$timewindow[i] = 3
  } else {
  }
}


### t37
### time length
t37 <- Data[which(Data$TrialID==37),]
lengt <- length(t37$Condition)
for (j in 1:lengt) {
  t37$timelen[j] = t37$Recording_timestamp[j+1] - t37$Recording_timestamp[j]
}

### time accumulation
t37$timeacc[1] <- 0
t37$timeacc[2] = t37$timelen[1] + t37$timelen[2]
for (i in 1:lengt) {
  t37$timeacc[i+2] = t37$timelen[i+2] + t37$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t37$timeacc[i] < onset$animal[37]) {
    t37$timewindow[i] = 1
  } else if (t37$timeacc[i] > onset$animal[37] && t37$timeacc[i] < onset$object[37]) {
    t37$timewindow[i] = 2
  } else if (t37$timeacc[i] > onset$object[37]) {
    t37$timewindow[i] = 3
  } else {
  }
}

### t38
### time length
t38 <- Data[which(Data$TrialID==38),]
lengt <- length(t38$Condition)
for (j in 1:lengt) {
  t38$timelen[j] = t38$Recording_timestamp[j+1] - t38$Recording_timestamp[j]
}

### time accumulation
t38$timeacc[1] <- 0
t38$timeacc[2] = t38$timelen[1] + t38$timelen[2]
for (i in 1:lengt) {
  t38$timeacc[i+2] = t38$timelen[i+2] + t38$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t38$timeacc[i] < onset$animal[38]) {
    t38$timewindow[i] = 1
  } else if (t38$timeacc[i] > onset$animal[38] && t38$timeacc[i] < onset$object[38]) {
    t38$timewindow[i] = 2
  } else if (t38$timeacc[i] > onset$object[38]) {
    t38$timewindow[i] = 3
  } else {
  }
}


### t39
### time length
t39 <- Data[which(Data$TrialID==39),]
lengt <- length(t39$Condition)
for (j in 1:lengt) {
  t39$timelen[j] = t39$Recording_timestamp[j+1] - t39$Recording_timestamp[j]
}

### time accumulation
t39$timeacc[1] <- 0
t39$timeacc[2] = t39$timelen[1] + t39$timelen[2]
for (i in 1:lengt) {
  t39$timeacc[i+2] = t39$timelen[i+2] + t39$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t39$timeacc[i] < onset$animal[39]) {
    t39$timewindow[i] = 1
  } else if (t39$timeacc[i] > onset$animal[39] && t39$timeacc[i] < onset$object[39]) {
    t39$timewindow[i] = 2
  } else if (t39$timeacc[i] > onset$object[39]) {
    t39$timewindow[i] = 3
  } else {
  }
}

### t40
### time length
t40 <- Data[which(Data$TrialID==40),]
lengt <- length(t40$Condition)
for (j in 1:lengt) {
  t40$timelen[j] = t40$Recording_timestamp[j+1] - t40$Recording_timestamp[j]
}

### time accumulation
t40$timeacc[1] <- 0
t40$timeacc[2] = t40$timelen[1] + t40$timelen[2]
for (i in 1:lengt) {
  t40$timeacc[i+2] = t40$timelen[i+2] + t40$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t40$timeacc[i] < onset$animal[40]) {
    t40$timewindow[i] = 1
  } else if (t40$timeacc[i] > onset$animal[40] && t40$timeacc[i] < onset$object[40]) {
    t40$timewindow[i] = 2
  } else if (t40$timeacc[i] > onset$object[40]) {
    t40$timewindow[i] = 3
  } else {
  }
}

### t41
### time length
t41 <- Data[which(Data$TrialID==41),]
lengt <- length(t41$Condition)
for (j in 1:lengt) {
  t41$timelen[j] = t41$Recording_timestamp[j+1] - t41$Recording_timestamp[j]
}

### time accumulation
t41$timeacc[1] <- 0
t41$timeacc[2] = t41$timelen[1] + t41$timelen[2]
for (i in 1:lengt) {
  t41$timeacc[i+2] = t41$timelen[i+2] + t41$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t41$timeacc[i] < onset$animal[41]) {
    t41$timewindow[i] = 1
  } else if (t41$timeacc[i] > onset$animal[41] && t41$timeacc[i] < onset$object[41]) {
    t41$timewindow[i] = 2
  } else if (t41$timeacc[i] > onset$object[41]) {
    t41$timewindow[i] = 3
  } else {
  }
}


### t42
### time length
t42 <- Data[which(Data$TrialID==42),]
lengt <- length(t42$Condition)
for (j in 1:lengt) {
  t42$timelen[j] = t42$Recording_timestamp[j+1] - t42$Recording_timestamp[j]
}

### time accumulation
t42$timeacc[1] <- 0
t42$timeacc[2] = t42$timelen[1] + t42$timelen[2]
for (i in 1:lengt) {
  t42$timeacc[i+2] = t42$timelen[i+2] + t42$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t42$timeacc[i] < onset$animal[42]) {
    t42$timewindow[i] = 1
  } else if (t42$timeacc[i] > onset$animal[42] && t42$timeacc[i] < onset$object[42]) {
    t42$timewindow[i] = 2
  } else if (t42$timeacc[i] > onset$object[42]) {
    t42$timewindow[i] = 3
  } else {
  }
}

### t43
### time length
t43 <- Data[which(Data$TrialID==43),]
lengt <- length(t43$Condition)
for (j in 1:lengt) {
  t43$timelen[j] = t43$Recording_timestamp[j+1] - t43$Recording_timestamp[j]
}

### time accumulation
t43$timeacc[1] <- 0
t43$timeacc[2] = t43$timelen[1] + t43$timelen[2]
for (i in 1:lengt) {
  t43$timeacc[i+2] = t43$timelen[i+2] + t43$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t43$timeacc[i] < onset$animal[43]) {
    t43$timewindow[i] = 1
  } else if (t43$timeacc[i] > onset$animal[43] && t43$timeacc[i] < onset$object[43]) {
    t43$timewindow[i] = 2
  } else if (t43$timeacc[i] > onset$object[43]) {
    t43$timewindow[i] = 3
  } else {
  }
}


### t44
### time length
t44 <- Data[which(Data$TrialID==44),]
lengt <- length(t44$Condition)
for (j in 1:lengt) {
  t44$timelen[j] = t44$Recording_timestamp[j+1] - t44$Recording_timestamp[j]
}

### time accumulation
t44$timeacc[1] <- 0
t44$timeacc[2] = t44$timelen[1] + t44$timelen[2]
for (i in 1:lengt) {
  t44$timeacc[i+2] = t44$timelen[i+2] + t44$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t44$timeacc[i] < onset$animal[44]) {
    t44$timewindow[i] = 1
  } else if (t44$timeacc[i] > onset$animal[44] && t44$timeacc[i] < onset$object[44]) {
    t44$timewindow[i] = 2
  } else if (t44$timeacc[i] > onset$object[44]) {
    t44$timewindow[i] = 3
  } else {
  }
}


### t45
### time length
t45 <- Data[which(Data$TrialID==45),]
lengt <- length(t45$Condition)
for (j in 1:lengt) {
  t45$timelen[j] = t45$Recording_timestamp[j+1] - t45$Recording_timestamp[j]
}

### time accumulation
t45$timeacc[1] <- 0
t45$timeacc[2] = t45$timelen[1] + t45$timelen[2]
for (i in 1:lengt) {
  t45$timeacc[i+2] = t45$timelen[i+2] + t45$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t45$timeacc[i] < onset$animal[45]) {
    t45$timewindow[i] = 1
  } else if (t45$timeacc[i] > onset$animal[45] && t45$timeacc[i] < onset$object[45]) {
    t45$timewindow[i] = 2
  } else if (t45$timeacc[i] > onset$object[45]) {
    t45$timewindow[i] = 3
  } else {
  }
}


### t46
### time length
t46 <- Data[which(Data$TrialID==46),]
lengt <- length(t46$Condition)
for (j in 1:lengt) {
  t46$timelen[j] = t46$Recording_timestamp[j+1] - t46$Recording_timestamp[j]
}

### time accumulation
t46$timeacc[1] <- 0
t46$timeacc[2] = t46$timelen[1] + t46$timelen[2]
for (i in 1:lengt) {
  t46$timeacc[i+2] = t46$timelen[i+2] + t46$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t46$timeacc[i] < onset$animal[46]) {
    t46$timewindow[i] = 1
  } else if (t46$timeacc[i] > onset$animal[46] && t46$timeacc[i] < onset$object[46]) {
    t46$timewindow[i] = 2
  } else if (t46$timeacc[i] > onset$object[46]) {
    t46$timewindow[i] = 3
  } else {
  }
}


### t47
### time length
t47 <- Data[which(Data$TrialID==47),]
lengt <- length(t47$Condition)
for (j in 1:lengt) {
  t47$timelen[j] = t47$Recording_timestamp[j+1] - t47$Recording_timestamp[j]
}

### time accumulation
t47$timeacc[1] <- 0
t47$timeacc[2] = t47$timelen[1] + t47$timelen[2]
for (i in 1:lengt) {
  t47$timeacc[i+2] = t47$timelen[i+2] + t47$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t47$timeacc[i] < onset$animal[47]) {
    t47$timewindow[i] = 1
  } else if (t47$timeacc[i] > onset$animal[47] && t47$timeacc[i] < onset$object[47]) {
    t47$timewindow[i] = 2
  } else if (t47$timeacc[i] > onset$object[47]) {
    t47$timewindow[i] = 3
  } else {
  }
}


### t48
### time length
t48 <- Data[which(Data$TrialID==48),]
lengt <- length(t48$Condition)
for (j in 1:lengt) {
  t48$timelen[j] = t48$Recording_timestamp[j+1] - t48$Recording_timestamp[j]
}

### time accumulation
t48$timeacc[1] <- 0
t48$timeacc[2] = t48$timelen[1] + t48$timelen[2]
for (i in 1:lengt) {
  t48$timeacc[i+2] = t48$timelen[i+2] + t48$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t48$timeacc[i] < onset$animal[48]) {
    t48$timewindow[i] = 1
  } else if (t48$timeacc[i] > onset$animal[48] && t48$timeacc[i] < onset$object[48]) {
    t48$timewindow[i] = 2
  } else if (t48$timeacc[i] > onset$object[48]) {
    t48$timewindow[i] = 3
  } else {
  }
}


### t49
### time length
t49 <- Data[which(Data$TrialID==49),]
lengt <- length(t49$Condition)
for (j in 1:lengt) {
  t49$timelen[j] = t49$Recording_timestamp[j+1] - t49$Recording_timestamp[j]
}

### time accumulation
t49$timeacc[1] <- 0
t49$timeacc[2] = t49$timelen[1] + t49$timelen[2]
for (i in 1:lengt) {
  t49$timeacc[i+2] = t49$timelen[i+2] + t49$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t49$timeacc[i] < onset$animal[49]) {
    t49$timewindow[i] = 1
  } else if (t49$timeacc[i] > onset$animal[49] && t49$timeacc[i] < onset$object[49]) {
    t49$timewindow[i] = 2
  } else if (t49$timeacc[i] > onset$object[49]) {
    t49$timewindow[i] = 3
  } else {
  }
}


### t50
### time length
t50 <- Data[which(Data$TrialID==50),]
lengt <- length(t50$Condition)
for (j in 1:lengt) {
  t50$timelen[j] = t50$Recording_timestamp[j+1] - t50$Recording_timestamp[j]
}

### time accumulation
t50$timeacc[1] <- 0
t50$timeacc[2] = t50$timelen[1] + t50$timelen[2]
for (i in 1:lengt) {
  t50$timeacc[i+2] = t50$timelen[i+2] + t50$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t50$timeacc[i] < onset$animal[50]) {
    t50$timewindow[i] = 1
  } else if (t50$timeacc[i] > onset$animal[50] && t50$timeacc[i] < onset$object[50]) {
    t50$timewindow[i] = 2
  } else if (t50$timeacc[i] > onset$object[50]) {
    t50$timewindow[i] = 3
  } else {
  }
}

### t51
### time length
t51 <- Data[which(Data$TrialID==51),]
lengt <- length(t51$Condition)
for (j in 1:lengt) {
  t51$timelen[j] = t51$Recording_timestamp[j+1] - t51$Recording_timestamp[j]
}

### time accumulation
t51$timeacc[1] <- 0
t51$timeacc[2] = t51$timelen[1] + t51$timelen[2]
for (i in 1:lengt) {
  t51$timeacc[i+2] = t51$timelen[i+2] + t51$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t51$timeacc[i] < onset$animal[51]) {
    t51$timewindow[i] = 1
  } else if (t51$timeacc[i] > onset$animal[51] && t51$timeacc[i] < onset$object[51]) {
    t51$timewindow[i] = 2
  } else if (t51$timeacc[i] > onset$object[51]) {
    t51$timewindow[i] = 3
  } else {
  }
}

### t52
### time length
t52 <- Data[which(Data$TrialID==52),]
lengt <- length(t52$Condition)
for (j in 1:lengt) {
  t52$timelen[j] = t52$Recording_timestamp[j+1] - t52$Recording_timestamp[j]
}

### time accumulation
t52$timeacc[1] <- 0
t52$timeacc[2] = t52$timelen[1] + t52$timelen[2]
for (i in 1:lengt) {
  t52$timeacc[i+2] = t52$timelen[i+2] + t52$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t52$timeacc[i] < onset$animal[52]) {
    t52$timewindow[i] = 1
  } else if (t52$timeacc[i] > onset$animal[52] && t52$timeacc[i] < onset$object[52]) {
    t52$timewindow[i] = 2
  } else if (t52$timeacc[i] > onset$object[52]) {
    t52$timewindow[i] = 3
  } else {
  }
}


### t53
### time length
t53 <- Data[which(Data$TrialID==53),]
lengt <- length(t53$Condition)
for (j in 1:lengt) {
  t53$timelen[j] = t53$Recording_timestamp[j+1] - t53$Recording_timestamp[j]
}

### time accumulation
t53$timeacc[1] <- 0
t53$timeacc[2] = t53$timelen[1] + t53$timelen[2]
for (i in 1:lengt) {
  t53$timeacc[i+2] = t53$timelen[i+2] + t53$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t53$timeacc[i] < onset$animal[53]) {
    t53$timewindow[i] = 1
  } else if (t53$timeacc[i] > onset$animal[53] && t53$timeacc[i] < onset$object[53]) {
    t53$timewindow[i] = 2
  } else if (t53$timeacc[i] > onset$object[53]) {
    t53$timewindow[i] = 3
  } else {
  }
}


### t54
### time length
t54 <- Data[which(Data$TrialID==54),]
lengt <- length(t54$Condition)
for (j in 1:lengt) {
  t54$timelen[j] = t54$Recording_timestamp[j+1] - t54$Recording_timestamp[j]
}

### time accumulation
t54$timeacc[1] <- 0
t54$timeacc[2] = t54$timelen[1] + t54$timelen[2]
for (i in 1:lengt) {
  t54$timeacc[i+2] = t54$timelen[i+2] + t54$timeacc[i+1]
}

### time window
for (i in 1:lengt) {
  if (t54$timeacc[i] < onset$animal[54]) {
    t54$timewindow[i] = 1
  } else if (t54$timeacc[i] > onset$animal[54] && t54$timeacc[i] < onset$object[54]) {
    t54$timewindow[i] = 2
  } else if (t54$timeacc[i] > onset$object[54]) {
    t54$timewindow[i] = 3
  } else {
  }
}







# length = length(Data$TrialID)
# length = length - 2
# 
# subdata$timelen[78954] = subdata$Recording_timestamp[78954] - subdata$Recording_timestamp[78953]
# 
# subdata$timeacc[1] <- 0
# subdata$timeacc[2] = subdata$timelen[1] + subdata$timelen[2]
# 
# for (i in 1:length) {
#   subdata$timeacc[i+2] = subdata$timelen[i+2] + subdata$timeacc[i+1]
# }
# 
# subdata$timeacc[78954] = subdata$timelen[78954] + subdata$timeacc[78953]
# 
# 
# lengthtimacc = length(subdata$timeacc)
# 
# a <- subset(Data, TrialID==1,select=TrialID)
# b <- lengths(a)
# 
# for (j in 1:lengthtrial){
#   for (i in 1:length){
#    if (subdata$TrialID == j && subdata$timeacc[i] < onset$animal[j]){
#       subdata$timewindow[i] = 1
#    } else if (subdata$TrialID == j && subdata$timeacc[i] > onset$animal[j]){
#       subdata$timewindow[i] = 2
#    } else {
#       subdata$timewindow[i] = 3
#     }
#   }
# }
