#rm(list = ls())
library("plyr")
library("dplyr")
library("Hmisc")
library("ggplot2")
library("reshape2")
library("Rmisc")

filename <- "firstfix"
path <- getwd()

## load files
Inputdir <- paste(path,"/",filename,".csv",sep = "")
Data <- read.csv(Inputdir, header = TRUE,sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df1<- summarySE(Data, measurevar="fixpro", groupvars=c("cond","fixreg"))

## making plot
# plot1 <- ggplot(df, aes(x = cond, y = fixpro, group = fixreg)) + 
#   geom_line(aes(linetype = fixreg), position = position_dodge(width = 0.2)) +
#   geom_errorbar(aes(ymin = fixpro - se, ymax =fixpro + se),
#                 width = .1, position = position_dodge(width = 0.2), linetype = 1) +
#   geom_point(size = 4, position = position_dodge(width = 0.2)) +
#   geom_point(size = 3, position = position_dodge(width = 0.2), color = "white") +
#   guides(linetype = guide_legend("fixreg")) +
#   labs(title = paste("Mean Early Negativity effect depending on",
#                      "different task conditions",
#                      "(Error bars represent standard error)",
#                      sep = "\n"),
#        x = "Conditions",
#        y = "Average Early Negativity effect (Voltage) ")


## making plot
positions <- c("Inst", "Equi", "mod")

df <- subset(df1, fixreg =="ti+di" | fixreg == "ta+da") 
plot1 <- ggplot(data = df, 
                aes(x=cond,
                    y= fixpro, 
                    ymin=fixpro-se,
                    ymax=fixpro+se,
                    fill=fixreg)) +
  geom_bar(position="dodge", stat = "identity") + 
  geom_errorbar(position = position_dodge(), colour="black") +
  geom_point(position=position_dodge(0.9), aes(y=fixpro, colour=fixreg, width = 0.5))

# plot1 <- ggplot(df, aes(cond, fixpro)) +
#   geom_bar(aes(fill = fixreg), data = df, stat = "identity",position = position_dodge(1), width = 0.9)+
#   ylab("Proportion of trials with first click on the instrument")+
#   xlab(" ")+
#   scale_fill_grey()+
#   scale_x_discrete(limits = positions)+
#   ylim(0, 0.8)+
#   geom_errorbar(aes(ymin = fixpro-se, ymax = fixpro+se), data = df, width = 0.1, position = position_dodge(0.1))

## making plot
df2 <- subset(df1, fixreg =="ti" | fixreg == "ta" | fixreg == "da" | fixreg == "di") 
plot2 <- ggplot(data = df2, 
       aes(x=cond,
           y= fixpro, 
           ymin=fixpro-se,
           ymax=fixpro+se,
           fill=fixreg)) +
  geom_bar(position="dodge", stat = "identity") + 
  geom_errorbar(position = position_dodge(), colour="black") +
  geom_point(position=position_dodge(0.9), aes(y=fixpro, colour=fixreg, width = 0.1))
