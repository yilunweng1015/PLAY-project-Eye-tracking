library(Rmisc)
library(ggsignif)
library(ggplot2)
library(ggpubr)
Data <- read.csv("play_correlation_eeg_et.csv")
#eeg <- read.csv("play_EEG_raw.csv")
#look <- subset(Data, cond == "Instrument-biased" & look == "animal")
#corr <- cbind(eeg[1:2],look[3])

ggscatter(Data, x = "eeg_N2time", y = "ti_fix_N1time", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "EEG frontal positivity", ylab = "First look")


#write.csv(Data1,file = "Play_EEG_raw.csv",row.names=T)
# data$cond <- "NA"
# data$cond[grepl("^7$|^16$",data$bini)] <- "Equi-biased"
# data$cond[grepl("^9$|^18$",data$bini)] <- "Instrument-biased"
# 
# data$action <- "NA"
# data$action[grepl("^7$|^8$|^9$",data$bini)] <- "Target Animal"
# data$action[grepl("^16$|^17$|^18$",data$bini)] <- "Target Instrument"
#write.csv(corr,file = "Play_EEG_correlation.csv",row.names=T)

## Making QQ plot
library(Rmisc)
library(ggsignif)
library(ggplot2)
library(ggpubr)
Data <- read.csv("play_eye_fixation_proportion_plot.csv")
cols <- c("Instrument-biased" = "#999999", "Equi-biased" = "#E69F00", "Modifier-biased" = "#56B4E9")

plot1 <- qplot(y = mean, x = time, data = Data, size=I(3), color=Condition, shape=LookRegion)+
  labs(y = "Proportion of fixations", x = "Time relative to verb onset (0 to 4000ms)")+
  theme_bw()+
  ylim(0, 0.6)+
  scale_fill_discrete(name = c("Condition", "Click Region"))+
  scale_colour_manual(values = cols)+
  scale_x_continuous(breaks=seq(0,4000,400))

