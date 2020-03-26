library(lme4)
library(lmerTest)
library(optimx)
library(Rmisc)
library(ggsignif)
library(ggplot2)
Data <- read.csv("play_mouseclick.csv")

my.helmert = matrix(c(1/3, -2/3, 1/3, -1/2, 0, 1/2), ncol = 2)
contrasts(Data$cond) = my.helmert

DataIn <- subset(Data, click!="ta")
DataAn <- subset(Data, click!="ti")

## Target instrument region
resultIn <- lmer(mean ~ 1 + cond + (1|subject), data = DataIn, control = lmerControl(optimizer= "optimx", optCtrl=list(method="nlminb")))
summary(resultIn)

## Target animal region
resultAn <- lmer(mean ~ 1 + cond + (1|subject), data = DataAn, control = lmerControl(optimizer= "optimx", optCtrl=list(method="nlminb")))
summary(resultAn)






# ## Target instrument region
# ti_result <- glmer(mean ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = Data)
# summary(ti_result)
# 
# 
# ## Target animal region
# ta_result <- glmer(mean ~ 1 + cond + (1 + cond|subject) + (1|trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = Data)
# summary(ta_result)
# 

#td2.lmer.ar1 <- glmer(tdlooks ~ 1 + tdlag1 + ambcode*refcode + (1+tdlag1+ambcode|Subnum) + (1+tdlag1+ambcode+refcode|Trial), family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),data = Data)
#summary(td2.lmer.ar1) # You will be reporting the fixed effects table in the following outputs in the manuscript.

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: tdlooks ~ 1 + tdlag1 + ambcode * refcode + (1 + tdlag1 + ambcode|Subnum) + (1 + tdlag1 + ambcode + refcode|Trial)
# Data: pigpen_td2_ar1
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 3285.5   3450.8  -1621.7   3243.5    19345 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -6.9670 -0.1114 -0.0995 -0.0865 12.5241 
# 
# Random effects:
#   Groups Name        Variance Std.Dev. Corr             
# Subnum (Intercept) 0.097826 0.31277                   
# tdlag1      0.012127 0.11012  -0.74            
# ambcode     0.080556 0.28382  -0.87  0.31      
# Trial  (Intercept) 0.066257 0.25741                   
# tdlag1      0.008421 0.09176  -1.00            
# ambcode     0.001364 0.03694  -1.00  1.00      
# refcode     0.008835 0.09399  -1.00  1.00  1.00
# Number of obs: 19366, groups:  Subnum, 38; Trial, 24
# 
# Fixed effects:
#                 Estimate   Std. Error z value Pr(>|z|)    
# (Intercept)      -4.4789     0.1075 -41.647   <2e-16 ***
#   tdlag1            7.3617     0.1227  59.974   <2e-16 ***
#   ambcode          -0.2962     0.1307  -2.266   0.0235 *  
#   refcode           0.2633     0.1494   1.763   0.0780 .  
#   ambcode:refcode  -0.2720     0.2452  -1.109   0.2674    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) tdlag1 ambcod refcod
# tdlag1      -0.583                     
# ambcode     -0.121  0.032              
# refcode     -0.100  0.031  0.007       
# ambcod:rfcd  0.024 -0.042 -0.065 -0.150