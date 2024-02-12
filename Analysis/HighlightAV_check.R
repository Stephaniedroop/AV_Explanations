###############################################################
############## AV_Explanation #################################

# Script to check whether the manipulation of participant seeing 
# 'car' or 'self driving car' (in var 'HighlightAV') had effect
# lmer model found NO DIFFERENCE >>> THIS NOW SHOULD BE OUR MAIN LINE
# Following format of `anaysis.R` but separate script to keep them short

# load packages
library(tidyverse)
library(lme4)

rm(list = ls())

# import the pre-processed data
d <- read.csv('../Analysis/preProcessedData.csv') # 4963 obs of 25 vars

# As in `analysis.R`
# since SufficientDetail, Satisfyingness and Completeness are highly correlated
# with each other (rs > .8), we create a composite averaged 'Quality' variable
d <- d %>% mutate(Quality = (SufficientDetail + Satisfying + Complete)/3) # 4963 of 26 vars

# Set type as a factor
d$type <- as.factor(d$type)

# To see if car type (AV or car) manipulation has effect on quality
mqual <- lmer(Quality ~ type * HighlightAV + 
            (1|SID)+(1|EID)+(1|PID), 
          data = d)
summary(mqual) # Doesn't look like it, but could still check syntax and whether we need all the random effects

# But the real test is whether the car type manipulation affects how likely people are to cite teleological explanations
mtel <- lmer(Teleological ~ type * HighlightAV + 
               (1|SID)+(1|EID)+(1|PID), 
             data = d)
summary(mtel)

# --------------------- Data visualisation -----------------------------
# Note this section just to inspect the effect on Quality of the car type manipulation
# Now we diverge from `analysis.R` by splitting the dataset in two to reproduce 
# the same figs QualitybyExp and QualitybyExpScer but for each group separately

dcar <- d %>% filter(HighlightAV=='car') # 1560 obs of 26 vars
dav <- d %>% filter(HighlightAV=='self-driving car') # 3403 obs of 26 vars

#-------------- Now summarise as per analysis.r for each df separately----------------

# dcompexp for dcar - 297 obs of 16 vars
dcarcompexp <- dcar %>% group_by(EID, type, SID, explanation, scenarioName,
                           scenarioDescription) %>% 
  summarize(Satisfying=mean(Satisfying),
            SufficientDetail=mean(SufficientDetail),
            Complete=mean(Complete),
            Trust=mean(Trust),
            Quality=mean(Quality),
            Teleological=mean(Teleological), 
            MechanisticAgent=mean(MechanisticAgent),
            MechanisticLayout=mean(MechanisticLayout),
            ExplanationNeed=mean(ExplanationNeed),
            NumCauses=mean(NumCauses))

# dcompexp for dav - 650 obs of 16 vars
davcompexp <- dav %>% group_by(EID, type, SID, explanation, scenarioName,
                                 scenarioDescription) %>% 
  summarize(Satisfying=mean(Satisfying),
            SufficientDetail=mean(SufficientDetail),
            Complete=mean(Complete),
            Trust=mean(Trust),
            Quality=mean(Quality),
            Teleological=mean(Teleological), 
            MechanisticAgent=mean(MechanisticAgent),
            MechanisticLayout=mean(MechanisticLayout),
            ExplanationNeed=mean(ExplanationNeed),
            NumCauses=mean(NumCauses))

#---------- Chart each separately using same format as done for aggregate in `analysis.r` -----------

# a graph showing individual explanation quality as a function of 
# manipulated type (we add a bit of vertical jitter to avoid over-plotting)

Pdcar <- dcarcompexp %>% ggplot(aes(x=type, y=Quality))+
  geom_quasirandom(aes(y=Quality+rnorm(nrow(dcarcompexp), 0, .05), color=type),
                   cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Perceived Quality')+
  # facet_wrap(~SID)+
  theme_classic()+theme(legend.position = 'none',
                        axis.title=element_text(size=16),
                        axis.text=element_text(size=13))

Pdcar

Pdav <- davcompexp %>% ggplot(aes(x=type, y=Quality))+
  geom_quasirandom(aes(y=Quality+rnorm(nrow(davcompexp), 0, .05), color=type),
                   cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Perceived Quality')+
  # facet_wrap(~SID)+
  theme_classic()+theme(legend.position = 'none',
                        axis.title=element_text(size=16),
                        axis.text=element_text(size=13))
Pdav

# The two plots look almost identical, with same pattern and values 
# (although there are fewer datapoints for the car than the av)
