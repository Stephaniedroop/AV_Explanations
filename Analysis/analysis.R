### Analysis

# load packages
library(tidyverse)
library(lme4)
library(ggbeeswarm)
library(ggcorrplot)
library(colorspace)
library(effectsize)
library(ggpubr)

# convenience function 
se <- function(vector){
  return(sd(vector)/sqrt(length(vector)))
}


#### Load data and do some more pre-processing---------------------

# import the pre-processed data
d <- read.csv('Analysis/preProcessedData.csv')

# put data in long format
dlong <- d %>% gather(key='perceivedType', value='value',
                      Teleological, MechanisticAgent, MechanisticLayout)

# since SufficientDetail, Satisfyingness and Completeness are highly correlated
# with each other (rs > .8), we create a composite 'Quality' variable
# that averages them
d <- d %>% mutate(Quality = (SufficientDetail + Satisfying + Complete)/3)

# create a compressed dataframe with the mean rating for 
# each explanation
dcompexp <- d %>% group_by(EID, type, SID, explanation, scenarioName,
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


#### Manipulation check and correlation matrix------------------------


# manipulation check: are Teleological explanations perceived as containing
# teleological content?

# first fit a model including the effect of explanation type
f <- lmer(Teleological ~ relevel(factor(type), ref='Teleological') +
               (1|SID)+(1|EID)+(1|PID),
             data=d)

# compare this to a null model without explanation type
n <- lmer(Teleological ~ (1|SID)+(1|EID)+(1|PID),
          data=d)

anova(n, f)

# are Teleological explanations perceived as containing different level of 
# detail about traffic and road layout?
f <- lmer(MechanisticLayout ~ relevel(factor(type), ref='Teleological') + 
               (1|SID)+(1|EID)+(1|PID),
             data=d)

n <- lmer(MechanisticLayout ~ (1|SID)+(1|EID)+(1|PID),
          data=d)

anova(n,f)

# are Teleological explanations perceived as containing different level of
# detail about other the actions of other agents?
f <- lmer(MechanisticAgent ~ relevel(factor(type), ref='Teleological') + 
               (1|SID)+(1|EID)+(1|PID),
             data=d)

n <- lmer(MechanisticAgent ~  (1|SID)+(1|EID)+(1|PID),
          data=d)

anova(n,f)

## compute correlation matrix (a lot of the code here is borrowed from
## https://osf.io/wyk2j; Sulik, J., van Paridon, J., & Lupyan, G. (2023). 
## Explanations in the wild. Cognition, 237, 105464.)

# create correlation matrix
rdf <- d %>% select(Satisfying, SufficientDetail, Complete, Trust, 
                    Teleological, MechanisticAgent, MechanisticLayout,
                    ExplanationNeed, NumCauses) %>% as.matrix %>% rcorr

# plot the matrix
ggcorrplot(rdf$r,
           #method="circle",
           type = "upper",outline.color = "white",lab_size = 5,
           lab = TRUE,ggtheme = theme_bw,pch = 1, pch.col = "#FB9D0B",
           pch.cex = 12,p.mat = rdf$P*36,show.legend = FALSE) +
  scale_fill_continuous_sequential(palette="sunset", rev=F)

#ggsave('correlationMatrix.png', dpi=600, width=6, height=5)


#### effect of experimental manipulation-----------------------

# does explanation type impact perceived quality?
q0fullModel <- lmer(Quality ~ type+(1+type|SID)+
                   (1|EID)+ (1|PID), 
                  data=d)
summary(q0fullModel)

# extract standardized parameters
sq0 <- standardize_parameters(q0fullModel, method = "pseudo",
                              ci_method = "satterthwaite")

# null model
q0nullModel <- lmer(Quality ~ type+(1|SID)+
                   (1|EID)+ (1|PID),
                  data=d)

summary(q0nullModel)

# compare null and full model to extract a p-value
anova(q0nullModel, q0fullModel)

## same analysis for Trustworthiness
t0fullModel <- lmer(Trust ~ type+(1+type|SID)+
                  (1|EID) + (1|PID), 
                  data=d)
summary(t0fullModel)

t0nullModel <- lmer(Trust ~ type+(1|SID)+
                  (1|EID) + (1|PID),
                  data=d)

anova(t0nullModel, t0fullModel)

st0 <- standardize_parameters(t0fullModel, method = "pseudo", 
                              ci_method = "satterthwaite")


#### effect of perceived features---------------------------------

# run linear mixed model with lots of features
qModel <- lmer(Quality ~ Teleological + 
               MechanisticAgent + 
               MechanisticLayout+
               ExplanationNeed+
               NumCauses+
               type+(1|SID)+
               (1|PID)+
                 (1|EID), 
             data=d)

summary(qModel)

# extract standardized parameters 
# (see https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html)
s <- standardize_parameters(qModel, method = "pseudo", 
                            ci_method = "satterthwaite")

# extract the parameters so we can plot them

sdf <- data.frame(s[[1]], s[[2]], s[[4]], s[[5]])
colnames(sdf) <- c('predictor', 'coefficient', 'CI_lower', 'CI_upper')

# create figure
coefplot1 <- sdf[2:nrow(sdf),] %>% ggplot(
  aes(x=predictor, y=coefficient)
)+
  geom_point()+
  geom_pointrange(aes(ymin=CI_lower,
                    ymax=CI_upper))+
  xlab('Predictor')+ylab('Standardized coefficient')+
  ggtitle('Quality')+
  geom_hline(aes(yintercept=0), linetype='dashed', color='red')+
  coord_flip(ylim=c(-.05,.3))+
  theme_classic()+theme(axis.title=element_text(size=16),
                        axis.text=element_text(size=13),
                        axis.title.x=element_blank())

## same analysis for trustworthiness

# linear mixed model
tModel <- lmer(Trust ~ Teleological + 
               MechanisticAgent + 
               MechanisticLayout+
               ExplanationNeed+
               NumCauses+
               type+(1|SID)+
               (1|PID)+(1|EID), 
             data=d)

summary(tModel)

# extract standardized parameters
s2 <- standardize_parameters(tModel, method = "pseudo", 
                             ci_method = "satterthwaite")

# extract the parameters so we can plot them
 sdf2 <- data.frame(s2[[1]], s2[[2]], s2[[4]], s2[[5]])
colnames(sdf2) <- c('predictor', 'coefficient', 'CI_lower', 'CI_upper')

# create figure
coefplot2 <- sdf2[2:nrow(sdf2),] %>% ggplot(
  aes(x=predictor, y=coefficient)
)+
  geom_point()+
  geom_pointrange(aes(ymin=CI_lower,
                      ymax=CI_upper))+
  geom_hline(aes(yintercept=0), linetype='dashed', color='red')+
  xlab('Predictor')+ylab('Standardized coefficient')+
  coord_flip(ylim=c(-.05, .3))+
  ggtitle('Trustworthiness')+
  theme_classic()+theme(axis.title=element_text(size=16),
                        axis.text=element_text(size=13),
                        axis.text.y=element_blank(),
                        axis.title.y=element_blank(),
                        axis.title.x=element_blank())
coefplot2

# combine these two plots

coefCombined  <- ggarrange(coefplot1, coefplot2, widths=c(.85,.5))
coefCombined
annotate_figure(coefCombined,
                bottom=text_grob('Standardized coefficient', size=16))

#ggsave('predictorsCombined.png', width=7, height=6)


## test whether Teleology is 'significantly' better than other predictors

# a model without Teleology
qNoT <- lmer(Quality ~  MechanisticAgent + 
               MechanisticLayout+
               ExplanationNeed+
               NumCauses+
               type+(1|SID)+
               (1|PID)+
               (1|EID), 
             data=d)

# a model without MechanisticAgent
qNoMA <- lmer(Quality ~ Teleological + 
                MechanisticLayout+
                ExplanationNeed+
                NumCauses+
                type+(1|SID)+
                (1|PID)+
                (1|EID), 
              data=d)

# a model without MechanisticLayout
qNoML <- lmer(Quality ~ Teleological + 
                MechanisticAgent + 
                ExplanationNeed+
                NumCauses+
                type+(1|SID)+
                (1|PID)+
                (1|EID), 
              data=d)

# compute AICs
AIC(qNoT)
AIC(qNoMA)
AIC(qNoML)


## same approach for Trustworthiness
# a model without Teleology
qNoT2 <- lmer(Trust ~  MechanisticAgent + 
               MechanisticLayout+
               ExplanationNeed+
               NumCauses+
               type+(1|SID)+
               (1|PID)+
               (1|EID), 
             data=d)

# a model without MechanisticAgent
qNoMA2 <- lmer(Trust ~ Teleological + 
                MechanisticLayout+
                ExplanationNeed+
                NumCauses+
                type+(1|SID)+
                (1|PID)+
                (1|EID), 
              data=d)

# a model without MechanisticLayout
qNoML2 <- lmer(Trust ~ Teleological + 
                MechanisticAgent + 
                ExplanationNeed+
                NumCauses+
                type+(1|SID)+
                (1|PID)+
                (1|EID), 
              data=d)

# compute AICs
AIC(qNoT2)
AIC(qNoMA2)
AIC(qNoML2)



# does the slope of Teleological vary as a function of scenario?

# model with Teleological
f <- lmer(Quality ~ Teleological + (1+Teleological|type) + (1|SID) + (1|EID)+
            (1|PID),
          data=d)

# same model without Teleological
n <- lmer(Quality ~ Teleological + (1|type) + (1|SID) + (1|EID)+
            (1|PID),
          data=d)

summary(f)
summary(n)
anova(n,f)
# no (p=.62)

# same analysis for Trustworthiness
f <- lmer(Trust ~ Teleological + (1+Teleological|type) + (1|SID) + (1|EID)+
            (1|PID),
          data=d)

n <- lmer(Trust ~ Teleological + (1|type) + (1|SID) + (1|EID)+
            (1|PID),
          data=d)

summary(f)
summary(n)
anova(n,f)




#### graphs------------------------------

# manipulation check:
# does perceived explanation type vary as a function of manipulated 
# explanation type?
dlong %>% ggplot(aes(x=perceivedType, y=value, fill=type))+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  xlab('type of explanation probed')+
  ylab('perceived completeness along that type')+
  facet_wrap(~type)+theme_classic()+
  theme(legend.position='none',
        axis.text.x=element_text(angle=90))

#ggsave('manipulationCheck.png', dpi=600, width=6, height=4.5)



# a graph showing individual explanation quality as a function of 
# manipulated type (we add a bit of vertical jitter to avoid over-plotting)
dcompexp %>% ggplot(aes(x=type, y=Quality))+
  geom_quasirandom(aes(y=Quality+rnorm(nrow(dcompexp), 0, .05), color=type),
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

# uncomment line below to save graph on your hard drive
#ggsave('qualityByExp.png', dpi=600, width=6, height=4)

# same graph for Trust
dcompexp %>% ggplot(aes(x=type, y=Trust))+
  geom_quasirandom(aes(y=Trust+rnorm(nrow(dcompexp), 0, .05), color=type),
                   cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Perceived Trustworthiness')+
  # facet_wrap(~SID)+
  theme_classic()+theme(legend.position = 'none')

## breaking down by scenario and exp

# quality
dcompexp %>% ggplot(aes(x=type, y=Quality))+
  geom_quasirandom(aes(y=Quality+rnorm(nrow(dcompexp), 0, .05), color=type),
                   cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Perceived Quality')+
  facet_wrap(~SID, nrow=2)+
  scale_color_discrete(name='Explanation Type')+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  theme_classic()+theme(legend.position='top',
                        axis.text.x=element_blank(),
                        axis.title.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.title=element_text(size=16),
                        axis.text=element_text(size=13),
                        legend.text=element_text(size=13),
                        legend.title=element_text(size=16))

#ggsave('qualityByExpScer.png', dpi=600, width=7, height=6)


# same graph for trust
dcompexp %>% ggplot(aes(x=type, y=Trust))+
  geom_quasirandom(aes(y=Trust+rnorm(nrow(dcompexp), 0, .05), color=type),
                   cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Trustworthiness')+
  facet_wrap(~SID, nrow=2)+
  scale_color_discrete(name='Explanation Type')+
  theme_classic()+theme(legend.position='top',
                        axis.text.x=element_blank(),
                        axis.title.x=element_blank(),
                        axis.ticks.x=element_blank())



# look at the effect of perceived teleology on
# quality
dcompexp %>% ggplot(aes(x=Teleological, y=Quality))+
  geom_jitter(alpha=.1)+
  geom_smooth(method='lm', aes(color=type))+
  facet_wrap(~type)+
  theme_classic()

# look at the effect of perceived agent-focused mechanism on 
# quality
dcompexp %>% ggplot(aes(x=MechanisticAgent, y=Quality))+
  geom_jitter(alpha=.1)+
  geom_smooth(method='lm', aes(color=type))+
  facet_wrap(~type)+
  theme_classic()

# look at the effect of perceived layout-focused mechanism on 
# quality
dcompexp %>% ggplot(aes(x=MechanisticLayout, y=Quality))+
  geom_jitter(alpha=.1)+
  geom_smooth(method='lm', aes(color=type))+
  facet_wrap(~type)+
  theme_classic()
  

# the impact of perceived teleology across scenarios
dcompexp %>% 
  ggplot(aes(x=Teleological, y=Quality))+
  geom_jitter(alpha=.1)+
  geom_smooth(method='lm', aes(color=type))+
  facet_wrap(~SID)+coord_cartesian(xlim=c(1,5),
                                   ylim=c(1,5))+
  xlab('Perceived teleology')+ylab('Quality')+
  theme_classic()+theme(legend.position='top')

# ggsave('perceivedTeleology.png', dpi=600, width=6,height=5)



#### Exploratory demographic analyses-----------------------

# does the annotator's gender impact explanation need?
m <- lmer(ExplanationNeed ~ GenderAnnotator + (1|EID) + (1|SID),
           data=d)
summary(m)


d %>% filter(GenderAnnotator=='Male'|
               GenderAnnotator=='Female') %>% 
  ggplot(aes(x=factor(SID), y=ExplanationNeed,
             color=GenderAnnotator))+
  stat_summary(fun='mean', geom='point')+
  stat_summary(fun='mean', geom='line', aes(group=GenderAnnotator))+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  theme_classic()


summary(lmer(Quality ~ GenderAnnotator + (1|EID) + (1|SID),
             data=d))


# does the Annotator's gender impact perceived quality?
d %>% filter(GenderAnnotator=='Male'|
               GenderAnnotator=='Female') %>% 
  ggplot(aes(x=factor(SID), y=Quality,
             color=GenderAnnotator))+
  stat_summary(fun='mean', geom='point')+
  stat_summary(fun='mean', geom='line', aes(group=GenderAnnotator))+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  theme_classic()

# --> apparently, male evaluators tend to find that situations are more in need
# of explanation, and also find find explanations more satisfying

# analyses targeting the explainer's gender (not reported here) find null 
# results



### getting a more qualitative feel for the explanations------------------

# print the best explanation for each scenario
# (broken down by explanation type)
for (i in 1:14){
  sub <- dcompexp %>% filter(type=='Teleological', SID==i)
  index <- which.max(sub$Quality)
  print(i)
  print(sub[index,]$Quality)
  print(sub[index,]$explanation)
}

for (i in 1:14){
  sub <- dcompexp %>% filter(type=='Mechanistic', SID==i)
  index <- which.max(sub$Quality)
  print(i)
  print(sub[index,]$Quality)
  print(sub[index,]$explanation)
}

for (i in 1:14){
  sub <- dcompexp %>% filter(type=='Counterfactual', SID==i)
  index <- which.max(sub$Quality)
  print(i)
  print(sub[index,]$Quality)
  print(sub[index,]$explanation)
}


# print the worst explanation for each scenario
# (broken down by explanation type)
for (i in 1:14){
  sub <- dcompexp %>% filter(type=='Teleological', SID==i)
  index <- which.min(sub$Quality)
  print(sub[index,]$Quality)
  print(sub[index,]$explanation)
}

for (i in 1:14){
  sub <- dcompexp %>% filter(type=='Mechanistic', SID==i)
  index <- which.min(sub$Quality)
  print(sub[index,]$Quality)
  print(sub[index,]$explanation)
}

for (i in 1:14){
  sub <- dcompexp %>% filter(type=='Counterfactual', SID==i)
  index <- which.min(sub$Quality)
  print(sub[index,]$Quality)
  print(sub[index,]$explanation)
}

