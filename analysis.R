### Preliminary analyses

# load packages
library(tidyverse)
library(lme4)
library(ggbeeswarm)


# manipulation check:
# does perceived explanation type vary as a function of manipulated 
# explanation type?

# put data in long format
dlong <- d %>% gather(key='perceivedType', value='value',
                      Teleological, MechanisticAgent, MechanisticLayout)

# graph
dlong %>% ggplot(aes(x=perceivedType, y=value, fill=type))+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  xlab('type of explanation probed')+
  ylab('perceived completeness along that type')+
  facet_wrap(~type)+theme_classic()+
  theme(legend.position='none',
        axis.text.x=element_text(angle=90))

ggsave('manipulationCheck.png', dpi=600, width=6, height=4.5)

# Satisfyingness as a function of Manipulated explanation type
d %>% ggplot(aes(x=type, y=Satisfying, fill=type))+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  # facet_wrap(~SID)+
  theme_classic()


# Sufficient-Detail as a function of Manipulated explanation type
d %>% ggplot(aes(x=type, y=SufficientDetail))+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  # facet_wrap(~SID)+
  theme_classic()



# Completeness as a function of Manipulated explanation type
d %>% ggplot(aes(x=type, y=Complete))+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  # facet_wrap(~SID)+
  theme_classic()

# Trust as a function of Manipulated explanation type
d %>% ggplot(aes(x=type, y=Trust))+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  # facet_wrap(~SID)+
  theme_classic()


## showing individual explanations

# create a compressed dataframe with the mean rating for 
# each explanation
dcompexp <- d %>% group_by(EID) %>% 
  summarize(satisfying=mean(Satisfying), type=type, SID=SID, 
            Teleological=mean(Teleological), 
            ExplanationNeed=mean(ExplanationNeed))

# a graph showing individual explanation ratings as a function of 
# manipulated type (we add a bit of vertical jitter to avoid over-plotting)
dcompexp %>% ggplot(aes(x=type, y=satisfying+rnorm(nrow(dcompexp), 0, .05)))+
  geom_quasirandom(aes(color=type), cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Satisfyingness')+
  # facet_wrap(~SID)+
  theme_classic()+theme(legend.position = 'none')

# uncomment line below to save graph on your hard drive
# ggsave('satisfyingnessByExp.png', dpi=600, width=6, height=4)

# breaking down by scenario and exp
dcompexp %>% ggplot(aes(x=type, y=satisfying+rnorm(nrow(dcompexp), 0, .05)))+
  geom_quasirandom(aes(color=type), cex=.7)+
  stat_summary(fun='mean', geom='line', aes(group=1))+
  stat_summary(fun='mean', geom='point', pch=21, size=2, fill='black')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation Type')+ylab('Satisfyingness')+
  facet_wrap(~SID, nrow=1)+
  scale_color_discrete(name='Explanation Type')+
  theme_classic()+theme(legend.position='top',
                        axis.text.x=element_blank(),
                        axis.title.x=element_blank(),
                        axis.ticks.x=element_blank())

# ggsave('satisfyingnessByExpScer.png', dpi=600, width=12, height=4)

# Breaking down by scenario

se <- function(vector){
  return(sd(vector)/sqrt(length(vector)))
}

dcompscer <- d %>% group_by(SID, type) %>% 
  summarize(satisfying=mean(Satisfying), se.satisfying=se(Satisfying))

pj <- position_jitter(width=.15, seed=123)
dcompscer %>% ggplot(aes(x=type, y=satisfying))+
  # stat_summary(fun='mean', geom='point')+
  # stat_summary(fun.data='mean_se', geom='errorbar', width=.1)+
  geom_errorbar(aes(
    ymin=satisfying-se.satisfying,
    ymax=satisfying+se.satisfying
  ),width=.02, position=pj)+
  geom_point(pch=21, aes(fill=type), position=pj)+
  geom_path(aes(group=SID), position=pj)+
  coord_cartesian(ylim=c(1,5))+
  xlab('Explanation type')+ylab('Satisfyingness')+
  theme_classic()+theme(legend.position='none')

# ggsave('satisfyingnessByScer.png', dpi=600, width=4, height=4)


# predicting satisfyingness with a big regression
# (note: I'm still not completely sure that this is the right
# way to set up this mixed model)
summary(lmer(Satisfying ~ Teleological + 
             MechanisticAgent + 
             MechanisticLayout+
               ExplanationNeed+
               NumCauses+
             type+(1+type|SID)+
               (1|PID), 
   data=d))


# look at the effect of perceived teleology on
# satisfyingness
d %>% ggplot(aes(x=Teleological, y=Satisfying))+
  geom_jitter(alpha=.1)+
  geom_smooth(method='lm', aes(color=type))+
  facet_wrap(~type)+
  theme_classic()
  

# does satisfyingness vary a lot between scenario?

d %>% ggplot(aes(x=factor(SID), y=Satisfying))+
  geom_jitter(alpha=.1)+
  stat_summary(fun='mean', geom='col')+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  theme_classic()


# the impact of perceived teleology across scenarios
dcompexp %>% 
  ggplot(aes(x=Teleological, y=satisfying))+
  geom_jitter(alpha=.1)+
  geom_smooth(method='lm', aes(color=type))+
  facet_wrap(~SID)+coord_cartesian(xlim=c(1,5),
                                   ylim=c(1,5))+
  xlab('Perceived teleology')+ylab('Satisfyingness')+
  theme_classic()+theme(legend.position='top')

# ggsave('perceivedTeleology.png', dpi=600, width=6,height=5)


# does the annotator's gender impact explanation need?
summary(lm(ExplanationNeed ~ GenderAnnotator,
           data=d))

# does the Annotator's gender impact satisfyingness?
d %>% filter(GenderAnnotator=='Male'|
               GenderAnnotator=='Female') %>% 
  ggplot(aes(x=factor(SID), y=Satisfying,
             color=GenderAnnotator))+
  stat_summary(fun='mean', geom='point')+
  stat_summary(fun='mean', geom='line', aes(group=GenderAnnotator))+
  stat_summary(fun.data='mean_se', geom='errorbar', width=.5)+
  coord_cartesian(ylim=c(1,5))+
  theme_classic()

cor.test(d$ExplanationNeed, d$Satisfying)
