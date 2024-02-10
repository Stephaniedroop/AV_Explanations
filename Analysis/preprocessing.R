## This script is used to put the data in a convenient
## format for subsequent analyses

## the goal is to create one big dataframe (that we
## will call 'd') that contains all the relevant data

# import packages
library(tidyverse)
library(jsonlite)


### 1) import explanations----------------------------

# import data (this creates a List, that we then must
# convert into a dataframe)
expList <- fromJSON('explanations.json', flatten=FALSE)

# initialize the dataframe
exp <- data.frame(expList[[1]])
# (EID is the ID for a given explanation)
exp$EID <- names(expList)[1]

# fill in the dataframe
for (i in 2:length(expList)){
  expexp <- data.frame(expList[[i]])
  expexp$EID <- names(expList)[i]
  exp <- rbind(exp, expexp)
}




### 2) import participants' annotations -----------------
### i.e. from the second set of participants, who
### rated the explanations created by the first set
### of participants

# import dataset
dList <- fromJSON('explanation_annotations.json', flatten=TRUE)

# initialize the dataframe (this dataframe will be our main one)
d <- data.frame(dList[[1]][[1]])
for(i in 2:length(dList[[1]])){
  d <- rbind(d, data.frame(dList[[1]][[i]]))
}
# initialize the columns that will contains the 
# IDs for explanations and scenarios
d$EID <- names(dList)[1]
d$PID <- names(dList[[1]])

# fill in the dataframe
for (a in 2:length(dList)){
  dd <- data.frame(dList[[a]][[1]])
  for(i in 2:length(dList[[a]])){
    dd <- rbind(dd, data.frame(dList[[a]][[i]]))
  }
  
  dd$EID <- names(dList)[a]
  dd$PID <- names(dList[[a]])
  d <- rbind(d,dd)
}


## import explanation type into the main dataframe
## (we look for that information in the explanations
## dataframe)

getType <- function(id){
  sub <- exp %>% filter(EID==id)
  return(sub$QuestionType)
}

d <- d %>% mutate(type=unlist(
  pmap(
    list(EID),
    getType
  )
))

# import scenario into the main dataframe
# (this information is found in the explanations dataframe)
getScenario <- function(id){
  sub <- exp %>% filter(EID==id)
  return(sub$SID)
}

d <- d %>% mutate(SID=unlist(
  pmap(
    list(EID),
    getScenario
  )
))


## recode ratings

# first kind of likert scale
verbalRatings <- unique(d$Teleological)
numericRatings <- c(5,3,4,2,1)
which(verbalRatings == 'A lot')

replaceLikert <- function(rating){
  index <- which(verbalRatings==rating)
  return(numericRatings[index])
}

d <- d %>% mutate(Teleological = unlist(
  pmap(
    list(Teleological),
    replaceLikert
  )
),
MechanisticAgent = unlist(
  pmap(
    list(MechanisticAgent),
    replaceLikert
  )),
MechanisticLayout = unlist(
  pmap(
    list(MechanisticLayout),
    replaceLikert
  )
))

# second kind of Likert scale

verbalRatings2 <- unique(d$SufficientDetail)
numericRatings2 <- c(4,3,2,5,1)

replaceLikert2 <- function(rating){
  index <- which(verbalRatings2==rating)
  return(numericRatings2[index])
}

d <- d %>% mutate(
SufficientDetail = unlist(
  pmap(
    list(SufficientDetail),
    replaceLikert2
  )
),
Satisfying= unlist(
  pmap(
    list(Satisfying),
    replaceLikert2
  )
),
Complete = unlist(
  pmap(
    list(Complete),
    replaceLikert2
  )
),
Trust = unlist(
  pmap(
    list(Trust),
    replaceLikert2
  )
))



#### 3) extract scenario ratings---------------------

# import scenario data
sList <- fromJSON('scenario_annotations.json')
#View(sList)

# initialize dataframe
s <- data.frame(sList[[1]])
s$PID <- names(sList)[1]

# fill in dataframe
for (i in 2:length(sList)){
  ss <- data.frame(sList[[i]])
  ss$PID <- names(sList)[i]
  s <- rbind(s, ss)
}

# recode 'explanation need' variable as numeric

s <- s %>% mutate(ExplanationNeed= unlist(
  pmap(
    list(ExplanationNeed),
    replaceLikert
  )
))

# import explanation need into the main dataframe
getExplanationNeed <- function(participant_id, scenario_id){
  sub <- s %>% filter(SID==scenario_id,
                      PID==participant_id)
  return(sub$ExplanationNeed)
}

d <- d %>% mutate(ExplanationNeed=unlist(
  pmap(
    list(PID, SID),
    getExplanationNeed
  )
))

# import explanation curiosity into the main dataframe
getExplanationCuriosity <- function(participant_id, scenario_id){
  sub <- s %>% filter(SID==scenario_id,
                      PID==participant_id)
  return(sub$ExplanationCuriosity)
}

d <- d %>% mutate(ExplanationCuriosity=unlist(
  pmap(
    list(PID, SID),
    getExplanationCuriosity
  )
))


#### 4) import demographic information--------------

# for the annotators ('r' stands for 'raters')
rList <- fromJSON('demographics_annotators.json')
#View(rList)

r <- data.frame(rList[[1]])
r$PID <- names(rList)[1]

for (i in 2:length(rList)){
  rr <- data.frame(rList[[i]])
  rr$PID <- names(rList)[i]
  r <- rbind(r, rr)
}

# function to import a variable onto the main dataframe
getDemographic <- function(participant_id, variable){
  sub <- r %>% subset(PID==participant_id)
  return(sub[variable])
}
getDemographic('65428adcca88b655d14cf6db', 'Gender')

d <- d %>% mutate(GenderAnnotator = unlist(
  pmap(
    list(PID, 'Gender'),
    getDemographic
  )
),
AgeAnnotator = unlist(
  pmap(
    list(PID, 'AgeGroup'),
    getDemographic
  )
))

# for the explainers ('c' stands for 'creators')
cList <- fromJSON('demographics_explainers.json')
#View(cList)

c <- data.frame(cList[[1]])
c$PID <- names(cList)[1]

for (i in 2:length(cList)){
  cc <- data.frame(cList[[i]])
  cc$PID <- names(cList)[i]
  c <- rbind(c, cc)
}

# import these data into the main dataframe

# first we need to import the explainer's ID
getExplainerID <- function(explanation_id){
  sub <- exp %>% filter(EID==explanation_id)
  return(sub$PID)
}

d <- d %>% mutate(expPID = unlist(
  pmap(
    list(EID),
    getExplainerID
  )
))

# now we can use the explainer's ID to import the explainer's demographics
getDemographicExplainer <- function(participant_id, variable){
  sub <- c %>% subset(PID==participant_id)
  return(sub[variable])
}

d <- d %>% mutate(GenderExplainer=unlist(
  pmap(
    list(expPID, 'Gender'),
    getDemographicExplainer
  )
  ),
  AgeExplainer=unlist(
    pmap(
      list(expPID, 'AgeGroup'),
      getDemographicExplainer
    )
  )
)


#### 5) import the explanations-----------------------------

getExplanation <- function(explanation_id){
  sub <- exp %>% filter(EID==explanation_id)
  return(sub$Text)
}

d <- d %>% mutate(explanation = unlist(pmap(
  list(EID),
  getExplanation
)))


#### 6) import scenario data---------------------------------

sList2 <- fromJSON('metadata/scenarios_metadata.json')


s2 <- data.frame(sList2$scenarios)

getScenarioDescription <- function(scenario_id){
  sub <- s2 %>% filter(SID==scenario_id)
  return(sub$Description)
}

getScenarioName <- function(scenario_id){
  sub <- s2 %>% filter(SID==scenario_id)
  return(sub$Name)
}


d <- d %>% mutate(scenarioDescription=unlist(
  pmap(
    list(SID),
    getScenarioDescription
  )
),
scenarioName=unlist(
  pmap(
    list(SID),
    getScenarioName
  )
))

#### 6) export the dataframe--------------
#write.csv(d, 'preProcessedData.csv')
