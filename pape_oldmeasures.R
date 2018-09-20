library(lme4)
library(arm)
library(ggplot2)
library(reshape)
library(lattice)
library(plyr)
library(car)

setwd("/Users/chael/Dropbox/Projects/dimensions")


dd = read.csv("pgdb_analysis.txt")

dd$participant=factor(as.numeric(unlist(regmatches(dd$speaker, gregexpr("[[:digit:]]+", dd$speaker)))))

dd$experiment=factor(unlist(lapply(strsplit(as.character(dd$speaker), "\\_"), "[", 1)))




suborn <- read.csv("suborn.csv")
suborn.responses <- read.csv("suborn_responses.txt",sep = "\t")
names(suborn.responses)[names(suborn.responses)=="item_original"] <- "itemOriginal"
data.suborn=merge(suborn,suborn.responses,all.x=TRUE,by=c("item","condition","participant"),suffixes=c("",".other"))

suborq <- read.csv("suborq.csv")
suborq$word[suborq$item==4&suborq$condition==4&suborq$word==3]=7
suborq.responses <- read.csv("suborq_responses.txt",sep = "\t")
names(suborq.responses)[names(suborq.responses)=="condition"] <- "itemOriginal"
names(suborq.responses)[names(suborq.responses)=="bracketing"] <- "condition"
names(suborq.responses)[names(suborq.responses)=="order"] <- "bracketing"
names(suborq.responses)[names(suborq.responses)=="order.1"] <- "order"
data.suborq=merge(suborq,suborq.responses,all.x=TRUE,by=c("item","condition","participant"),suffixes=c("",".other"))

data=rbind(data.suborn,data.suborq)

for (i in 1:6) {
  data[,i]<- factor(data[,i])
}

for (i in 7:22) {
  data[,i]<- as.numeric(as.character(data[,i]))
}

## woi Annotation (in this example: declarative; focus on woi13; phrasing not indicated here (was done with parentheses))
# I THOUGHT THEY SAID_1 MARION_2 OR_3 MARVIN_4 AND_5 SARAH_6 ARRIVED_7 BUT IN FACT THEY SAID THAT_8 MARION_9 OR_10 MARVIN_11 AND_12 NOLAN_13 ARRIVED_14

# Manipulation in the Experiment:
# 1: Intonation: Declarative vs. Interrogative (i.e., polar question)
# 2: Focus: Which word is focused? Wide focus on entire coordinate structure vs. First (=woi 9), Second (=woi 11), or Third (=woi12)
# 3: Constituency: Do first two conjuncts form a constituent or the second two conjuncts?

# Manipulation 1 (intonation) was bewteen participants; 2 (Focus) & 3 (Constituency) were within participants

# There were 4 item sets
length(unique(data$itemOriginal))
ddply(data.wide,.(itemOriginal,experiment,Constituency,Focus,Intonation),summarise,length(condition))


# There were 27 participants in the declarative and 31 in the interrogative experiment
ddply(data,.(experiment),summarise,length(unique(participant)))

# here we only look at the three NPs (Names):
data=data[data$word==9|data$word==11|data$word==13,]

# Coding of main predictors:

# Focus: Wide vs. First vs. Second vs. Third
data$Focus=factor(recode(data$condition,"'1'='Third';'2'='Second';'3'='First';'4'='Wide'"),levels=c('Wide','First','Second','Third'))
contrasts(data$Focus)=cbind("Wide.vs.Narrow"=c(3/4,-1/4,-1/4,-1/4),"First.vs.Late"=c(0,2/3,-1/3,-1/3),"Second.vs.Third"=c(0,0,1/2,-1/2))
data$Wide.vs.Narrow=model.matrix(~ Focus,data)[,2]
data$FocFirst.vs.Late=model.matrix(~ Focus,data)[,3]
data$FocSecond.vs.Third=model.matrix(~ Focus,data)[,4]

data$Intonation[data$experiment=="suborq"]="Interrogative"
data$Intonation[data$experiment=="suborn"]="Declarative"
data$Intonation=factor(data$Intonation)
contrasts(data$Intonation)=cbind("Decl.vs.Inter"=c(0.5,-0.5))
data$Decl.vs.Inter=model.matrix(~ Intonation,data)[,2]

data$Position=factor(recode(data$word,"'9'='A';'11'='B';'13'='C'"))
contrasts(data$Position)=cbind("First.vs.Late"=c(2/3,-1/3,-1/3),"Second.vs.Third"=c(0,0.5,-0.5))
data$PosFirst.vs.Late=model.matrix(~ Position,data)[,2]
data$PosSecond.vs.Third=model.matrix(~ Position,data)[,3]

data$Structure[data$bracketing=="l"]="(AB)C"
data$Structure[data$bracketing=="r"]="A(BC)"
data$Structure=factor(data$Structure)

data$Constituency[data$bracketing=="l"]="(AB)C"
data$Constituency[data$bracketing=="r"]="A(BC)"
data$Constituency=factor(data$Constituency)
contrasts(data$Constituency)=cbind("Left.vs.Right"=c(0.5,-0.5))
data$Left.vs.Right=model.matrix(~ Intonation,data)[,2]

data.long=data

# create a horizontal data frame with one line per utterance
#
data.wide=reshape(data.long,idvar=c("experiment", "participant","item","condition"),v.names=c("wordlabel", "Position","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity","PosFirst.vs.Late","PosSecond.vs.Third"),timevar="word",direction="wide")

# relative pitch measures
data.wide$rpitch1=12*log2(data.wide$thirdpitch.11/data.wide$thirdpitch.9)
data.wide$rpitch2=12*log2(data.wide$thirdpitch.13/data.wide$thirdpitch.11)

#
### Create Residuals for each dimension by fitting model which includes all the othe predictors except the one of interest
## (for plotting purposes)

# Residuals for focus

resid = data.long
data=data.long

for (i in 8:22) {
  col_count = i
  print(i)
  mod.lm = lmer(as.matrix(data[col_count])~
                  Constituency*Intonation*Position+phonelength+(1|participant)+(1|itemOriginal),data=data,na.action=na.exclude)
  mod_resid = resid(mod.lm)
  resid[col_count]=as.matrix(mod_resid)
}

resid.wide=reshape(resid,idvar=c("experiment", "participant","item","condition"),v.names=c("wordlabel", "Position","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

resid.wide$rpitch1=resid.wide$maxpitch.9-resid.wide$maxpitch.11
resid.wide$rpitch2=resid.wide$maxpitch.11-resid.wide$maxpitch.13

focus.long=resid
focus.wide=resid.wide

#  Residuals for bracketing

data=data.long
data$word=as.numeric(as.character(data$word))
data=data[data$word>7,]
data$word=factor(data$word)
resid = data

for (i in 8:22) {
  col_count = i
  print(i)
  mod.lm = lmer(as.matrix(data[col_count])~
                  Focus*Intonation*Position+phonelength+
                  (1|participant)+(1|itemOriginal),
                data=data,na.action=na.exclude)
  mod_resid = resid(mod.lm)
  resid[col_count]=as.matrix(mod_resid)
}

resid.wide=reshape(resid,idvar=c("experiment", "participant","item","condition"),v.names=c("wordlabel","Position", "phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

resid.wide$rpitch1=resid.wide$maxpitch.9-resid.wide$maxpitch.11
resid.wide$rpitch2=resid.wide$maxpitch.11-resid.wide$maxpitch.13

brack.long=resid
brack.wide=resid.wide


# residuals for intonation

resid = data.long
data=data.long

for (i in 8:22) {
  col_count = i
  print(i)

  mod.lm = lmer(as.matrix(data[col_count])~
                  Focus*Constituency*Position+phonelength+
                  (1|participant)+(1|itemOriginal),
                data=data,na.action=na.exclude)
  mod_resid = resid(mod.lm)
  resid[col_count]=as.matrix(mod_resid)
}

resid.wide=reshape(resid,idvar=c("experiment", "participant","item","condition"),v.names=c("wordlabel", "Position","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

resid.wide$rpitch1=resid.wide$maxpitch.9-resid.wide$maxpitch.11
resid.wide$rpitch2=resid.wide$maxpitch.11-resid.wide$maxpitch.13

intonation.long=resid
intononation.wide=resid.wide

# residuals for position

resid = data.long
data=data.long

for (i in 8:22) {
  col_count = i
  print(i)

  mod.lm = lmer(as.matrix(data[col_count])~
                  Focus*Constituency*Intonation+phonelength+
                  (1|participant)+(1|itemOriginal),
                data=data,na.action=na.exclude)
  mod_resid = resid(mod.lm)
  resid[col_count]=as.matrix(mod_resid)
}

resid.wide=reshape(resid,idvar=c("experiment", "participant","item","condition"),v.names=c("wordlabel", "Position","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

resid.wide$rpitch1=resid.wide$maxpitch.9-resid.wide$maxpitch.11
resid.wide$rpitch2=resid.wide$maxpitch.11-resid.wide$maxpitch.13

position.long=resid
position.wide=resid.wide




##
## Plots and Analysis: Constituency
##

## for unresidualized plot:
# subset=data.long


# Pitch (thirdpitch seems to work best for phrasing cues--but this choicepoint should be motivated/understood better)
# Plots with lines to evoke that these are very stylized pitch tracks

# Raw pitch
ggplot(data.long, aes(x=Position, y=thirdpitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Raw pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed")
# ggsave(file='plotsDec2016/PitchPhrasingRaw.pdf',height=4)

# Pitch residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=thirdpitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed")
# ggsave(file='plotsDec2016/PitchPhrasingResidualized.pdf',height=4)


#Pitch residualized for predictors other than intonation
ggplot(intonation.long, aes(x=Position, y=thirdpitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Pitch residualized for predictors other that intonation') + facet_grid(Intonation ~ Constituency, scales = "fixed")
# ggsave(file='plotsDec2016/PitchIntonationResidualized.pdf')

# Pitch residualized for predictors other than focus
ggplot(focus.long, aes(x=Position, y=thirdpitch,shape=Intonation))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Intonation)) + ylab('Pitch residualized for predictors other than focus') + facet_grid(Focus ~ Constituency, scales = "fixed")
# ggsave(file='plotsDec2016/PitchFocusResidualized.pdf')

#Pitch residualized for predictors other than position
ggplot(position.long, aes(x=Position, y=thirdpitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Pitch for predictors other than position') + facet_grid(Intonation ~ Constituency, scales = "fixed")
# ggsave(file='plotsDec2016/PitchPositionResidualized.pdf')


## Model:
# Does pitch cue phrasing?
# (if it does so uniformly across foci and intonations, this would be strong argument for overlay theory)

# First a general model, checking whether pitch of woi11 contributes to detecting phrasing in raw data,
# with effects of Intonation and Focus also part of the model
#
pitchModel=glmer(Constituency~
                   scale(thirdpitch.11)*scale(Decl.vs.Inter) +
                   scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third +
                   (scale(thirdpitch.11)*scale(Decl.vs.Inter) +
                      scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third||participant) +
                   (scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third||itemOriginal)
                 ,family="binomial", data=data.wide)
summary(pitchModel)

# Does pitch encode phrasing in questions?
# testing based on subset,  controlling for the effects of Focus:
pitchModelQuestion=glmer(Constituency~
                           scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third +
                           (scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third||participant) +
                           (scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third||itemOriginal)
                         ,family="binomial", data=subset(data.wide,Intonation=="Interrogative"))
summary(pitchModelQuestion)


# Does pitch encode phrasing in the post-focal domain?
# Testing based on cases with focus on first word, pitch cue from second word:
pitchModelFocFirst=glmer(Constituency~
                           scale(thirdpitch.11)*scale(Decl.vs.Inter) +
                           (scale(thirdpitch.11)*scale(Decl.vs.Inter)||participant) +
                           (scale(thirdpitch.11)*scale(Decl.vs.Inter)||itemOriginal)
                         ,family="binomial", data=subset(data.wide,Focus=="First"))
summary(pitchModelFocFirst)




