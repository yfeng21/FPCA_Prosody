library(lme4)
library(arm)
library(ggplot2)
library(reshape)
library(lattice)
library(plyr)
library(car)
library(sjPlot)

setwd("/Users/chael/Dropbox/Projects/dimensions/2_oldStats")


# load acoustic measures
dd = read.csv("pgdb_analysis.txt")
#
dd$participant=factor(as.numeric(unlist(regmatches(dd$speaker, gregexpr("[[:digit:]]+", dd$speaker)))))
dd$experiment=factor(unlist(lapply(strsplit(as.character(dd$speaker), "\\_"), "[", 1)))
dd$recordedFile=paste0(dd$discourse,'.wav')

# load response files
# declarative experiment:
suborn.responses <- read.csv("suborn_responses.txt",sep = "\t")
names(suborn.responses)[names(suborn.responses)=="item_original"] <- "itemOriginal"
# question expeirment:
suborq.responses <- read.csv("suborq_responses.txt",sep = "\t")
names(suborq.responses)[names(suborq.responses)=="condition"] <- "itemOriginal"
names(suborq.responses)[names(suborq.responses)=="bracketing"] <- "condition"
names(suborq.responses)[names(suborq.responses)=="order"] <- "bracketing"
names(suborq.responses)[names(suborq.responses)=="order.1"] <- "order"
# combine
subor.responses=rbind(suborq.responses,suborn.responses)

# merge acoustics with responses file
dd=merge(dd,subor.responses,all.x=TRUE,by=c('recordedFile'),suffixes=c("",".other"))

numericCols=c("duration","relative_duration","Mean_F0", "Mean_F0_relative", "Mean_Intensity", "Mean_Intensity_relative")

nColumns = ncol(dd)
# convert to numeric column, otherwise treat as factor:
for (i in 1:nColumns) {
    if (colnames(dd)[i] %in% numericCols) {
        dd[, i] <- as.numeric(as.character(dd[, i]))
    } else {
        dd[, i] <- as.factor(as.character(dd[, i]))
    }
}

ddply(dd,.(experiment,condition,element_number),summarise,length(condition))
ddply(dd,.(experiment),summarise,length(unique(participant)))

dd$Focus=factor(recode(dd$condition,"'1'='Third';'2'='Second';'3'='First';'4'='Wide'"),levels=c('Wide','First','Second','Third'))
contrasts(dd$Focus)=cbind("Wide.vs.Narrow"=c(3/4,-1/4,-1/4,-1/4),"First.vs.Late"=c(0,2/3,-1/3,-1/3),"Second.vs.Third"=c(0,0,1/2,-1/2))
dd$FocWide.vs.Narrow=model.matrix(~ Focus,dd)[,2]
dd$FocFirst.vs.Late=model.matrix(~ Focus,dd)[,3]
dd$FocSecond.vs.Third=model.matrix(~ Focus,dd)[,4]

dd$Intonation[dd$experiment=="suborq"]="Interrogative"
dd$Intonation[dd$experiment=="suborn"]="Declarative"
dd$Intonation=factor(dd$Intonation)
contrasts(dd$Intonation)=cbind("Decl.vs.Inter"=c(0.5,-0.5))
dd$Decl.vs.Inter=model.matrix(~ Intonation,dd)[,2]

# in wide condition, renumber words for consistency
dd$woi=as.numeric(as.character(dd$element_number))
dd$woi[dd$Focus=='Wide']=dd$woi[dd$Focus=='Wide']+2
dd$woi=factor(dd$woi)

dd$Constituency[dd$bracketing=="l"]="(AB)C"
dd$Constituency[dd$bracketing=="r"]="A(BC)"
dd$Constituency=factor(dd$Constituency)
contrasts(dd$Constituency)=cbind("Left.vs.Right"=c(0.5,-0.5))
dd$Left.vs.Right=model.matrix(~ Constituency,dd)[,2]


# woi annotation
# I THOUGHT THEY SAID MARION_1 OR MARVIN_2 AND SARAH_3 ARRIVED BUT IN FACT THEY SAID THAT MARION_4 OR MARVIN_5 AND NOLAN_6 ARRIVED

# Manipulation in the Experiment:
# 1: Intonation: Declarative vs. Interrogative (i.e., polar question)
# 2: Focus: Which word is focused? Wide focus on entire coordinate structure vs. First (=woi 9), Second (=woi 11), or Third (=woi12)
# 3: Constituency: Do first two conjuncts form a constituent or the second two conjuncts?

# Manipulation 1 (intonation) was done between 2 sub-experiments; 2 (Focus) & 3 (Constituency) were within a single experiment

# There were 4 item sets
length(unique(data$itemOriginal))
ddply(dd,.(itemOriginal,experiment,Constituency,Focus,Intonation),summarise,length(condition))

# There were 27 participants in the declarative and 31 in the interrogative experiment
# almost all participated in both experiments
ddply(dd,.(experiment),summarise,length(unique(participant)))


# subset of only the second clause

# here we only look at the three NPs (Names):
dd2=subset(dd,woi%in%c('4','5','6'))

dd2$Position=factor(recode(dd2$woi,"'4'='A';'5'='B';'6'='C'"))
contrasts(dd2$Position)=cbind("First.vs.Late"=c(2/3,-1/3,-1/3),"Second.vs.Third"=c(0,0.5,-0.5))
dd2$PosFirst.vs.Late=model.matrix(~ Position,dd2)[,2]
dd2$PosSecond.vs.Third=model.matrix(~ Position,dd2)[,3]

# create 'wide' data frame with one row per utterance

# columns that vary depending on woi
varyColumns=c("word", "element_number", "duration", "relative_duration","Mean_F0", "Mean_F0_relative", "Mean_Intensity", "Mean_Intensity_relative","Position",'PosFirst.vs.Late','PosSecond.vs.Third')

# dput(names(dd))
dd.wide=reshape(dd,idvar=c("experiment", "participant","item","condition"),
                v.names=varyColumns,
                timevar="woi",direction="wide")


### Create Residuals for each dimension by fitting model which includes all the othe predictors except the one of interest
## (for plotting purposes)

# Residuals for focus

resid = dd2

for (i in c(4,5,8:11)) {
  colu = i
  print(paste('column',i,': ',names(dd)[i]))
  model.lm = lmer(as.matrix(dd2[colu])~
                Constituency*Intonation*Position+(1|participant)+(1|itemOriginal),
                data=dd2,na.action=na.exclude)
  modelResiduals = resid(model.lm)
  resid[colu]=as.matrix(modelResiduals)
}

focus.long=resid
focus.wide=reshape(focus.long,idvar=c("experiment", "participant","item","condition"),
                v.names=varyColumns,
                timevar="woi",direction="wide")



#  Residuals for bracketing

resid = dd2

for (i in c(4,5,8:11)) {
    colu = i
    print(paste('column',i,': ',names(dd)[i]))
    model.lm = lmer(as.matrix(dd2[colu])~
                        Focus*Intonation*Position+(1|participant)+(1|itemOriginal),
                    data=dd2,na.action=na.exclude)
    modelResiduals = resid(model.lm)
    resid[colu]=as.matrix(modelResiduals)
}

brack.long=resid
brack.wide=reshape(brack.long,idvar=c("experiment", "participant","item","condition"),
                   v.names=varyColumns,
                   timevar="woi",direction="wide")



# residuals for intonation

resid = dd2

for (i in c(4,5,8:11)) {
    colu = i
    print(paste('column',i,': ',names(dd)[i]))
    model.lm = lmer(as.matrix(dd2[colu])~
                        Focus*Constituency*Position+(1|participant)+(1|itemOriginal),
                    data=dd2,na.action=na.exclude)
    modelResiduals = resid(model.lm)
    resid[colu]=as.matrix(modelResiduals)
}

intonation.long=resid
intonation.wide=reshape(brack.long,idvar=c("experiment", "participant","item","condition"),
                   v.names=varyColumns,
                   timevar="woi",direction="wide")



# residuals for position

resid = dd2

for (i in c(4,5,8:11)) {
    colu = i
    print(paste('column',i,': ',names(dd)[i]))
    model.lm = lmer(as.matrix(dd2[colu])~
                        Focus*Constituency*Intonation+(1|participant)+(1|itemOriginal),
                    data=dd2,na.action=na.exclude)
    modelResiduals = resid(model.lm)
    resid[colu]=as.matrix(modelResiduals)
}

position.long=resid
position.wide=reshape(brack.long,idvar=c("experiment", "participant","item","condition"),
                        v.names=varyColumns,
                        timevar="woi",direction="wide")





# acoustic measures:

dput(names(dd)[c(4,5,8:11)])



##
## Plots and Analysis: Constituency
##

## for unresidualized plot:
# subset=data.long


# Pitch (thirdpitch seems to work best for phrasing cues--but this choicepoint should be motivated/understood better)
# Plots with lines to evoke that these are very stylized pitch tracks

plotwid=2

# Raw normalized pitch
ggplot(dd2, aes(x=Position, y=Mean_F0_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Noramlized Pitch (z-score)') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Normalized Pitch') + theme(plot.title = element_text(hjust = 0.5))
ggsave(file='plotsMar2017/PitchPhrasingRaw.pdf',width=3,height=3)

# Pitch residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=Mean_F0_relative,shape=Focus)) +
    stat_summary(fun.y=mean, geom="point") +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5)) + ylab('Residualized Pitch')
ggsave(file='plotsMar2017/PitchConstituencyResidualized.pdf',width=plotwid,height=3)



#Pitch residualized for predictors other than intonation
ggplot(intonation.long, aes(x=Position, y=Mean_F0_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5))  + ylab('')
ggsave(file='plotsMar2017/PitchIntonationResidualized.pdf',width=plotwid,height=3)

# Pitch residualized for predictors other than focus
ggplot(focus.long, aes(x=Position, y=Mean_F0_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Focus') + theme(plot.title = element_text(hjust = 0.5))  + ylab('')
ggsave(file='plotsMar2017/PitchFocusResidualized.pdf',width=plotwid,height=3)

#Pitch residualized for predictors other than position
ggplot(position.long, aes(x=Position, y=Mean_F0_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch')+ facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Position') + theme(plot.title = element_text(hjust = 0.5)) + ylab('')
ggsave(file='plotsMar2017/PitchPositionResidualized.pdf',width=3,height=3)



#
# plot for duration
#

plotwid=2

# Raw normalized duration
ggplot(dd2, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Noramlized Duration (z-score)') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Normalized Duration') + theme(plot.title = element_text(hjust = 0.5))
ggsave(file='plotsMar2017/DurationPhrasingRaw.pdf',width=3,height=3)

# duration residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=relative_duration,shape=Focus)) +
  stat_summary(fun.y=mean, geom="point") +
  #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
  ylab('Residualized Duration') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5)) + ylab('Residualized Duration')
ggsave(file='plotsMar2017/DurationConstituencyResidualized.pdf',width=plotwid,height=3)



#duration residualized for predictors other than intonation
ggplot(intonation.long, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5))  + ylab('')
ggsave(file='plotsMar2017/DurationIntonationResidualized.pdf',width=plotwid,height=3)

# duration residualized for predictors other than focus
ggplot(focus.long, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Focus') + theme(plot.title = element_text(hjust = 0.5))  + ylab('')
ggsave(file='plotsMar2017/DurationFocusResidualized.pdf',width=plotwid,height=3)

#Pitch residualized for predictors other than position
ggplot(position.long, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch')+ facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Position') + theme(plot.title = element_text(hjust = 0.5)) + ylab('')
ggsave(file='plotsMar2017/DurationPositionResidualized.pdf',width=3,height=3)



## Model:
# Does pitch cue phrasing?
# (if it does so uniformly across foci and intonations, this would be strong argument for overlay theory)

# First a general model, checking whether pitch of woi11 contributes to detecting phrasing in raw data,
# with effects of Intonation and Focus also part of the model
#
constituencyModel=glmer(Constituency~
                     scale(Mean_F0_relative.5)*scale(Decl.vs.Inter) +
                     scale(Mean_F0_relative.5)*FocFirst.vs.Late+scale(Mean_F0_relative.5)*FocSecond.vs.Third +
                     (scale(Mean_F0_relative.5)*scale(Decl.vs.Inter) +
                          scale(Mean_F0_relative.5)*FocFirst.vs.Late+scale(Mean_F0_relative.5)*FocSecond.vs.Third||participant) +
                     (scale(Mean_F0_relative.5)*FocFirst.vs.Late+scale(Mean_F0_relative.5)*FocSecond.vs.Third||itemOriginal)
                 ,family="binomial", data=dd.wide)
summary(constituencyModel)



# model of normalized pitch for word 6:
pitchModel5=lmer(Mean_F0_relative.5~
                    Intonation*Constituency*(FocWide.vs.Narrow+FocFirst.vs.Late+FocSecond.vs.Third)
                    +(1|item)+(1|participant),
                data=dd.wide,na.action=na.exclude)
summary(pitchModel5)

dd.wide$Mean_F0_relative.6_predict=predict(pitchModel)

# plot model predictions
ggplot(dd.wide, aes(x=Constituency, y=Mean_F0_relative.6_predict,shape=Focus)) +
    stat_summary(fun.y=mean, geom="point") +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ ., scales = "fixed")


pitchModel=lmer(Mean_F0_relative~
                     Position*Intonation*Constituency*(FocWide.vs.Narrow+FocFirst.vs.Late+FocSecond.vs.Third) + (1|itemOriginal) + (1|participant),
                 data=dd2)
summary(pitchModel)


sjp.lmer(pitchModel, y.offset = .4)

sjp.lmer(pitchModel, type = "pred.fe",vars=c('Position','Constituency'),show.scatter = F,facet.grid = F)

dd2$Mean_F0_relative_predict=predict(pitchModel)






# residuals:
ggplot(brack.long, aes(x=Position, y=Mean_F0_relative,shape=Focus)) +
    stat_summary(fun.y=mean, geom="point") +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed")



fief=fixef(pitchModel)
# add relevant coeffecients of constituency plus intercept
fief[grepl( "Constituency" , names( fief ) )]


# model predictions
ggplot(dd2, aes(x=Position, y=Mean_F0_relative_predict,shape=Focus)) +
    stat_summary(fun.y=mean, geom="point") +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed")





# Does pitch encode phrasing in questions?
# testing based on subset,  controlling for the effects of Focus:
pitchModelQuestion=glmer(Constituency~
                             scale(Mean_F0_relative.5)*FocFirst.vs.Late+scale(Mean_F0_relative.5)*FocSecond.vs.Third +
                             (scale(Mean_F0_relative.5)*FocFirst.vs.Late+scale(Mean_F0_relative.5)*FocSecond.vs.Third||participant) +
                             (scale(Mean_F0_relative.5)*FocFirst.vs.Late+scale(Mean_F0_relative.5)*FocSecond.vs.Third||itemOriginal)
                         ,family="binomial", data=subset(dd.wide,Intonation=="Interrogative"))
summary(pitchModelQuestion)


# Does pitch encode phrasing in the post-focal domain?
# Testing based on cases with focus on first word, pitch cue from second word:
pitchModelFocFirst=glmer(Constituency~
                             scale(thirdpitch.11)*scale(Decl.vs.Inter) +
                             (scale(thirdpitch.11)*scale(Decl.vs.Inter)||participant) +
                             (scale(thirdpitch.11)*scale(Decl.vs.Inter)||itemOriginal)
                         ,family="binomial", data=subset(data.wide,Focus=="First"))
summary(pitchModelFocFirst)



