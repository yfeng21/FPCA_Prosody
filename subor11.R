library(lme4)
library(arm)
library(ggplot2)
library(reshape)
library(lattice)
library(plyr)
library(car)

setwd("/Users/chael/Dropbox/Projects/dimensions")

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
ggplot(data.long, aes(x=Position, y=thirdpitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Raw pitch') + facet_grid(Constituency~Intonation, scales = "fixed")
# ggsave(file='plotsDec2016/PitchPhrasingRaw.pdf',height=4)

# Pitch residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=thirdpitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Constituency~Intonation, scales = "fixed") 
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

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                               -0.020682   0.060909  -0.340   0.7342  
# scale(thirdpitch.11)                       0.230049   0.092289   2.493   0.0127 *
#   scale(Decl.vs.Inter)                       0.052813   0.049343   1.070   0.2845  
# FocFirst.vs.Late                           0.040562   0.120608   0.336   0.7366  
# FocSecond.vs.Third                         0.000389   0.137473   0.003   0.9977  
# scale(thirdpitch.11):scale(Decl.vs.Inter)  0.047734   0.051980   0.918   0.3585  
# scale(thirdpitch.11):FocFirst.vs.Late      0.063934   0.118488   0.540   0.5895  
# scale(thirdpitch.11):FocSecond.vs.Third    0.006308   0.147074   0.043   0.9658  

# Does pitch encode phrasing in questions?
# testing based on subset,  controlling for the effects of Focus:
pitchModelQuestion=glmer(Constituency~
                   scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third + 
                      (scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third||participant) +
                   (scale(thirdpitch.11)*FocFirst.vs.Late+scale(thirdpitch.11)*FocSecond.vs.Third||itemOriginal)
                 ,family="binomial", data=subset(data.wide,Intonation=="Interrogative"))
summary(pitchModelQuestion)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                              0.01496    0.06468   0.231   0.8171  
# scale(thirdpitch.11)                     0.14244    0.06568   2.169   0.0301 *
#   FocFirst.vs.Late                        -0.01358    0.15867  -0.086   0.9318  
# FocSecond.vs.Third                       0.05811    0.18330   0.317   0.7512  
# scale(thirdpitch.11):FocFirst.vs.Late   -0.01629    0.14946  -0.109   0.9132  
# scale(thirdpitch.11):FocSecond.vs.Third -0.01739    0.19105  -0.091   0.9275 


# Does pitch encode phrasing in the post-focal domain?
# Testing based on cases with focus on first word, pitch cue from second word:
pitchModelFocFirst=glmer(Constituency~
                           scale(thirdpitch.11)*scale(Decl.vs.Inter) + 
                           (scale(thirdpitch.11)*scale(Decl.vs.Inter)||participant) +
                           (scale(thirdpitch.11)*scale(Decl.vs.Inter)||itemOriginal)
                         ,family="binomial", data=subset(data.wide,Focus=="First"))
summary(pitchModelFocFirst)




#
#  other stuff below, currently not used
#
#

#
# look at residualized measures
#

ggplot(brack.wide, aes(x=Constituency, y=thirdpitch.11)) + geom_boxplot() + facet_grid(~Intonation)

ggplot(brack.wide, aes(x=Structure, y=thirdpitch.11)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,)  

# for residualized plot:
subBrackDecl=subset[brack.long$Intonation=="Declarative",]
subBrackInter=subset[brack.long$Intonation=="Interrogative",]

ggplot(data.long, aes(x=Position, y=thirdpitch))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + ylab('Raw Pitch (Hz)') + facet_grid(Focus ~ Structure + Intonation, scales = "fixed") + xlab('Position')
# ggsave(file='plotsDec2016/PitchPhrasingRaw.pdf')
ggplot(brack.long, aes(x=Position, y=thirdpitch))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + ylab('Residualized Pitch') + facet_grid(Focus ~ Structure + Intonation, scales = "fixed") 

# Model: Does pitch cue phrasing?
pitchModel=glm(Structure~scale(thirdpitch.11)*Intonation+scale(thirdpitch.11)*Focus,family="binomial", data=brack.wide) 
summary(pitchModel)

# what about just in interrogatives?
subsInterrogative=subset(brack.wide,Intonation=="Interrogative")
pitchModelInter=glm(Structure~scale(thirdpitch.11),family="binomial", data=subsInterrogative) 
summary(pitchModelInter)


# measures other than pitch

# Duration
ggplot(brack.long, aes(x=Position, y=duration))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + ylab('Residualized Pitch') + facet_grid(Focus ~ Structure + Intonation, scales = "fixed") 

# Intensity (this plot looks incredible--i think it's this good because final lengthening will lead to drop in mean intensity, since it doesn't go with higher intensity (as opposed to prominence))
ggplot(brack.long, aes(x=Position, y=meanIntensity))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + ylab('Residualized Pitch') + facet_grid(Focus ~ Structure + Intonation, scales = "fixed") 




#
# old stuff
#
#


##
## Graphs: Intonation
## 

subset=inton.long
subset1=subset[subset$experiment=="suborn",]
subset2=subset[subset$experiment=="suborq",]

###############
#
# Intonation
#
####

ggplot(subset, aes(x=Constituent, y=,fourthpitch,group=Intonation))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(. ~ Intonation, scales = "fixed") + ylab("LatePitch")

# Intonation*Structure

ggplot(subset, aes(x=word, y=,fourthpitch,group=Intonation))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Structure ~ Intonation, scales = "fixed")

# Intonation*Focus

ggplot(subset, aes(x=word, y=,fourthpitch,group=Intonation))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Intonation, scales = "fixed")

#
# Intonation*Focus*Structure
#

plot1=ggplot(subset1, aes(x=word, y=fourthpitch)) + stat_summary(fun.y=mean, geom="point") + 
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + 
  facet_grid(Focus ~ Structure, scales = "fixed") + labs(title="Declarative")  + ylab("LatePitch")

plot2=ggplot(subset2, aes(x=word, y=fourthpitch)) + stat_summary(fun.y=mean, geom="point") + 
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + 
  facet_grid(Focus ~ Structure, scales = "fixed") + labs(title="Interrogative")  + ylab("LatePitch")

multiplot(plot1,plot2,cols=2)

# Constituent A  lower if second constituent focused in Interrogative condition?

# ##
# ## Graphs: Focus
# ## 

# subset=focus.long
# subset1=subset[subset$Focus=="Control",]
# subset2=subset[subset$Focus=="First",]
# subset3=subset[subset$Focus=="Second",]
# subset4=subset[subset$Focus=="Third",]

# ##
# ## Line graphs
# ##

# # Focus effect

# ggplot(subset, aes(x=word, y=,maxpitch,group=Focus))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(. ~ Focus, scales = "fixed") 

# # Focus*Intonation

# ggplot(subset, aes(x=word, y=,maxpitch,group=Focus))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Intonation, scales = "fixed") 

# # Focus*Structure

# ggplot(subset, aes(x=word, y=,maxpitch,group=Structure))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Structure, scales = "fixed")


##
## Graphs: Focus
## 

subset=focus.long
subset1=subset[subset$experiment=="suborn",]
subset2=subset[subset$experiment=="suborq",]
subset3=subset[subset$bracketing=="l",]
subset4=subset[subset$bracketing=="r",]

##
## Line graphs
##

# Focus effect

ggplot(subset, aes(x=Constituent, y=,maxpitch,group=Focus))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(. ~ Focus, scales = "fixed") + ylab("MaxPitch")

# Focus*Intonation

ggplot(subset, aes(x=word, y=,maxpitch,group=Focus))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Intonation, scales = "fixed") 

# Focus*Structure

ggplot(subset, aes(x=word, y=,fourthpitch,group=Structure))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Structure, scales = "fixed")

# Focus*Intonation*Structure
plot1=ggplot(subset1, aes(x=Constituent, y=maxpitch)) + stat_summary(fun.y=mean, geom="point") + 
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + 
  facet_grid(Focus ~ bracketing, scales = "fixed") + labs(title="Declarative") + ylab("Maxpitch")

plot2=ggplot(subset2, aes(x=Constituent, y=maxpitch)) + stat_summary(fun.y=mean, geom="point") + 
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + 
  facet_grid(Focus ~ bracketing, scales = "fixed") + labs(title="Interrogative") + ylab("MaxPitch")

multiplot(plot1,plot2,cols=2)


# Interaction: Post-focal pitch reduction less in interrogative intonation

##write.csv(center_resid,file="center_resid.csv")
# write.csv(data.long, file="datalong.csv")
# write.csv(data.wide,file="datawide.csv")


# The effect of Constituency on Duration:
ggplot(brack.long, aes(x=Constituent, y=duration,group=Structure))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(. ~ Structure, scales = "fixed") + ylab("Duration")

# The effect of Constituency on Duration by Intonation
ggplot(brack.long, aes(x=word, y=duration,group=Structure))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Intonation ~ Structure, scales = "fixed") 

# The effect of Constituency on Duration by Focus
ggplot(brack.long, aes(x=word, y=duration,group=Structure))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Structure, scales = "fixed") 