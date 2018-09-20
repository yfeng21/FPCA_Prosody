library(lme4)
library(arm)
library(ggplot2)
library(reshape)
library(lattice)
library(plyr)
library(car)
library(sjPlot)
library(psych)

## Helper functions

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )

    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
    library(plyr)

    # Measure var on left, idvar + between vars on right of formula.
    data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                           .fun = function(xx, col, na.rm) {
                               c(subjMean = mean(xx[,col], na.rm=na.rm))
                           },
                           measurevar,
                           na.rm
    )

    # Put the subject means with original data
    data <- merge(data, data.subjMean)

    # Get the normalized data in a new column
    measureNormedVar <- paste(measurevar, "_norm", sep="")
    data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
        mean(data[,measurevar], na.rm=na.rm)

    # Remove this subject mean column
    data$subjMean <- NULL

    return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

    # Ensure that the betweenvars and withinvars are factors
    factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                         FUN=is.factor, FUN.VALUE=logical(1))

    if (!all(factorvars)) {
        nonfactorvars <- names(factorvars)[!factorvars]
        message("Automatically converting the following non-factors to factors: ",
                paste(nonfactorvars, collapse = ", "))
        data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
    }

    # Get the means from the un-normed data
    datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                       na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

    # Drop all the unused columns (these will be calculated with normed data)
    datac$sd <- NULL
    datac$se <- NULL
    datac$ci <- NULL

    # Norm each subject's data
    ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

    # This is the name of the new column
    measurevar_n <- paste(measurevar, "_norm", sep="")

    # Collapse the normed data - now we can treat between and within vars the same
    ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                        na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

    # Apply correction from Morey (2008) to the standard error and confidence interval
    #  Get the product of the number of conditions of within-S variables
    nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                    FUN.VALUE=numeric(1)))
    correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

    # Apply the correction factor
    ndatac$sd <- ndatac$sd * correctionFactor
    ndatac$se <- ndatac$se * correctionFactor
    ndatac$ci <- ndatac$ci * correctionFactor

    # Combine the un-normed means with the normed results
    merge(datac, ndatac)
}


dannot=read.csv("E:/Summer 2017/2_Stats/suborq_responses.txt",sep='\t')
dannot1=read.csv("E:/Summer 2017/2_Stats/suborn_responses.txt",sep='\t')
dannot2=rbind(dannot,dannot1)
# load acoustic measures
dd = read.csv("E:/Summer 2017/Yulan/interspeech/pgdb_analysis.txt")
#
dd$participant=factor(as.numeric(unlist(regmatches(dd$speaker, gregexpr("[[:digit:]]+", dd$speaker)))))
dd$experiment=factor(unlist(lapply(strsplit(as.character(dd$speaker), "\\_"), "[", 1)))
dd$recordedFile=paste0(dd$discourse,'.wav')


# merge acoustics with responses file
dd=merge(dd,condition.data,all.x=TRUE,by=c('discourse'),suffixes=c("",".other"))

dd$item=dd$item
numericCols=c("duration","relative_duration","pitch", "relative_pitch", "intensity", "relative_intensity")

nColumns = ncol(dd)
# convert to numeric column, otherwise treat as factor:
for (i in 1:nColumns) {
    if (colnames(dd)[i] %in% numericCols) {
        dd[, i] <- as.numeric(as.character(dd[, i]))
    } else {
        dd[, i] <- as.factor(as.character(dd[, i]))
    }
}

ddply(dd,.(experiment),summarize,length(unique(participant)))
participantsN=unique(dd$participant[dd$experiment=='suborn'])
participantsQ=unique(dd$participant[dd$experiment=='suborq'])
participantsBoth=participantsN[participantsN%in%participantsQ]

# only look at participants that were in both studies
dd=subset(dd,participant%in%participantsBoth)

ddply(dd,.(experiment,condition,element_number),summarise,length(condition))
ddply(dd,.(experiment),summarise,length(unique(participant)))

dd$Focus=factor(recode(dd$condition,"'1'='Third';'2'='Second';'3'='First';'4'='Wide'"),levels=c('Wide','First','Second','Third'))
contrasts(dd$Focus)=cbind("Wide.vs.Narrow"=c(3/4,-1/4,-1/4,-1/4),"First.vs.Late"=c(0,2/3,-1/3,-1/3),"Second.vs.Third"=c(0,0,1/2,-1/2))
dd$FocWide.vs.Narrow=model.matrix(~ Focus,dd)[,2]
dd$FocFirst.vs.Late=model.matrix(~ Focus,dd)[,3]
dd$FocSecond.vs.Third=model.matrix(~ Focus,dd)[,4]


#dd$Focused=

dd$Intonation[dd$experiment=="suborq"]="Interrogative"
dd$Intonation[dd$experiment=="suborn"]="Declarative"
dd$Intonation=factor(dd$Intonation)
contrasts(dd$Intonation)=cbind("Decl.vs.Inter"=c(0.5,-0.5))
dd$Decl.vs.Inter=model.matrix(~ Intonation,dd)[,2]

# in wide condition, renumber words for consistency
dd$woi=as.numeric(as.character(dd$element_number))
dd$woi[dd$Focus=='Wide']=dd$woi[dd$Focus=='Wide']+4
dd$woi=factor(dd$woi)

dd$Constituency[dd$bracketing=="l"]="(AB)C"
dd$Constituency[dd$bracketing=="r"]="A(BC)"
dd$Constituency=factor(dd$Structure)
contrasts(dd$Constituency)=cbind("Left.vs.Right"=c(0.5,-0.5))
dd$Left.vs.Right=model.matrix(~ Constituency,dd)[,2]


# RA annotations
#
length(unique(dd$recordedFile))

# Problematic soundfiles: 86, 6.9% of the data
length(unique(dd$recordedFile[dd$Thea_problematic=='1'|dd$David_problematic=='1'|dd$Erin2_problematic=='1']))

# exclude files marked as problematic
dd=subset(dd,Thea_problematic!='1'&Thea_intonation!=4&Erin2_problematic!='1')

# remove level 4='problematic' from Thea's intonation annotation:
dd$Thea_intonation=factor(dd$Thea_intonation)

length(unique(dd$recordedFile))
# remaining files: 1167/1253

# beginPause("What kind of response?")
# comment ("Where does the main prominence fall?")
# anno = choice ("intonation",3)
# option ("Falling")
# option ("Rising")
# option ("unclear")
# option ("problematic")
# anno = choice ("focus",5)
# option ("Wide Focus")
# option ("First")
# option ("Second")
# option ("Third")
# option ("unclear")
# anno = choice ("branching",3)
# option ("(a b) c")
# option ("a (b c)")
# option ("unclear")
# anno = boolean("problematic",0)
# clicked = endPause("Continue",1)

# "Cohen suggested the Kappa result be interpreted as follows: values ≤ 0 as indicating no agreement and 0.01–0.20 as none to slight, 0.21–0.40 as fair, 0.41– 0.60 as moderate, 0.61–0.80 as substantial, and 0.81–1.00 as almost perfect agreement."




# Intonation
dd$IntonationA1=mapvalues(dd$Thea_intonation,from=c("1", "2", "3", "4"),to=c("Falling", "Rising", "Unclear", NA))
dd$IntonationTest=mapvalues(dd$Erin2_intonation,from=c("1", "2", "3", "4"),to=c("Declarative", "Interrogative", NA, NA))
dd$IntonationA2=mapvalues(dd$Erin2_intonation,from=c("1", "2", "3", "4"),to=c("Falling", "Rising", "Unclear", NA))
# interrater agreement: 0.96 ('almost perfect')
cohen.kappa(x=cbind(dd$IntonationA1,dd$IntonationA2))

dd=ddply(dd,.(recordedFile),transform,IntonationCorrect=(Intonation==IntonationTest&!is.na(IntonationTest)))
# overall proportion of intonation annotation as expected based on annotation by A1: 0.963
nrow(dd[dd$IntonationCorrect,])/nrow(dd)

# intonation annotation breakdown:
subsIntonation=subset(dd,!is.na(IntonationA2))
subsIntonation=ddply(subsIntonation,.(experiment),transform,n=length(unique(recordedFile)))
ddply(subsIntonation,.(experiment,IntonationA2),summarise,number=length(unique(recordedFile)),proportion=(length(unique(recordedFile))/unique(n)))

# Prominence:
dd$ProminenceA1=mapvalues(dd$Thea_prominence,from=c("1", "2", "3","4","5"),to=c("Wide", "First", "Second","Third",NA))
dd$ProminenceA2=mapvalues(dd$Erin2_prominence,from=c("1", "2", "3", "4","5"),to=c("Wide", "First", "Second","Third",NA))
# Prominence: agreement 0.66 'substantial'
cohen.kappa(x=cbind(dd$ProminenceA1,dd$ProminenceA2))
#
dd=ddply(dd,.(recordedFile),transform,ProminenceCorrect=(Focus==ProminenceA2&!is.na(ProminenceA2)))
#
# overall proportion of prominence as expected, based on annotation by A1: 0.379
nrow(dd[dd$ProminenceCorrect,])/nrow(dd)

#
# Breakdown of proportions:
subsProminence=subset(dd,!is.na(ProminenceA2))
subsProminence=ddply(subsProminence,.(Focus),transform,n=length(unique(recordedFile)))
ddply(subsProminence,.(Focus,ProminenceA2),summarise,number=length(unique(recordedFile)),proportion=(length(unique(recordedFile))/unique(n)))




# Branching:
dd$ConstituencyA1=mapvalues(dd$Thea_branching,from=c("1", "2", "3"),to=c("(AB)C", "A(BC)", "Unclear"))
dd$ConstituencyA2=mapvalues(dd$Erin2_branching,from=c("1", "2", "3"),to=c("(AB)C", "A(BC)", "Unclear"))
# Branching: interrater reliability: 0.73 (substantial)
cohen.kappa(x=cbind(dd$ConstituencyA1,dd$ConstituencyA2))

dd=ddply(dd,.(recordedFile),transform,ConstituencyCorrect=(as.character(Constituency)==as.character(ConstituencyA2)&!is.na(ConstituencyA2)))
#
# overall proportion of constituency as expected based on annotation by A1: 0.610
#
nrow(dd[dd$ConstituencyCorrect,])/nrow(dd)

# breakdown of correct/incorect by levels:
subsConstituency=subset(dd,!is.na(ConstituencyA2))
subsConstituency=ddply(subsConstituency,.(Constituency),transform,n=length(unique(recordedFile)))
ddply(subsConstituency,.(Constituency,ConstituencyA2),summarise,number=length(unique(recordedFile)),proportion=(length(unique(recordedFile))/unique(n)))





## to only consider data for which annotation A1 was correct in all three dimensions, uncomment this, but it reduces the data a lot...:

dd=subset(dd,IntonationCorrect&ProminenceCorrect&ConstituencyCorrect)


# woi annotation
# I THOUGHT THEY SAID MARION_1 OR MARVIN_2 AND SARAH_3 ARRIVED BUT IN FACT THEY SAID THAT MARION_4 OR MARVIN_5 AND NOLAN_6 ARRIVED

# Manipulation in the Experiment:
# 1: Intonation: Declarative vs. Interrogative (i.e., polar question)
# 2: Focus: Which word is focused? Wide focus on entire coordinate structure vs. First (=woi 9), Second (=woi 11), or Third (=woi12)
# 3: Constituency: Do first two conjuncts form a constituent or the second two conjuncts?

# Manipulation 1 (intonation) was done between 2 sub-experiments; 2 (Focus) & 3 (Constituency) were within a single experiment

# There were 4 item sets
length(unique(dd$item))
ddply(dd,.(item,experiment,Constituency,Focus,Intonation),summarise,length(condition))

# There were 27 participants in the declarative and 31 in the interrogative experiment
# almost all participated in both experiments
ddply(dd,.(experiment),summarise,length(unique(participant)))


# subset of only the second clause

# here we only look at the three NPs (Names):
dd2=subset(dd,woi%in%c('6','8','10'))

dd2$Position=factor(recode(dd2$woi,"'6'='A';'8'='B';'10'='C'"))
contrasts(dd2$Position)=cbind("First.vs.Late"=c(2/3,-1/3,-1/3),"Second.vs.Third"=c(0,0.5,-0.5))
dd2$PosFirst.vs.Late=model.matrix(~ Position,dd2)[,2]
dd2$PosSecond.vs.Third=model.matrix(~ Position,dd2)[,3]

# create 'wide' data frame with one row per utterance

# columns that vary depending on woi
varyColumns=c("word", "element_number", "duration", "relative_duration","pitch", "relative_pitch", "intensity", "relative_intensity","Position",'PosFirst.vs.Late','PosSecond.vs.Third')

# dput(names(dd))
dd.wide=reshape(dd2,idvar=c("experiment", "participant","item","condition"),
                v.names=varyColumns,
                timevar="woi",direction="wide")


reshape(brack.long,idvar=c("experiment", "participant","item","condition"),
        v.names=varyColumns,
        timevar="woi",direction="wide")

### Create Residuals for each dimension by fitting model which includes all the othe predictors except the one of interest
## (for plotting purposes)

# residuals based on using intensity as predictor:

resid = dd2

for (i in c(4,5,8:11)) {
  colu = i
  print(paste('column',i,': ',names(dd)[i]))
  model.lm = lmer(as.matrix(dd2[colu])~
                    relative_intensity+(1|participant)+(1|itemOriginal),
                  data=dd2,na.action=na.exclude)
  modelResiduals = resid(model.lm)
  resid[colu]=as.matrix(modelResiduals)
}

intensityOut.long=resid
intensityOut.wide=reshape(focus.long,idvar=c("experiment", "participant","item","condition"),
                   v.names=varyColumns,
                   timevar="woi",direction="wide")


# residuals based on using pitch as predictor:

resid = dd2

for (i in c(4,5,8:11)) {
  colu = i
  print(paste('column',i,': ',names(dd)[i]))
  model.lm = lmer(as.matrix(dd2[colu])~
                    relative_pitch+(1|participant)+(1|itemOriginal),
                  data=dd2,na.action=na.exclude)
  modelResiduals = resid(model.lm)
  resid[colu]=as.matrix(modelResiduals)
}

pitchOut.long=resid
pitchOut.wide=reshape(focus.long,idvar=c("experiment", "participant","item","condition"),
                          v.names=varyColumns,
                          timevar="woi",direction="wide")

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

plotwid=1.5
altplotwid=2.5
plotheight=2.7

# Relativized pitch
ggplot(dd2, aes(x=Position, y=relative_pitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab(' (z-score)') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Relativized Pitch') + theme(plot.title = element_text(hjust = 0.5)) + xlab('')
#ggsave(file='PitchRaw.pdf',width=3,height=3)

# Pitch residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=relative_pitch,shape=Focus)) +
    stat_summary(fun.y=mean, geom="point") +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5)) + ylab('Residualized Pitch') + xlab('')

# Intensity residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=Mean_Intensity_relative,shape=Focus)) +
  stat_summary(fun.y=mean, geom="point") +
  #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
  ylab('Residualized Intensity') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5)) + xlab('')



## MM version

plotData <- summarySE(brack.long,'relative_pitch', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency'),na.rm = T)
cons.plot<-ggplot(plotData, aes(x=Position, y=relative_pitch,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_blank()) + ylab('Residualized Pitch') + xlab('')+ scale_y_continuous(breaks = c(-0.5,-0.25,0,0.25,0.5), limits = c(-0.52,0.52))
cons.plot
#ggsave(file='PitchConstituencyResidualized.pdf',width=plotwid,height=plotheight)

#Pitch residualized for predictors other than intonation
ggplot(intonation.long, aes(x=Position, y=relative_pitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10)  + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5))  + ylab('') + theme(legend.position="none") + xlab('')

#INtensity residualized for predictors other than intonation
ggplot(intonation.long, aes(x=Position, y=Mean_Intensity_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10)  + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5))  + ylab('') + theme(legend.position="none") + xlab('')


## MM version

plotData <- summarySE(intonation.long,'relative_pitch', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency'),na.rm = T)
tune.plot<-ggplot(plotData, aes(x=Position, y=relative_pitch,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),strip.text.y = element_blank()) + xlab('') + scale_y_continuous(breaks = c(-0.5,-0.25,0,0.25,0.5), limits = c(-0.52,0.52))
tune.plot
#ggsave(file='PitchIntonationResidualized.pdf',width=plotwid,height=plotheight)

# Pitch residualized for predictors other than focus
ggplot(focus.long, aes(x=Position, y=relative_pitch,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10)  + ggtitle('Effect of Focus') + xlab('') + theme(plot.title = element_text(hjust = 0.5))  + ylab('') #+ theme(legend.position="none")

# Intensity residualized for predictors other than focus
ggplot(focus.long, aes(x=Position, y=Mean_Intensity_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10)  + ggtitle('Effect of Focus') + xlab('') + theme(plot.title = element_text(hjust = 0.5))  + ylab('') #+ theme(legend.position="none")

## MM version

plotData <- summarySE(focus.long,'relative_pitch', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency'),na.rm = T)
focus.plot<-ggplot(plotData, aes(x=Position, y=relative_pitch,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Focus') + theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) + ylab('Residualized Pitch') + xlab('') + scale_y_continuous(breaks = c(-0.5,-0.25,0,0.25,0.5), limits = c(-0.52,0.52))
focus.plot
#ggsave(file='PitchFocusResidualized.pdf',width=altplotwid,height=plotheight)
#ggsave('PitchResidualized.pdf',width=5.5,height=plotheight)

#Pitch residualized for predictors other than position
ggplot(position.long, aes(x=Position, y=Mean_F0_relative,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch')+ facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Position') + theme(plot.title = element_text(hjust = 0.5)) + ylab('')
#ggsave(file='PitchPositionResidualized.pdf',width=3,height=3)


## Diff MM version - All in one

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

pdf("PitchResidualized.pdf",width=fullwidth,height=plotheight)
multiplot(cons.plot, tune.plot, focus.plot, cols = 3)
dev.off()
lay = rbind(c(1,1,1,1,2,2,2,3,3,3,3,3))
pdf("PitchResidualized.pdf",width=fullwidth,height=plotheight)
grid.arrange(cons.plot, tune.plot, focus.plot, ncol = 3,layout_matrix = lay)
dev.off()

brack.long$Setting <- 'Constituency'
intonation.long$Setting <- 'Tune'
focus.long$Setting <- 'Focus'
fullwidth = 6.5
comb <- rbind(brack.long, intonation.long, focus.long)

plotData <- summarySE(comb,'Mean_F0_relative', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency', 'Setting'),na.rm = T)
ggplot(plotData, aes(x=Position, y=Mean_F0_relative,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Setting*Constituency) + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ylab('Residualized Pitch') + xlab('') + scale_y_continuous(breaks = c(-0.5,-0.25,0,0.25,0.5))
ggsave(file='PitchResidualized.pdf',width=fullwidth,height=plotheight)


plotData <- summarySE(comb,'relative_duration', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency', 'Setting'),na.rm = T)
ggplot(plotData, aes(x=Position, y=relative_duration,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Duration') + facet_grid(Intonation ~ Setting+Constituency) + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + xlab('') + scale_y_continuous(breaks = c(-0.5,-0.25,0,0.25,0.5))
ggsave(file='DurationResidualized.pdf',width=fullwidth,height=plotheight)



#
# plot for duration
#


# Raw normalized duration
ggplot(dd2, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('(z-score)') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=8)  + ggtitle('Relativized Duration') + theme(plot.title = element_text(hjust = 0.5)) + xlab('')
ggsave(file='DurationRaw.pdf',width=3,height=2)

# duration residualized for predictors other than phrasing
ggplot(brack.long, aes(x=Position, y=relative_duration,shape=Focus)) +
  stat_summary(fun.y=mean, geom="point") +
  #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
  stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
  ylab('Residualized Duration') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5)) + ylab('Residualized Duration') + xlab('')

## MM version

plotData <- summarySE(brack.long,'relative_duration', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency'),na.rm = T)
cons.plot<- ggplot(plotData, aes(x=Position, y=relative_duration,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Duration') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Constituency') + theme(plot.title = element_text(hjust = 0.5), strip.text.y = element_blank()) + ylab('Residualized Pitch') + xlab('')+ scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3), limits = c(-0.25,0.3))
cons.plot
ggsave(file='DurationConstituencyResidualized.pdf',width=plotwid,height=plotheight)



#duration residualized for predictors other than intonation
ggplot(intonation.long, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10)  + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5))  + ylab('') + theme(legend.position="none") + xlab('')

## MM version

plotData <- summarySE(intonation.long,'relative_duration', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency'),na.rm = T)
tune.plot <- ggplot(plotData, aes(x=Position, y=relative_duration,shape=Focus,group=Focus)) +
    geom_point() +
    #stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    geom_line() +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + theme(legend.position="none") + ggtitle('Effect of Tune') + theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),strip.text.y = element_blank()) + ylab('Residualized Pitch') + xlab('')+ scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3), limits = c(-0.25,0.3))
tune.plot
ggsave(file='DurationIntonationResidualized.pdf',width=plotwid,height=plotheight)

# duration residualized for predictors other than focus
ggplot(focus.long, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Focus') + theme(plot.title = element_text(hjust = 0.5))  + ylab('') # + theme(legend.position="none") + xlab('')

## MM version

plotData <- summarySE(focus.long,'relative_duration', groupvars = c('Position', 'Focus', 'Intonation', 'Constituency'),na.rm = T)
focus.plot <- ggplot(plotData, aes(x=Position, y=relative_duration,shape=Focus,group=Focus)) +
    geom_point() +
    geom_line() +
    ylab('Residualized Duration') + facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Focus') + theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) + ylab('Residualized Pitch') + xlab('')+ scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3), limits = c(-0.25,0.3))
focus.plot
ggsave(file='DurationFocusResidualized.pdf',width=altplotwid,height=plotheight)

#Pitch residualized for predictors other than position
ggplot(position.long, aes(x=Position, y=relative_duration,shape=Focus))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + ylab('Residualized Pitch')+ facet_grid(Intonation ~ Constituency, scales = "fixed") + theme_bw(base_size=10) + ggtitle('Effect of Position') + theme(plot.title = element_text(hjust = 0.5)) + ylab('')
# ggsave(file='DurationPositionResidualized.pdf',width=3,height=3)

pdf("DurationResidualized.pdf",width=fullwidth,height=plotheight)
grid.arrange(cons.plot, tune.plot, focus.plot, ncol = 3,layout_matrix = lay)
dev.off()



## Model:
# Does pitch cue phrasing?
# (if it does so uniformly across foci and intonations, this would be strong argument for overlay theory)

# model of normalized pitch for word 6:
pitchModel5=lmer(Mean_F0_relative.5~
                    Intonation*Constituency*(FocWide.vs.Narrow+FocFirst.vs.Late+FocSecond.vs.Third)
                    +(Intonation*Constituency*(FocWide.vs.Narrow+FocFirst.vs.Late+FocSecond.vs.Third)|item)+(Intonation*Constituency*(FocWide.vs.Narrow+FocFirst.vs.Late+FocSecond.vs.Third)|participant),
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
                             scale(relative_pitch.5)*FocFirst.vs.Late+scale(relative_pitch.5)*FocSecond.vs.Third +
                             (scale(relative_pitch.5)*FocFirst.vs.Late+scale(relative_pitch.5)*FocSecond.vs.Third||participant) +
                             (scale(relative_pitch.5)*FocFirst.vs.Late+scale(relative_pitch.5)*FocSecond.vs.Third||itemOriginal)
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



