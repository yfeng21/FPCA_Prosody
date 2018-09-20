## Model:
# Does pitch cue phrasing?
# (if it does so uniformly across foci and intonations, this would be strong argument for overlay theory)

# First a general model, checking whether pitch of woi11 contributes to detecting phrasing in raw data,
# with effects of Intonation and Focus also part of the model
#

dd.wide$relpitch1=scale(dd.wide$Mean_F0_relative.4-dd.wide$Mean_F0_relative.5)
dd.wide$relpitch2=scale(dd.wide$Mean_F0_relative.5-dd.wide$Mean_F0_relative.6)


ggplot(brack.wide, aes(x=Constituency, y=Mean_F0_relative.5,shape=Focus)) +
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) +
    ylab('Residualized Pitch') + facet_grid(Intonation ~ ., scales = "fixed")


ggplot(brack.wide, aes(x=Constituency, y=Mean_F0_relative.5)) +
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) +
    stat_summary(fun.y = "mean", geom="line") +
    ylab('Residualized Pitch')

pitchModelResi=glmer(Constituency~scale(Mean_F0_relative.5)+scale(relative_duration.5)+(1|item)+(1|participant),family="binomial", data=brack.wide)
summary(pitchModelResi)



pitchModel=glmer(Constituency~relpitch1+relpitch2+(1|item)+(1|participant),family="binomial", data=dd.wide)
summary(pitchModel)



pitchModel=glmer(Constituency~
                     scale(Mean_F0_relative.5)*(scale(Decl.vs.Inter)+scale(FocWide.vs.Narrow)+scale(FocFirst.vs.Late)+scale(FocSecond.vs.Third))+
                     scale(Mean_F0_relative.6)*(scale(Decl.vs.Inter)++scale(FocWide.vs.Narrow)+scale(FocFirst.vs.Late)+scale(FocSecond.vs.Third))+
                     (1|item) + (1|participant)
                 ,family="binomial", data=dd.wide)
summary(pitchModel)


pitchModel=glmer(Constituency~
                     scale(Mean_F0_relative.5)*scale(Mean_F0_relative.6)+
                     (1|item) + (1|participant)
                 ,family="binomial", data=dd.wide)
summary(pitchModel)
