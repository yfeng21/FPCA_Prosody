# Residuals for focus
library(lme4)

i=1
features=data.frame(discourse_list,speakers,intonation,focus,structure)
data1=data.frame(f0_list[i],time_list[i])
colnames(data1) <- c("f0","time")
write.csv(structure_item1, file = "structure_item1.csv",row.names=FALSE)

t_data$F0_relative<-t_data$F0_relative
t_data$element_norm_time<-t_data$element_norm_time-4
t_data$Intonation<-as.factor(t_data$Intonation)
t_data$Structure<-as.factor(t_data$Structure)
t_data$Focus<-as.factor(t_data$Focus)
t_data$speaker<-as.factor(t_data$speaker)

# Residuals for focus
model.lm = lmer(F0_relative~
                  element_norm_time*Structure*Intonation+(1|speaker),data=t_data,na.action=na.exclude)

modelResiduals = resid(model.lm)
t_data$Focus_res=modelResiduals

#Residuals for intonation
model.lm = lmer(F0_relative~
                  element_norm_time*Structure*Focus+(1|speaker),data=t_data,na.action=na.exclude)

modelResiduals = resid(model.lm)
t_data$Int_res=modelResiduals

#Residuals for Structure
model.lm = lmer(F0_relative~
                  element_norm_time*Intonation*Focus+(1|speaker),data=t_data,na.action=na.exclude)

modelResiduals = resid(model.lm)
t_data$Str_res=modelResiduals

#second word only
t_data_2$F0_relative<-t_data_2$F0_relative
t_data_2$element_norm_time<-t_data_2$element_norm_time+1
t_data_2$Intonation<-as.factor(t_data_2$Intonation)
t_data_2$Structure<-as.factor(t_data_2$Structure)
t_data_2$Focus<-as.factor(t_data_2$Focus)
t_data_2$speaker<-as.factor(t_data_2$speaker)

#Residuals for Structure
model.lm = lmer(F0_relative~
                  element_norm_time*Intonation*Focus+(1|speaker),data=t_data_2,na.action=na.exclude)
modelResiduals = resid(model.lm)
t_data_2$Str_res=modelResiduals

#Residuals for intonation
model.lm = lmer(F0_relative~
                  element_norm_time*Structure*Focus+(1|speaker),data=t_data_2,na.action=na.exclude)
modelResiduals = resid(model.lm)
t_data_2$Int_res=modelResiduals

# Residuals for focus
model.lm = lmer(F0_relative~
                  element_norm_time*Structure*Intonation+(1|speaker),data=t_data_2,na.action=na.exclude)

modelResiduals = resid(model.lm)
t_data_2$Focus_res=modelResiduals




