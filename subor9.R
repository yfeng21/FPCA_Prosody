library(lme4)
library(Design)
library(arm)
library(ggplot2)
library(reshape)
library(lattice)


multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

setwd("/Volumes/prosodylab/work_experiments/1_projects_1/3_subor/2_suborqn_r")


suborn <- read.csv("suborn.csv")
suborn.responses <- read.csv("suborn_responses.txt",sep = "\t")
names(suborn.responses)[names(suborn.responses)=="item_original"] <- "itemOriginal"
data.suborn=merge(suborn,suborn.responses,all.x=TRUE,by=c("item","condition","participant"))

suborq <- read.csv("suborq.csv")
suborq$word[suborq$item==4&suborq$condition==4&suborq$word==3]=7
suborq.responses <- read.csv("suborq_responses.txt",sep = "\t")
names(suborq.responses)[names(suborq.responses)=="condition"] <- "itemOriginal"
names(suborq.responses)[names(suborq.responses)=="bracketing"] <- "condition"
names(suborq.responses)[names(suborq.responses)=="order"] <- "bracketing"
names(suborq.responses)[names(suborq.responses)=="order.1"] <- "order"
data.suborq=merge(suborq,suborq.responses,all.x=TRUE,by=c("item","condition","participant"))

data=rbind(data.suborn,data.suborq)

for (i in 1:6) {
  data[,i]<- factor(data[,i])
}

for (i in 7:22) {
  data[,i]<- as.numeric(as.character(data[,i]))
}


data$Focus[data$condition==1]="Third"
data$Focus[data$condition==2]="Second"
data$Focus[data$condition==3]="First"
data$Focus[data$condition==4]="Control"
data$Focus=factor(data$Focus,levels=c("Control","First","Second","Third"))

data$Intonation[data$experiment.x=="suborq"]="Interrogative"
data$Intonation[data$experiment.x=="suborn"]="Declarative"
data$Intonation=factor(data$Intonation)

data$Constituent[data$word=="9"]="A"
data$Constituent[data$word=="11"]="B"
data$Constituent[data$word=="13"]="C"
data$Constituent=factor(data$Constituent)

data$Structure[data$bracketing=="l"]="(AB)C"
data$Structure[data$bracketing=="r"]="A(BC)"
data$Structure=factor(data$Structure)

# I THOUGHT THEY SAID_1 MARION_2 OR_3 MARVIN_4 AND_5 SARAH_6 ARRIVED_7 BUT IN FACT THEY SAID THAT_8 MARION_9 OR_10 MARVIN_11 AND_12 NOLAN_13 ARRIVED_14

data=data[data$word==9|data$word==11|data$word==13,]

data.long=data

data.wide=reshape(data,idvar=c("experiment.x", "participant","item","condition"),v.names=c("wordlabel", "Constituent","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

data.wide$rpitch1=data.wide$maxpitch.11-data.wide$maxpitch.9
data.wide$rpitch2=data.wide$maxpitch.13-data.wide$maxpitch.11

### Create Residuals

# Residuals for focus

resid = data.long
data=data.long

for (i in 8:22) {
	col_count = i
	print(i)
	
	mod.lm = lmer(as.matrix(data[col_count])~Structure*Intonation*Constituent+phonelength+(1|participant), 	data=data,na.action=na.exclude)

	mod_resid = resid(mod.lm)
	resid[col_count]=as.matrix(mod_resid)
	}
		
resid.wide=reshape(resid,idvar=c("experiment.x", "participant","item","condition"),v.names=c("wordlabel", "Constituent","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

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
	
	mod.lm = lmer(as.matrix(data[col_count])~Focus*Intonation*Constituent+phonelength+(1|participant), 	data=data,na.action=na.exclude)

	mod_resid = resid(mod.lm)
	resid[col_count]=as.matrix(mod_resid)
	}
	
resid.wide=reshape(resid,idvar=c("experiment.x", "participant","item","condition"),v.names=c("wordlabel","Constituent", "phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

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
	
	mod.lm = lmer(as.matrix(data[col_count])~Focus*Structure*Constituent+phonelength+(1|participant), 	data=data,na.action=na.exclude)

	mod_resid = resid(mod.lm)
	resid[col_count]=as.matrix(mod_resid)
	}

resid.wide=reshape(resid,idvar=c("experiment.x", "participant","item","condition"),v.names=c("wordlabel", "Constituent","phonelength", "duration", "silence", "durasil", "begin","meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity"),timevar="word",direction="wide")

resid.wide$rpitch1=resid.wide$maxpitch.9-resid.wide$maxpitch.11
resid.wide$rpitch2=resid.wide$maxpitch.11-resid.wide$maxpitch.13

inton.long=resid
inton.wide=resid.wide


##
## Graphs: Intonation
## 

subset=inton.long
subset1=subset[subset$experiment.x=="suborn",]
subset2=subset[subset$experiment.x=="suborq",]

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
subset1=subset[subset$experiment.x=="suborn",]
subset2=subset[subset$experiment.x=="suborq",]
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

##
## Graphs: Structure
## 

subset=data.long
subset1=subset[subset$bracketing=="l",]
subset2=subset[subset$bracketing=="r",]

##
## Line graphs
##

# Structure effect

ggplot(subset, aes(x=Constituent, y=duration,group=Structure))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(. ~ Structure, scales = "fixed") + ylab("Duration")

# Structure*Intonation

ggplot(subset, aes(x=word, y=,maxpitch,group=Structure))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Structure ~ Intonation, scales = "fixed") 

# Structure*Focus

ggplot(subset, aes(x=word, y=,fourthpitch,group=Structure))+ stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line") + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Structure, scales = "fixed")


# Structure*Intonation*Structure
plot1=ggplot(subset1, aes(x=Constituent, y=duration)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + facet_grid(Focus ~ Intonation, scales = "fixed") + labs(title="(AB)C") + ylab("Duration")

plot2=ggplot(subset2, aes(x=Constituent, y=duration)) + stat_summary(fun.y=mean, geom="point") + 
stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + stat_summary(fun.data = "mean_cl_boot", geom="errorbar",size=0.6, width=.15,) + 
facet_grid(Focus ~ Intonation, scales = "fixed") + labs(title="A(BC)") + ylab("Duration")

multiplot(plot1,plot2,cols=2)




##

subset=resid.wide
subset1=resid.wide[data.wide$experiment.x=="suborn",]
subset2=resid.wide[data.wide$experiment.x=="suborq",]

p = position_dodge(0.7)
plot1=ggplot(subset, aes(x=experiment.x, y=rpitch1,fill=Focus,width=.6
))+
 stat_summary(fun.y = "mean", geom="bar", position = p) + 
 #coord_cartesian(ylim = c(-0.25, 0.25)) +
 stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15, position = p) +
 #scale_x_discrete("Condition") +
 #scale_y_continuous("Duration")+
scale_fill_grey()

plot2=ggplot(subset, aes(x=experiment.x, y=rpitch2,fill=Focus,width=.6
))+
 stat_summary(fun.y = "mean", geom="bar", position = p) + 
 #coord_cartesian(ylim = c(-0.25, 0.25)) +
 stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15, position = p) +
 #scale_x_discrete("Condition") +
 #scale_y_continuous("Duration")+
scale_fill_grey()

multiplot(plot1,plot2,cols=1)

#write.csv(center_resid,file="center_resid.csv")
write.csv(data.long, file="datalong.csv")
write.csv(data.wide,file="datawide.csv")