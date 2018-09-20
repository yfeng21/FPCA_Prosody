####################################################################################################
#
#   Functional Data Analysis (FDA) - Case study on Diphthong/Hiatus contrast in European Spanish 
#
#	Michele Gubian (PhD)
#	Centre for Language and Speech Technology
#	Radboud University Nijmegen
#	email: m.gubian@let.ru.nl, michele.gubian@gmail.com
#	website on FDA: http://lands.let.ru.nl/FDA
#
#	Licensed under GPLv3 (See http://www.gnu.org/licenses/gpl-3.0.html)
#
#   This script allows one to reproduce the FDA procedure described in the paper:
#   "Using Functional Data Analysis for investigating multidimensional dynamic phonetic contrasts" by M. Gubian, F. Torreira and L. Boves. 
#   A draft version of the paper is available in the paper/ directory.
#
####################################################################################################


library(fda)
library(lattice)
library(ggplot2)
library(stringr)

regexp <- "[[:digit:]]+"
t_data$speaker=str_extract(t_data$speaker, regexp)

t_data_2$speaker=str_extract(t_data_2$speaker, regexp)

root_dir = 'E:/Summer 2017/2_Stats/' # root dir for this experiment, change it.
FDA_dir="E:/Summer 2017/FDA-DH-master/"
plots_dir = paste(root_dir,'FDAplots/word2/',sep='')
scripts_dir =  paste(FDA_dir,'scripts/',sep='')

# use pca.fd version from package fda_2.2.5.tar.gz or earlier (you find a copy in the scripts/ dir)
source(paste(scripts_dir,'pca.fd.R',sep=''))
# this is a modified version of the landmarkreg() command 
source(paste(scripts_dir,'landmarkreg.nocurve.R',sep=''))
# this is a slightly modified version of the plot.pca.fd() command,
# but you can also use the standard one.
source(paste(scripts_dir,'plot.pca.fd.corr.R',sep=''))

my_data=t_data

# my_data=t_data_2


# f0 and formants
# all raw contours start 30ms before the beginning of /l/
# samples are every 5ms
# get rid of the first 6 samples (= start at beginning of /l/) for f0
# start at beginning of vowel sequence /ja/ or /i.a/ for formants

time_list = list() # list for justified time
f0_list = list() #for Residualized F0
res_focus=list()
res_int=list()
res_str=list()
dur_f0 = c() # max justified time
speakers=c()
discourse=factor(my_data$discourse)
discourse_list=c()
focus=c()
intonation=c()
structure=c()
len_f0 = c() # number of samples 
stressed_time=c()
stressed_time_list=list()
i=1
for (id in levels(discourse)){
  sub_t_data=subset(my_data, discourse == id)
  sub_t_data=sub_t_data[!duplicated(sub_t_data[,c('element_norm_time')]),]
  for (phone in levels(factor(sub_t_data$phone[which(grepl(1, sub_t_data$phone))]))){
    stressed_time=c(stressed_time,min(sub_t_data$element_norm_time[which(sub_t_data$phone==phone)]))#,max(sub_t_data$element_norm_time[which(sub_t_data$phone==phone)]))
  }
  if(length(stressed_time)<3){
    stressed_time=c()
    next
  }
  # if(stressed_time[2]<0.3){
  #   stressed_time=c()
  #   next
  # }
  stressed_time_list[[i]]=sort(stressed_time)
  stressed_time=c()
  discourse_list=c(discourse_list,id)
  f0_list[[i]] = sub_t_data$F0_relative
  res_focus[[i]]=sub_t_data$Focus_res
  res_int[[i]]=sub_t_data$Int_res
  res_str[[i]]=sub_t_data$Str_res
  speakers[[i]]=sub_t_data$speaker[1]
  time_list[[i]] = sub_t_data$element_norm_time
  dur_f0 = c(dur_f0, max(time_list[[i]]))
  len_f0=c(len_f0,length(time_list[[i]]))
  focus=c(focus,sub_t_data$Focus[1]) #Levels: Third Second First Wide
  intonation=c(intonation,sub_t_data$Intonation[1])
  structure=c(structure,sub_t_data$Structure[1]) #Levels: (AB)C A(BC)
  
  i<-i+1
  if(i%%1000==0){print (i/100)}
  
}




n_levels=length(f0_list)


########Finding scales to use##############


# set some graphic parameters
# this is convenient in order to get the label assignments right when using the groups argument in xyplot, splom etc.

#Structure
color = list ( "1" = 'blue',"2" = 'orangered')
symbol = list( "1" = 'L',"2" = 'R')
lty = list( "1" = 1,"2" = 2)
lwd  = list( "1" = 1,"2" = 2)
name = list( "1" = "(AB)C","2" = "A(BC)")

target<-structure
res_list<-res_str #structure

#Intonation
color = list ( "1" = 'blue',"2" = 'orangered')
symbol = list( "1" = 'D',"2" = 'I')
lty = list("1" = 1,"2" =2 )
lwd  = list( "1" = 1,"2" =2 )
name = list( "1" = 'Declarative',"2" = 'Interrogative')
res_list<-res_int;target<-intonation #intonation

#Focus
color = list ( "1" = 'blue',"2" = 'orangered',"3"="green4","4"="yellow")
symbol = list( "1" = 'T',"2" = 'F',"3"="S","4"="W")
lty = list( "1" = 1,"2" = 2,"3"=3,"4"=4)
lwd  = list( "1" = 1,"2" = 2,"3"=3,"4"=4)
name = list( "1" = "Third","2" = "First","3"="Second","4"="Wide")

f0_list<-res_focus #focus

# change color of trips in lattice plots
strip.background.col = trellis.par.get('strip.background')$col[4]; dev.off() # may open a blank plot window; just close it.

#Residualized F0(y_axis)
#cleaned_f0<-t_data$Residualized F0[which(!is.na(t_data$Residualized F0))] #exclude 0 ones
yup=ceiling(max(my_data$Str_res)) #4.015632
ybot=floor(min(my_data$Str_res)) #-4.753557

################## Smoothing ######################################
subsamp= runif(n_levels) < 0.2 # randomly select 2%
mean_dur_f0=mean(dur_f0)

# selected values:
lambda = 10^(-4) ; n_knots = 20
# build global f0 fd object
norm_rng <- c(0,mean_dur_f0)
knots <- seq(0,mean_dur_f0,length.out = n_knots)
Lfdobj <- 2
norder <- 4
nbasis <- length(knots) + norder - 2
basis <- create.bspline.basis(norm_rng, nbasis, norder, knots)
fdPar <- fdPar(basis, Lfdobj,lambda)
# convenient aliases
basis_res = basis
fdPar_res = fdPar
# smooth.basis() does not accept different time samples for different curves.
# Thus we create smooth curves one by one on the same basis, store the spline coefficients and compose an fd object at the end.
res_coefs = matrix(nrow = nbasis, ncol = n_levels)
for (i in 1:n_levels) {
  t_norm = (time_list[[i]] / dur_f0[i]) * mean_dur_f0
  res_coefs[,i] = c(smooth.basis(t_norm,res_list[[i]],fdPar)$fd$coefs)
}
res_fd = fd(coef=res_coefs, basisobj=basis)
# curves are linearly time normalized, their duration is mean_dur_f0

# plot the curves
png(paste(plots_dir,'f0_lin.png',sep=''))
#png(paste(plots_dir,'f0_lin_nocol.png',sep=''))
plot(c(0,mean_dur_f0),c(-5,4),type='n',xlab='element_norm_time (ms)',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
for (i in (1:n_levels)[subsamp]) {
  #lines(res_fd[i],col = 'black', lty=1,lwd=1)
  lines(res_fd[i],col = color[[target[i]]], lty=lty[[target[i]]], lwd=lwd[[target[i]]])
}
legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1.5)
dev.off()




# let us use these B-splines to represent the i-th curve
i=150 # select here a random curve
basis_fd = fd(diag(1,nbasis),basis)
t_norm = (time_list[[i]] / dur_f0[i]) * mean_dur_f0
y = res_list[[i]]
y_fd = smooth.basis(t_norm,y,fdPar)$fd
# this is how the splines combine (sum) to approximate the given curve samples
png(paste(plots_dir,'B-splines_smoothing.png',sep=''))
plot(y_fd,lwd=2,col='red',xlab='time (s)', ylab = 'norm. st', las=1,cex.axis=1.3,cex.lab=1.3,ylim=c(-3,3))
points(t_norm,res_list[[i]],pch=20,col='black')
for (b in 1:nbasis) {
  lines(y_fd$coefs[b] * basis_fd[b],col='red', lty=2)
}
dev.off()

################## Landmark Registration ###############################

# Use landmarkreg.nocurve(), a modified version of the landmarkreg() command. 
# It places knots according to de Boor's theorem, i.e. at landmark positions.
# It operates only on the landmark positions, not on the curves.
# It provides only the time warping curves, which have to be applied to the curves later on.
# It provides also relative rate curves (not used here).

# landmark matrix: 
# one internal landmark: end of /l/
# landmarkreg.nocurve() requires also beginning and end of the token to be included in the landmark matrix.
# because in general the total duration may differ (not in this case, since linear registration already occured).

land = matrix(nrow = n_levels, ncol = length(stressed_time_list[[i]])+2) # one internal landmark + begin and end

for (i in 1:n_levels) {
  land[i,] = c(0,stressed_time_list[[i]]/dur_f0[[i]],1) * mean_dur_f0
} 

reg = landmarkreg.nocurve(land, nhknots = n_knots) 
# nhknots are the used for the representation of h(t), not for the actual time warping
# other arguments are left at default values, since in this case registration is easy, having only one landmark (see command code for details).
# Registration may take some minutes.

# fd object for registered f0 contours
resreg_coefs =  matrix(nrow = nbasis, ncol = n_levels)
reg_fdPar = fdPar(basis, Lfdobj,1e-12) # lambda small, since smoothing already occurred
# reg$hfunmat is a matrix whose i-th column contains the time samples h(x) for the i-th curve,
# where x (reg$x) are regularly spaced time samples along the registered time axis and h() is the time warping function 
# that returns the original time axis points.
for (i in 1:n_levels) {
  h_i = reg$hfunmat[,i]
  resreg_coefs[,i] = c(smooth.basis(reg$x, eval.fd(h_i,res_fd[i]),reg_fdPar)$fd$coefs)
}

resreg_fd = fd(coef=resreg_coefs, basisobj=basis)

# Graphical parameters for landmark labels: place a label in the middle of every interval.
landlab = c("1st","2nd","3rd","rest") 
at_land = c() # position of the label along the time axis 
for (i in 1:(length(reg$land)-1)) {
  at_land = c(at_land, mean(reg$land[i:(i+1)]))
}

png(paste(plots_dir,'f0_reg.png',sep=''))
# png(paste(plots_dir,'f0_reg_nocol.png',sep=''))
plot(range(reg$land),c(ybot,yup),type='n',xlab='Element_norm_time',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
for (i in (1:n_levels)[subsamp]) {
  
  # lines(resreg_fd[i],col = 'black', lty=1,lwd=1)
  lines(resreg_fd[i],col = color[[target[i]]], lty=lty[[target[i]]], lwd=lwd[[target[i]]])
}
abline(v=reg$land[2],lty=2,lwd=1)
abline(v=reg$land[3],lty=2,lwd=1)
abline(v=reg$land[4],lty=2,lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1.5)
dev.off()



# inverse h(t)
h_inv_list = list()
for (i in 1:n_levels) {
  steps <- seq(norm_rng[1],norm_rng[2],len=50)
  hknots <- as.numeric(eval.fd(steps,reg$warpfd[i]))
  # rounding error quick fixes
  hknots[1] = 0
  hknots[length(steps)] = norm_rng[2]
  hnbasis=norder + 50 - 2
  hbasis <- create.bspline.basis(norm_rng, hnbasis, norder, hknots)
  hfdPar <- fdPar(hbasis, Lfdobj, lambda)
  h_inv_list[[i]] <- smooth.basis(hknots,steps,hfdPar)$fd
}



# plot some linearly registered curves, show landmark position
png(paste(plots_dir,'registration_lin.png',sep=''))
plot(range(reg$land),c(-3,3),type='n',xlab='element_norm_time',ylab='Residualized F0',las=1,ylim=c(ybot,yup),cex.lab=1.3,cex.axis=1.3)
for (i in subsamp_small) {
  lines(res_fd[i],lty=1,col=1)
  
  points(land[i,2],eval.fd(land[i,2],res_fd[i]),col='red', pch=19,cex=1.3)
  points(land[i,3],eval.fd(land[i,3],res_fd[i]),col='red', pch=19,cex=1.3)
  points(land[i,4],eval.fd(land[i,4],res_fd[i]),col='red', pch=19,cex=1.3)
}
dev.off()
# plot the same curves after registration
png(paste(plots_dir,'registration_land.png',sep=''))
plot(range(reg$land),c(-3,3),type='n',xlab='element_norm_time',ylab='Residualized F0',las=1,ylim=c(ybot,yup),cex.lab=1.3,cex.axis=1.3) 
for (i in subsamp_small) {
  lines(resreg_fd[i],lty=1,col=1)
  t_reg = eval.fd(land[i,2],h_inv_list[[i]])
  points(t_reg,eval.fd(t_reg,resreg_fd[i]),col='red', pch=19,cex=1.3)
  t_reg = eval.fd(land[i,3],h_inv_list[[i]])
  points(t_reg,eval.fd(t_reg,resreg_fd[i]),col='red', pch=19,cex=1.3)
  t_reg = eval.fd(land[i,4],h_inv_list[[i]])
  points(t_reg,eval.fd(t_reg,resreg_fd[i]),col='red', pch=19,cex=1.3)
}
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()


################## Functional PCA on f0 contours ########################

y_fd = resreg_fd # alias, line 420
lambda_pca    <- lambda
pcafdPar  <- fdPar(basis_res,int2Lfd(2), lambda_pca) #basis_f0 at line 320
res_pcafd <- pca.fd(y_fd, nharm=3, pcafdPar) # first three PCs

#check PC1
plot(res_pcafd$harmonics[1],xlab='Element_norm_time ',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,)

# store PC scores 

f0_s1 = res_pcafd$scores[,1]
f0_s2 = res_pcafd$scores[,2]
stressed_time_matrix=matrix(unlist(stressed_time_list), byrow=TRUE, nrow=length(stressed_time_list) )
item1=data.frame(discourse_list,speakers,stressed_time_matrix,target,f0_s1,f0_s2)
write.csv(item1, file = "res_structure",row.names=FALSE)

item1=read.csv("res_structure.csv")
classes=item1$target



# plot PC curves
plot.pca.fd.corr(res_pcafd,xlab = 'element_norm_time',ylab='Residualized F0',land = reg$land , nx=40,plots_dir = plots_dir, basename = 'PCA_f0reg_',height=480)




# plot only the first two PC scores
# grouped by class
png(paste(plots_dir,'PCscatter_f0reg.png',sep=''))
xyplot(res_pcafd$scores[,2] ~  res_pcafd$scores[,1] , cex=1.5,
       xlab = list(label=expression(s[1]),cex=2),ylab= list(label=expression(s[2]),cex=2), 
       groups= classes,
       pch  = sapply(levels(factor(classes)), function(x) symbol[[x]],USE.NAMES = FALSE),
       col  = sapply(levels(factor(classes)), function(x) color[[x]],USE.NAMES = FALSE),
       ,scales = list(cex=1.5)
)
dev.off()


# plot class-specific mean curves

t_f0 = reg$x
res_pcafd = res_pcafd 

#png(paste(plots_dir,'f0_mean.png',sep=''))
png(paste(plots_dir,'f0_lm_f0_s2.png',sep=''))
plot(range(reg$land),c(-1,0.5),type='n',xlab='element_norm_time',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
for (class in names(color)) {
  lines(res_pcafd$meanfd + mean(res_pcafd$scores[which(target == class),1]) * res_pcafd$harmonics[1] + mean(res_pcafd$scores[which(target == class),2]) * res_pcafd$harmonics[2],col = color[[class]], lwd = lwd[[class]], lty = lty[[class]])
}

# # or use values from linear model f0_s2_class.lm (see script DA.R)
# mean_lm_f0_s2 = list("1" = 0.005149841, "2" =  0.02914387)
# for (class in names(color)) {
#     lines(res_pcafd$meanfd + mean_lm_f0_s2[[class]]  * res_pcafd$harmonics[2],col = color[[class]], lwd = lwd[[class]], lty = lty[[class]])
# }
abline(v=reg$land[2],lty=2)
abline(v=reg$land[3],lty=2)
abline(v=reg$land[4],lty=2)
legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1)
dev.off()



# plot class- and speaker-specific mean curves
# see xyplot.ts
png(paste(plots_dir,'f0_mean_spk.png',sep=''))
table_plot = expand.grid(class = c("1","2"),spk = c('1','5','8','11','16','18','21','28','34'),stringsAsFactors = FALSE)

curves = matrix(nrow = length(t_f0),ncol = nrow(table_plot))
for (i in 1:nrow(table_plot)) {
  curve = res_pcafd$meanfd +
    # choose which PC you want to include
    mean(res_pcafd$scores[which(item1$target == table_plot$class[i] & item1$speakers == table_plot$spk[i]),2]) * res_pcafd$harmonics[2] 
  mean(res_pcafd$scores[which(item1$target == table_plot$class[i] & item1$speakers == table_plot$spk[i]),1]) * res_pcafd$harmonics[1] 
  curves[,i] = eval.fd(t_f0,curve)
}
xyp =	xyplot(
  ts(data=curves,start=t_f0[1],deltat=t_f0[2]-t_f0[1]),
  screens=table_plot$spk,
  col = sapply(table_plot$class, function(x) color[[x]],USE.NAMES = FALSE),
  lty = sapply(table_plot$class, function(x) lty[[x]],USE.NAMES = FALSE),
  lwd = sapply(table_plot$class, function(x) lwd[[x]],USE.NAMES = FALSE),
  layout = c(3,3),
  xlab = 'element_norm_time',
  ylab = 'Residualized F0',
  default.scales = list(relation='same',cex=1.0),
  panel = function(col,lty,lwd,...) {
    panel.superpose.plain(col=col,lty=lty,lwd=lwd,...)
    panel.abline(v=reg$land[2],lty=2,col='black',lwd=1)
    panel.abline(v=reg$land[3],lty=2,col='black',lwd=1)
    panel.abline(v=reg$land[4],lty=2,col='black',lwd=1)
  }
)
update	(xyp, par.settings=list(
  par.xlab.text = list(cex=1.3),
  par.ylab.text = list(cex=1.3),
  strip.background = list(col=strip.background.col)
),
key = list(
  space = 'top',
  lines = list(
    col = as.character(unlist(color)),
    lty = as.numeric(unlist(lty)),
    lwd = as.numeric(unlist(lwd))
  ),
  text = list(
    lab = as.character(unlist(name)),
    cex = 1.2
  )
),
as.table=TRUE
)
dev.off()





## FPCA-based reconstruction example (6 plots)

png(paste(plots_dir,'mean','.png',sep=''))
plot(res_pcafd$meanfd,xlab='Element_norm_time ',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=c(-1,0.5))
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()


png(paste(plots_dir,'PC1','.png',sep=''))
plot(res_pcafd$harmonics[1],xlab='Element_norm_time ',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()


png(paste(plots_dir,'PC2','.png',sep=''))
plot(res_pcafd$harmonics[2],xlab='Element_norm_time ',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()

png(paste(plots_dir,'PC3','.png',sep=''))
plot(res_pcafd$harmonics[3],xlab='Element_norm_time ',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()

i = 121
lim=c(-1.5,0.5)
png(paste(plots_dir,'reconstr_mean','.png',sep=''))
plot(res_pcafd$meanfd,xlab='Element_norm_time ',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=lim)
lines(resreg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()



png(paste(plots_dir,'reconstr_mean_PC1','.png',sep=''))
plot(res_pcafd$meanfd + res_pcafd$scores[i,1] * res_pcafd$harmonics[1] ,xlab='element_norm_time',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=lim)
lines(resreg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()


png(paste(plots_dir,'reconstr_mean_PC2','.png',sep=''))
plot(res_pcafd$meanfd+res_pcafd$scores[i,2] * res_pcafd$harmonics[2],xlab='element_norm_time',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=lim)
lines(resreg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()

png(paste(plots_dir,'reconstr_mean_PC1_PC2','.png',sep=''))
plot(res_pcafd$meanfd+ res_pcafd$scores[i,1] * res_pcafd$harmonics[1]+res_pcafd$scores[i,2] * res_pcafd$harmonics[2],xlab='element_norm_time',ylab='Residualized F0',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=lim)
lines(resreg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()

