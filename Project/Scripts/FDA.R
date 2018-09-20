####################################################################################################
#
#   Apply Functional Data Analysis (FDA) to the 3-dimensions task
#
#Code adapted from 	Michele Gubian's Case study on Diphthong/Hiatus contrast in European Spanish 
#	website on FDA: http://lands.let.ru.nl/FDA
#
#	Licensed under GPLv3 (See http://www.gnu.org/licenses/gpl-3.0.html)
#
####################################################################################################



library(fda)
library(lattice)
library(ggplot2)
library(stringr)

#############1.Preprocess Data######################

#settings
root_dir = 'E:/Summer 2017/2_Stats/' # root dir for this experiment, change it.
FDA_dir="E:/Summer 2017/FDA-DH-master/"
plots_dir = paste(root_dir,'FDAplots/Focus/',sep='')
scripts_dir =  paste(FDA_dir,'scripts/',sep='')

source(paste(scripts_dir,'pca.fd.R',sep=''))
source(paste(scripts_dir,'landmarkreg.nocurve.R',sep=''))
source(paste(scripts_dir,'plot.pca.fd.corr.R',sep=''))



#Extract digital speaker IDs from the original seperate speaker names from the 2 files
regexp <- "[[:digit:]]+"
t_data$speaker=str_extract(t_data$speaker, regexp)


#Extract features from the dataset that are grouped by discourse name
#Features include: justified time with corresponsing relative F0 values,
#                  3 dimensions(focus,intonation,structure),speaker
#                  time points for 3 stressted vowels(landmarks)


time_list = list() # list for justified time
f0_list = list() #list for f0_relative
dur_f0 = c() # max justified time
speakers=c()
discourse=factor(t_data$discourse)
discourse_list=c()
focus=c()
intonation=c()
structure=c()
len_f0 = c() # number of samples 
stressed_time=c()
stressed_time_list=list()
i=1
for (id in levels(discourse)){
  sub_t_data=subset(t_data, discourse == id)
  #remove duplicate time points
  sub_t_data=sub_t_data[!duplicated(sub_t_data[,c('element_norm_time')]),]
  #find time points of the 3 stressted vowels
  for (phone in levels(factor(sub_t_data$phone[which(grepl(1, sub_t_data$phone))]))){
    stressed_time=c(stressed_time,min(sub_t_data$element_norm_time[which(sub_t_data$phone==phone)]))
  }
  if(length(stressed_time)<3){
    stressed_time=c()
    next
  }
  #lei time start from 0
  stressed_time_list[[i]]=sort(stressed_time)-4
  stressed_time=c()
  discourse_list=c(discourse_list,id)
  f0_list[[i]] = sub_t_data$F0_relative
  speakers[[i]]=sub_t_data$speaker[1]
  time_list[[i]] = sub_t_data$element_norm_time-4
  dur_f0 = c(dur_f0, max(time_list[[i]]))
  len_f0=c(len_f0,length(time_list[[i]]))
  focus=c(focus,sub_t_data$Focus[1]) #Levels: Third Second First Wide
  intonation=c(intonation,sub_t_data$Intonation[1])
  structure=c(structure,sub_t_data$Structure[1]) #Levels: (AB)C A(BC)
  
  i<-i+1
  if(i%%1000==0){print (i/100)}
  
}




n_levels=length(f0_list) #351 samples


#Find scales to use, and set graph parameters
#f0_relative(y_axis)
#cleaned_f0<-t_data$F0_relative[which(!is.na(t_data$F0_relative))] #exclude 0 ones
yup=ceiling(max(t_data$F0_relative)) #4.015632
ybot=floor(min(t_data$F0_relative)) #-4.753557

# structure_item1=read.csv(file="structure_item1.csv")
color = list ( "1" = 'blue',"2" = 'orangered')
symbol = list( "1" = 'L',"2" = 'R')
lty = list( "1" = 1,"2" = 2)
lwd  = list( "1" = 1,"2" = 2)
name = list( "1" = "(AB)C","2" = "A(BC)	")

# color = list ( "Declarative" = 'blue',"Interrogative" = 'orangered')
# symbol = list( "1" = 'T',"2" = 'F',"3"="S","4"="W")
# lty = list("Declarative" = 1,"Interrogative" =2 )
# lwd  = list( "Declarative" = 1,"Interrogative" =2 )
# name = list( "Declarative" = 'Declarative',"Interrogative" = 'Interrogative')

# set some graphic parameters
color = list ( "1" = 'blue',"2" = 'orangered',"3"="green4","4"="yellow")
symbol = list( "1" = 'T',"2" = 'F',"3"="S","4"="W")
lty = list( "1" = 1,"2" = 2,"3"=3,"4"=4)
lwd  = list( "1" = 1,"2" = 2,"3"=3,"4"=4)
name = list( "1" = "Third","2" = "First","3"="Second","4"="Wide")
# change color of trips in lattice plots
strip.background.col = trellis.par.get('strip.background')$col[4]; dev.off() # may open a blank plot window; just close it.



################## Analysis of f0 contours ########################

######## Some exploratory plots

# display raw data

subsamp= runif(n_levels) < 0.2 # randomly select 2%
# two versions, separated by class or not. Uncomment accordingly.
png(paste(plots_dir,'f0_raw.png',sep=''))
#png(paste(plots_dir,'f0_raw_nocol.png',sep=''))
i=1
plot(time_list[[i]],f0_list[[i]],type = 'n',xlim=c(0,3.5),ylim=c(ybot,yup),xlab='element_norm_time ',ylab='F0_relative',las=1,main = '',cex.axis=1.5,cex.lab=1.5)
for (i in (1:n_levels)[subsamp]) {
  #    lines(time_list[[i]],f0_list[[i]],col = 'black', lty=1,lwd=1)
  lines(time_list[[i]],f0_list[[i]],col = color[[intonation[i]]], lty=lty[[intonation[i]]], lwd=lwd[[intonation[i]]])
}
legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1.5)
dev.off()

# f0 speaker variability
# select two speakers, show that speaker variability in f0 contours is larger than class variability
index=c()
for (spk in c("1256","1253")) { #1082 and 1081 occurances
  for (class in c('1','2','3','4')) {
    for (i in (1:n_levels)){
      if(speakers[[i]]== spk & focus[[i]]==class & intonation[i]=="Declarative"& structure[i]=="1")
        index=c(index,i)
    }
    png(paste(plots_dir,'f0_raw_',spk,'_',class,'.png',sep=''))
    plot(time_list[[index[1]]],f0_list[[index[1]]],type = 'n',xlim=c(0,3.5),ylim=c(ybot,yup),xlab='element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
    for (i in index) { 
      lines(time_list[[i]],f0_list[[i]],col = 1)
    }
    dev.off()
    index=c()
  }
}

################## 2. Smoothing ######################################


trunc(median(len_f0)/2)
# GCV for smoothing
mean_dur_f0=3
n_knots_vec <- seq(10,80,10) # explore from 10 knots up to 80
loglam_vec <- seq(-4,2,1) # explore lambda from 10^(-4) to 10^2
gcv_err <- array(dim=c(n_levels,length(loglam_vec),length(n_knots_vec)),dimnames=c('items','lambda','knots'))
i_sample <- sample(1:n_levels,50) # a data subset, to save computation time

# compute GCV error for all (n_knots, lambda) combinations on the i_sample curves, store it in gcv_err (may take some minutes)
for (k in 1:length(n_knots_vec)) { 
  for (l in 1:length(loglam_vec)) { 
    norm_rng <- c(0,mean_dur_f0)
    knots <- seq(0,mean_dur_f0,length.out = n_knots_vec[k])
    Lfdobj <- 3 # 2 + order of derivative expected to be used. We need velocity to apply Xu's principle, thus order = 1
    norder <- 5 # 2 + Lfdobj 
    nbasis <- length(knots) + norder - 2 # a fixed relation about B-splines
    basis <- create.bspline.basis(norm_rng, nbasis, norder, knots)
    fdPar <- fdPar(basis, Lfdobj, 10^(loglam_vec[l]))
    for (i in i_sample) {
      # apply linear time normalization
      t_norm = time_list[[i]] 
      gcv_err[i,l,k] = smooth.basis(t_norm,f0_list[[i]],fdPar)$gcv
    }
  }
}    

# compute log of the median of gcv_err for each n_knots and lambda combination
gcv_log_err = log( apply(gcv_err,2:3,median, na.rm=T)) # median to protect from outliers

# plot log GCV errors on a grid
png(paste(plots_dir,'GCV_log_err_f0.png',sep='')) 
col.l <- colorRampPalette(c('blue', 'white'))(30)
levelplot( gcv_log_err, scales=list(y=list(at=1:length(n_knots_vec),labels=n_knots_vec,cex=1.5), x=list(at=1:length(loglam_vec),labels=sapply(loglam_vec,function(x) eval(substitute(expression(10^y) ,list(y=x)) )),cex=1.5) ),xlab = list(label = expression(lambda), cex=2), ylab = list(label= 'k',cex=2),col.regions=col.l,
           colorkey=list(label=list(at=-6:-3,label=sapply(-6:-3,function(x) eval(substitute(expression(10^y) ,list(y=x)) )),cex=1.5)),
           #aspect = "iso", shrink = c(0.7, 1),
           #colorkey=T
)
dev.off()

# min GCV error is in: [4:8],2
argmin = which(gcv_log_err==min(gcv_log_err), arr.ind=TRUE) # rows are lambda indices, cols are n_knots indices
# arg min log lambda is: 0.1
10^(loglam_vec[argmin[1]])    
# arg min n_knots is: 50
n_knots_vec[argmin[2]]

# Inspection of gcv_log_err for f0 shows that:
# min estimated gcv error is obtained basically at the highest number of knots and at low lambda.
# However, the captured detail looks too much (overfitting) for the forthcoming analysis.
# So, lambda and n_knots will be chosen by combining eye inspection of some curves (code below) and the guidance of the GCV figure (above).
# (See the paper for details)


for (loglam in c(-2,-1,1)) {
  for (n_knots in c(20)) {
    lambda = 10^(loglam) 
    norm_rng <- c(0,mean_dur_f0)
    knots <- seq(0,mean_dur_f0,length.out = n_knots)
    Lfdobj <- 3
    norder <- 5
    nbasis <- length(knots) + norder - 2
    basis <- create.bspline.basis(norm_rng, nbasis, norder, knots)
    fdPar <- fdPar(basis, Lfdobj,lambda)
    i=150 # select here a random curve
    t_norm = time_list[[i]] 
    y_fd = smooth.basis(t_norm,f0_list[[i]],fdPar)$fd
    png(paste(plots_dir,'f0_fit_loglam',loglam,'n_knots',n_knots,'.png',sep=''))
    plot(y_fd,xlab='Element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='red',lwd=3,ylim=c(ybot,yup))
    points(t_norm,f0_list[[i]],pch=20)
    #legend('topleft',legend= c(eval(substitute(expression(lambda == 10^x),list(x = loglam))),eval(substitute(expression(k == x),list(x = n_knots))) ),cex=2  )
    dev.off()
  }
}




i=150 #  a random curve
loglam = -1 # change values here 
lambda = 10^(loglam) 
n_knots = 20 # and here
rng_i = range(time_list[[i]])
knots <- seq(rng_i[1],rng_i[2],length.out = n_knots)
Lfdobj <- 3
norder <- 5
nbasis <- length(knots) + norder - 2
basis_i <- create.bspline.basis(rng_i, nbasis, norder, knots)
fdPar_i <- fdPar(basis_i, Lfdobj,lambda)
y_fd = smooth.basis(time_list[[i]],f0_list[[i]],fdPar_i)$fd
png(paste(plots_dir,'f0_orig_time_loglam',loglam,'n_knots',n_knots,'.png',sep=''))
plot(y_fd,xlab='time (ms)',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='red',lwd=3,ylim=c(ybot,yup))
legend('topleft',legend= c(eval(substitute(expression(lambda == 10^x),list(x = loglam))),eval(substitute(expression(k == x),list(x = n_knots))) ),cex=2  )
dev.off()
# plot first derivative (note: time axis is in ms, should convert to s in order to get st/s on the y axis).
# y(t), t in ms. If T in s, then t = 1000*T. dy(1000*T)/dT = 1000* dy(T)/dT = 1000* dy(t)/dt 
png(paste(plots_dir,'Df0_orig_time_loglam',loglam,'n_knots',n_knots,'.png',sep=''))
plot(1000*y_fd,Lfdobj=1,xlab='time (ms)',ylab='st/s',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='red',lwd=3)
legend('bottomright',legend= c(eval(substitute(expression(lambda == 10^x),list(x = loglam))),eval(substitute(expression(k == x),list(x = n_knots))) ),cex=2  )
dev.off()




# selected values:
lambda = 10^(-1) ; n_knots = 20
# build global f0 fd object
norm_rng <- c(0,3)
knots <- seq(0,3,length.out = n_knots)
Lfdobj <- 3
norder <- 5
nbasis <- length(knots) + norder - 2
basis <- create.bspline.basis(norm_rng, nbasis, norder, knots)
fdPar <- fdPar(basis, Lfdobj,lambda)
# convenient aliases
basis_f0 = basis
fdPar_f0 = fdPar
# smooth.basis() does not accept different time samples for different curves.
# Thus we create smooth curves one by one on the same basis, store the spline coefficients and compose an fd object at the end.
f0_coefs = matrix(nrow = nbasis, ncol = n_items)
for (i in 1:n_levels) {
  t_norm = time_list[[i]] 
  f0_coefs[,i] = c(smooth.basis(t_norm,f0_list[[i]],fdPar)$fd$coefs)
}
f0_fd = fd(coef=f0_coefs, basisobj=basis)
# curves are linearly time normalized, their duration is mean_dur_f0

# plot the curves
png(paste(plots_dir,'f0_lin.png',sep=''))
#png(paste(plots_dir,'f0_lin_nocol.png',sep=''))
plot(c(0,3),c(-4,6),type='n',xlab='element_norm_time (ms)',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
for (i in (1:n_levels)[subsamp]) {
  #lines(f0_fd[i],col = 'black', lty=1,lwd=1)
  lines(f0_fd[i],col = color[[focus[i]]], lty=lty[[focus[i]]], lwd=lwd[[focus[i]]])
}
legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1.5)
dev.off()


# this is how the B-spline basis looks
basis_fd = fd(diag(1,nbasis),basis)
png(paste(plots_dir,'B-splines.png',sep=''))
plot(norm_rng,c(0,1),type='n',xlab='time (s)', ylab = '', las=1,cex.axis=1.3,cex.lab=1.3)
for (b in 1:nbasis) {
  lines(basis_fd[b],col='red',lty=2)
}
points(knots,rep(0,n_knots),pch=19,col='blue')
dev.off()


# let us use these B-splines to represent the i-th curve
i=150 # select here a random curve
t_norm = time_list[[i]] 
y = f0_list[[i]]
y_fd = smooth.basis(t_norm,y,fdPar)$fd
# this is how the splines combine (sum) to approximate the given curve samples
png(paste(plots_dir,'B-splines_smoothing.png',sep=''))
plot(y_fd,lwd=2,col='red',xlab='time (s)', ylab = 'norm. st', las=1,cex.axis=1.3,cex.lab=1.3,ylim=c(-3,3))
points(t_norm,f0_list[[i]],pch=20,col='black')
for (b in 1:nbasis) {
  lines(y_fd$coefs[b] * basis_fd[b],col='red', lty=2)
}
dev.off()

################## 3.  Landmark Registration ###############################

# Use landmarkreg.nocurve(), a modified version of the landmarkreg() command. 
# It places knots according to de Boor's theorem, i.e. at landmark positions.
# It operates only on the landmark positions, not on the curves.
# It provides only the time warping curves, which have to be applied to the curves later on.
# It provides also relative rate curves (not used here).

# landmark matrix: 
# 3 internal landmark: stressed vowels

land = matrix(nrow = n_levels, ncol = 5) # one internal landmark + begin and end

for (i in 1:n_levels) {
  land[i,] = c(0,stressed_time_list[[i]][1],stressed_time_list[[i]][2],stressed_time_list[[i]][3],3) 
} 

reg = landmarkreg.nocurve(land, nhknots = n_knots) 
# nhknots are the used for the representation of h(t), not for the actual time warping
# other arguments are left at default values, since in this case registration is easy, having only one landmark (see command code for details).
# Registration may take some minutes.

# fd object for registered f0 contours
f0reg_coefs =  matrix(nrow = nbasis, ncol = 351)
reg_fdPar = fdPar(basis, Lfdobj,1e-12) # lambda small, since smoothing already occurred
# reg$hfunmat is a matrix whose i-th column contains the time samples h(x) for the i-th curve,
# where x (reg$x) are regularly spaced time samples along the registered time axis and h() is the time warping function 
# that returns the original time axis points.
# for (i in 1:n_levels) {
# 	h_i = reg$hfunmat[,i]
# 	f0reg_coefs[,i] = c(smooth.basis(reg$x, eval.fd(h_i,f0_fd[i]),reg_fdPar)$fd$coefs)
# }


for (i in 1:n_levels) {
  t_norm = time_list[[i]] 
  y = f0_list[[i]]
  f0reg_coefs[,i] = c(smooth.basis(t_norm,y,fdPar)$fd$coefs)
}
subsamp= runif(n_levels) < 0.1 # randomly select 2%
f0reg_fd = fd(coef=f0reg_coefs, basisobj=basis)

# Graphical parameters for landmark labels: place a label in the middle of every interval.
landlab = c("1st","2nd","3rd","rest") 
at_land = c() # position of the label along the time axis 
for (i in 1:(length(reg$land)-1)) {
  at_land = c(at_land, mean(reg$land[i:(i+1)]))
}

#png(paste(plots_dir,'f0_reg.png',sep=''))
png(paste(plots_dir,'f0_reg_nocol.png',sep=''))
plot(c(0,3),c(ybot,yup),type='n',xlab='Element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
for (i in (1:n_levels)[subsamp]) {
  
  lines(f0reg_fd[i],col = 'black', lty=1,lwd=1)
}
abline(v=reg$land[2],lty=2,lwd=1)
abline(v=reg$land[3],lty=2,lwd=1)
abline(v=reg$land[4],lty=2,lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
#legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1.5)
dev.off()



# inverse h(t)
h_inv_list = list()
for (i in 1:n_levels) {
  rng <- range(reg$land)
  steps <- seq(rng[1],rng[2],len=50)
  knots <- as.numeric(eval.fd(steps,reg$warpfd[i]))
  # rounding error quick fixes
  knots[1] = 0
  knots[length(steps)] = rng[2]
  norder <- 4
  nbasis <- length(knots) + norder - 2
  basis <- create.bspline.basis(rng, nbasis, norder, knots)
  Lfdobj <- 2
  lambda <- 10^1
  fdPar <- fdPar(basis, Lfdobj, lambda)
  h_inv_list[[i]] <- smooth.basis(knots,steps,fdPar)$fd
}

subsamp_small= (1:n_levels)[runif(n_levels) < 0.03] # select 2% of the dataset
# plot h(t) for some curves, show alignment 
png(paste(plots_dir,'h_sample.png',sep=''))
plot(reg$warpfd[subsamp_small],lty=1,lwd=1,las=1,xlab='reg. time ',ylab='element_norm_time',cex.axis=1.3,cex.lab=1.3,col='black',ylim =c(0,3))
for (i in subsamp_small) {
  points(eval.fd(land[i,2],h_inv_list[[i]]),land[i,2],col='red',pch=19,cex=1)
  points(eval.fd(land[i,3],h_inv_list[[i]]),land[i,3],col='red',pch=19,cex=1)
  points(eval.fd(land[i,4],h_inv_list[[i]]),land[i,4],col='red',pch=19,cex=1)
}
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
dev.off()



# plot some linearly registered curves, show landmark position
png(paste(plots_dir,'registration_lin.png',sep=''))
plot(range(reg$land),c(-3,3),type='n',xlab='element_norm_time',ylab='F0_relative',las=1,ylim=c(ybot,yup),cex.lab=1.3,cex.axis=1.3)
for (i in subsamp_small) {
  lines(f0_fd[i],lty=1,col=1)
  
  points(land[i,2],eval.fd(land[i,2],f0_fd[i]),col='red', pch=19,cex=1.3)
  points(land[i,3],eval.fd(land[i,3],f0_fd[i]),col='red', pch=19,cex=1.3)
  points(land[i,4],eval.fd(land[i,4],f0_fd[i]),col='red', pch=19,cex=1.3)
}
dev.off()

# plot the same curves after registration
png(paste(plots_dir,'registration_land.png',sep=''))
plot(range(reg$land),c(-3,3),type='n',xlab='element_norm_time',ylab='F0_relative',las=1,ylim=c(ybot,yup),cex.lab=1.3,cex.axis=1.3) 
for (i in subsamp_small) {
  lines(f0reg_fd[i],lty=1,col=1)
  t_reg = eval.fd(land[i,2],h_inv_list[[i]])
  points(t_reg,eval.fd(t_reg,f0reg_fd[i]),col='red', pch=19,cex=1.3)
  t_reg = eval.fd(land[i,3],h_inv_list[[i]])
  points(t_reg,eval.fd(t_reg,f0reg_fd[i]),col='red', pch=19,cex=1.3)
  t_reg = eval.fd(land[i,4],h_inv_list[[i]])
  points(t_reg,eval.fd(t_reg,f0reg_fd[i]),col='red', pch=19,cex=1.3)
}
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()


################## 4. Functional PCA on f0 contours ########################


y_fd = f0reg_fd # alias
# usually a good solution is obtained by setting the same lambda and knots (thus basis) used for smoothing
lambda_pca    <- lambda
pcafdPar  <- fdPar(basis_f0, 2, lambda_pca)
f0_pcafd <- pca.fd(y_fd, nharm=3, pcafdPar) # first three PCs
#f0_pcafd = f0_pcafd # alias


# store PC scores in a new data frome and save it with  the other features

f0_s1 = f0_pcafd$scores[,1]
f0_s2 = f0_pcafd$scores[,2]
stressed_time_matrix=matrix(unlist(stressed_time_list), byrow=TRUE, nrow=length(stressed_time_list) )
structure_item1=data.frame(discourse_list,speakers,stressed_time_matrix,structure,f0_s1,f0_s2)
write.csv(structure_item1, file = "structure_item1.csv",row.names=FALSE)


# plot PC curves
plot.pca.fd.corr(f0_pcafd,xlab = 'element_norm_time',ylab='F0_relative',land = reg$land , nx=40,plots_dir = plots_dir, basename = 'PCA_f0reg_',height=480)



# plot PC scores 
png(paste(plots_dir,'PCsplom_f0reg.png',sep=''))
splom(f0_pcafd$scores ,
      groups=structure_item1$focus,
      # in lattice plot functions, the following complex sapply() expression is necessary
      # in order to get the order of groups graphical parameters right.
      pch  = sapply(c(1,2), function(x) symbol[[x]],USE.NAMES = FALSE),
      col  = sapply(c(1,2), function(x) color[[x]],USE.NAMES = FALSE),
      cex=0.8, varnames= c(expression(s[1]),expression(s[2]),expression(s[3])) )
dev.off()

# plot only the first two PC scores
# grouped by class
png(paste(plots_dir,'str_PCscatter_f0reg.png',sep=''))
xyplot(f0_pcafd$scores[,2] ~  f0_pcafd$scores[,1] , cex=1.5,
       xlab = list(label=expression(s[1]),cex=2), ylab= list(label=expression(s[2]),cex=2), 
       groups= structure_item1$structure,
       pch  = sapply(levels(factor(structure_item1$structure)), function(x) symbol[[x]],USE.NAMES = FALSE),
       col  = sapply(levels(factor(structure_item1$structure)), function(x) color[[x]],USE.NAMES = FALSE),
       ,scales = list(cex=1.5)
)
dev.off()

# the original PCA output, i.e. no classes
png(paste(plots_dir,'PCscatter_f0reg_allblack.png',sep=''))
xyplot(f0_pcafd$scores[,2] ~  f0_pcafd$scores[,1] , cex=1,
       xlab = list(label=expression(s[1]),cex=2), ylab= list(label=expression(s[2]),cex=2), 
       col  = 'black',pch=20,scales = list(cex=1.5)
)
dev.off()

# PC scores by class and speaker
# see http://tolstoy.newcastle.edu.au/R/e2/help/07/09/24852.html for the use of panel 
# png(paste(plots_dir,'PCscatter_f0reg_speaker.png',sep=''))
# xyp = xyplot(f0_pcafd$scores[,2] ~  f0_pcafd$scores[,1] | new_speakers , groups= intonation,
# xlab = list(label=expression(s[1]),cex=1.5), ylab= list(label=expression(s[2]),cex=1.5),cex=1,
# 	col = sapply(c(1,2), function(x) color[[x]],USE.NAMES = FALSE),
# 	pch = sapply(c(1,2), function(x) symbol[[x]],USE.NAMES = FALSE),
# panel = panel.superpose,
# panel.groups = function(...) {
# panel.xyplot(...)
# panel.abline(h=0,lty=2,col='grey')
# panel.abline(v=0,lty=2,col='grey')
# }
# )
# update	(xyp, par.settings=list(
# 	        par.xlab.text = list(cex=1.3),
# 	        par.ylab.text = list(cex=1.3),
# 	        strip.background = list(col=strip.background.col)
# 	    ),
#         as.table=TRUE        
#     )
# dev.off()

# boxplots for PC scores 
# (manually put s1 or s2, scores[,1 or 2] and s[1] or s[2] accordingly)

png(paste(plots_dir,'s1_f0reg_spk_box.png',sep=''))
bwp = bwplot(  f0_pcafd$scores[,1] ~ focus | structure_item1$speakers, ylab = expression(s[1]))
update	(bwp, par.settings=list(
  par.xlab.text = list(cex=1.3),
  par.ylab.text = list(cex=1.3),
  strip.background = list(col=strip.background.col)
),
scales=list(x=list(labels=sapply(c(1,2), function(x) symbol[[x]],USE.NAMES = FALSE),cex=1.3),y=list(cex=1.3)),
as.table=TRUE
)
dev.off()


# plot class-specific mean curves

t_f0 = reg$x
f0_pcafd = f0_pcafd 

#png(paste(plots_dir,'f0_mean.png',sep=''))
png(paste(plots_dir,'f0_lm_f0_s2.png',sep=''))
plot(c(0,3),c(-3,3),type='n',xlab='element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
for (class in names(color)) {
  lines(f0_pcafd$meanfd + mean(f0_pcafd$scores[which(focus == class),1]) * f0_pcafd$harmonics[1] + mean(f0_pcafd$scores[which(focus == class),2]) * f0_pcafd$harmonics[2],col = color[[class]], lwd = lwd[[class]], lty = lty[[class]])
}

abline(v=reg$land[2],lty=2)
abline(v=reg$land[3],lty=2)
abline(v=reg$land[4],lty=2)
legend('topleft',legend=unlist(name),col=unlist(color),lwd=unlist(lwd),lty=unlist(lty),cex=1)
dev.off()



# plot class- and speaker-specific mean curves
png(paste(plots_dir,'str_s2_f0_mean_spk.png',sep=''))
table_plot = expand.grid(class = c("1","2"),spk = c("107","207","930","933","1067","1141","1244","1253","1254"),stringsAsFactors = FALSE)

curves = matrix(nrow = length(t_f0),ncol = nrow(table_plot))
for (i in 1:nrow(table_plot)) {
  curve = f0_pcafd$meanfd +
  mean(f0_pcafd$scores[which(structure_item1$structure == table_plot$class[i] & structure_item1$speakers == table_plot$spk[i]),2]) * f0_pcafd$harmonics[2] 
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
  ylab = 'F0_relative',
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

# mean
png(paste(plots_dir,'mean','.png',sep=''))
plot(f0_pcafd$meanfd,xlab='Element_norm_time ',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=c(ybot,yup))
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()

#PC1
png(paste(plots_dir,'PC1','.png',sep=''))
plot(f0_pcafd$harmonics[1],xlab='Element_norm_time ',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()

#PC2
png(paste(plots_dir,'PC2','.png',sep=''))
plot(f0_pcafd$harmonics[2],xlab='Element_norm_time ',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
dev.off()

#reconstruct with only mean
i = 203
png(paste(plots_dir,'reconstr_mean','.png',sep=''))
plot(f0_pcafd$meanfd,xlab='Element_norm_time ',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=c(-1,3))
lines(f0reg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()


#reconstruct with mean+PC1
png(paste(plots_dir,'reconstr_mean_PC1','.png',sep=''))
plot(f0_pcafd$meanfd + f0_pcafd$scores[i,1] * f0_pcafd$harmonics[1] ,xlab='element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=c(-1,3))
lines(f0reg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()

#reconstruct with mean+PC2
png(paste(plots_dir,'reconstr_mean_PC2','.png',sep=''))
plot(f0_pcafd$meanfd+f0_pcafd$scores[i,2] * f0_pcafd$harmonics[2],xlab='element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=c(-1,3))
lines(f0reg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()


#reconstruct with mean+PC1+PC2
png(paste(plots_dir,'reconstr_mean_PC1_PC2','.png',sep=''))
plot(f0_pcafd$meanfd+ f0_pcafd$scores[i,1] * f0_pcafd$harmonics[1]+f0_pcafd$scores[i,2] * f0_pcafd$harmonics[2],xlab='element_norm_time',ylab='F0_relative',main = '',las=1,cex.axis=1.5,cex.lab=1.5,col='black',lwd=3,ylim=c(-1,3))
lines(f0reg_fd[i],lwd=2, lty=2)
abline(v=reg$land[2],lty=2,col='black',lwd=1)
abline(v=reg$land[3],lty=2,col='black',lwd=1)
abline(v=reg$land[4],lty=2,col='black',lwd=1)
axis(3,tick=F,at=at_land, labels=landlab,cex.axis=1.5)
legend('topleft',legend=c('original','reconstruction'),lty=c(2,1),lwd=c(2,3))
dev.off()
