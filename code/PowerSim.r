##################################################################################################################
#                                  0 - Load packages and Set Simulation Parameters                               #
##################################################################################################################
library(rgl) # for 3D plotting
library(ggplot2) # for 2D plotting

sample_size_range<-seq(5,25,1); length(sample_size_range) # vector of sample sizes to iterate over
effect_size_range<-seq(.05,.25,.01); length(effect_size_range) # vector of effect sizes to iterate over

iter<-1000 # number of iterations for each effect_size-sample_size combination
b0<-10 # constant term in regression
 fonc = 12
##################################################################################################################
#                                  1 - Simulation function to estimate p-value                                   #
##################################################################################################################
sim<-function(n){
  
  y<-b0+b1*x1+rnorm(n=n,mean=0,sd=1) # the data generation process (DGP) with given effect size, b1
  
  regest<-summary(lm(y~x1))$coefficients # estimate pvalue of b1
  
  pval<-regest[2,4] # extract and output pvalue

  return(pval)
}

# empty matrix to store sim results
pval_mat<-matrix(nrow=length(effect_size_range),ncol=length(sample_size_range)) 
rownames(pval_mat)<-effect_size_range
colnames(pval_mat)<-sample_size_range

##################################################################################################################
#                                  2 - Run Simulation for Each EffectSize and SampleSize                         #
##################################################################################################################

eff_count<-0 # iteration counter
for(effect_size in effect_size_range){ # iterate over effect size
  
  eff_count<-eff_count+1
  samp_count<-0 #iteration counter
  for(sample_size in sample_size_range){ # for each effect size, iterate over all sample sizes
    samp_count<-samp_count+1
  
    set.seed(1)
    
    x1<-rnorm(n = sample_size,mean = 100, sd = 20)
    b1<-effect_size
    
    # call sim function 1000 times for each effect size, for each sample size.
    res<-replicate(iter,sim(n=sample_size))
    
    # store average of 
    pval_mat[eff_count,samp_count]<-mean(res)
  }
}

##################################################################################################################
#                                  3 - Transform data into long form, for plotting                               #
##################################################################################################################

transpose<-numeric(0)
for(col in 1:ncol(pval_mat)){ 
  transpose<-rbind(transpose,cbind(sample_size_range,pval_mat[,col],effect_size_range[col])) 
}
transpose<-as.data.frame(transpose)
names(transpose)<-c('Sample Size','Mean P-Value','Effect Size')

##################################################################################################################
#                                  4 - Plot Results                                                              #
##################################################################################################################

# plot relationship between pvalue and sample size for each effect size
ggplot(transpose, aes(x=`Sample Size`, y=`Mean P-Value`,group=`Effect Size`))+
  geom_line(aes(colour = `Effect Size`))+
  scale_y_continuous(breaks=seq(0,.25,.05))+
  scale_color_gradient(aes(colour= `Effect Size`),guide = guide_colorbar(reverse=TRUE,title='Effect Size'))+
  geom_hline(aes(yintercept=.10),col='red',linetype=4)+
  geom_hline(aes(yintercept=.05),col='red',linetype=4)+
  geom_hline(aes(yintercept=.01),col='red',linetype=4)+
  ggtitle('Mean Simulated P-Value by Sample Size and Effect Size')+
  theme_grey(base_size = 13)

# plot pval ~ effect_size + sample_size surface
persp(x=sample_size_range,y=effect_size_range,z=pval_mat,col='lightblue',
      theta = 50, phi=10, axes='TRUE',ticktype = 'detailed',
      xlab="Sample Size",ylab="Effect Size",zlab="P-Value",main = "P-Value as a Function of Sample Size and Effect Size",
      shade=TRUE,
      d=4,r=2)
