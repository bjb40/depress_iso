#R Version 3.2.2 "Fire Safety"
#Bryce Bartlett
#pulls and cleans PROSPER data for SEM analysis

#@@@@@@@@@@@@@@@@@@@@@
#Preliminaries
#@@@@@@@@@@@@@@@@@@@@@
#clear cache
rm(list=ls())

#libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lme4) #random effects mixed models
library(plm) #individual fixed effects, panel models

#load directory file and functions
source("~config.R"); source('funs.R')
#imgdir = for images
#outdir = for output
#rawdir = for raw data

#load previously cleaned data
load(paste0(outdir,'cleandat~/cleandat.RData'))

#boxplots of dvs
box_wave =  function(var){
  ggplot(cleandat,aes_string(x='grade',y=var,group='grade')) + geom_boxplot()
}

pdf(paste0(imgdir,'boxplots.pdf'))

#dvs -- boxplots
print(box_wave('depress'))
print(box_wave('depress.d'))

print(box_wave('dv.outdegc'))
print(box_wave('dv.outdegc.d'))

print(box_wave('dv.indegc'))
print(box_wave('dv.indegc.d'))

dev.off()

#bivariate plots
#distress, indegc, outdegc scatter plot
plts = melt(cleandat %>% select(depress,dv.indegc,dv.outdegc),id='depress')
ggplot(plts, aes(x=depress,y=value,color=variable)) + 
  geom_jitter(alpha=0.15) + geom_smooth() +
  xlab('Depression') + ylab('Degrees')

ggsave(paste0(imgdir,'bivariate_dv.pdf'))

plts = melt(cleandat %>% select(depress.d,dv.indegc.d,dv.outdegc.d),id='depress.d')
ggplot(plts, aes(x=depress.d,y=value,color=variable)) + 
  geom_jitter(alpha=0.15) + geom_smooth() +
  xlab('Depression') + ylab('Degrees')

ggsave(paste0(imgdir,'bivariate_delta_dv.pdf'))

#indeg & outdeg
ggplot(cleandat, aes(x=dv.indegc,y=dv.outdegc)) + geom_jitter(alpha=0.15) + geom_smooth()
ggsave(paste0(imgdir,'bivariate_degrees.pdf'))

ggplot(cleandat, aes(x=dv.indegc.d,y=dv.outdegc.d)) + geom_jitter(alpha=0.15) + geom_smooth()
ggsave(paste0(imgdir,'bivariate_degrees_dv.pdf'))

#obs level associations
#controlling for sex,grade,treatment,cslun,cms,psamesex,alterdistress,reciprocity

x=cleandat %>% select(f.male,f.white,f.nwaves,grade,
                      alterdistress,psamesexuc,
                      freelunch,cms.r,grade,egodenuc)

y=cleandat %>% select(depress,dv.indegc,dv.outdegc)

#options(na.action='na.pass')
#test curvilinear

sink(paste0(outdir,'lin_mods.txt'))

for(m in 1:ncol(y))
{
  printhead(paste0('LM--no cluster or adjustment DV:',colnames(y)[m]))
  cols=1:ncol(y)
  ymat = y[,!(cols==m)]; cn=colnames(ymat)
  ymat = cbind(ymat,
               as.data.frame(apply(ymat,2,function(x) x^2))
               )
  colnames(ymat) = c(cn,paste0(cn,'^2'))
  options(na.action='na.pass')
  xmat = model.matrix(~egodenuc*alterdistress + .,cbind(ymat,x))
  options(na.action='na.omit')
  print(
    summary(lm(y[,m]~xmat-1))
    )
}


sink()

sink(paste0(outdir,'fe-analyze.txt'))


fe1=plm(depress~dv.indegc+dv.outdegc+egodenuc+alterdistress+alterdistress*egodenuc+
       psamesexuc+freelunch+cms.r+grade,index='f.id',model='within',data=cleandat)
printhead("Depress-DV")
print(summary(fe1))

fe2=plm(dv.indegc~depress+dv.outdegc+egodenuc+alterdistress+alterdistress*egodenuc+
          psamesexuc+freelunch+cms.r+grade,index='f.id',model='within',data=cleandat)
printhead("Indegree-DV")
print(summary(fe2))

fe3=plm(dv.outdegc~depress+dv.indegc+egodenuc+alterdistress+alterdistress*egodenuc+
          psamesexuc+freelunch+cms.r+grade,index='f.id',model='within',data=cleandat)
printhead("Outdegree-DV")
print(summary(fe3))
sink()

sink(paste0('Growth Curves with Random Slopes (Grade) and Intercepts'))

m1=lmer(depress~dv.indegc+dv.outdegc+egodenuc+alterdistress+alterdistress*egodenuc+
       psamesexuc+freelunch+cms.r+grade+f.male+f.white+f.treat+f.nwaves+
       (grade | f.id),data=cleandat)
printhead("Depress-DV")
print(summary(m1))

m2=lmer(dv.indegc~depress+dv.outdegc+egodenuc+alterdistress+alterdistress*egodenuc+
          psamesexuc+freelunch+cms.r+grade+f.male+f.white+f.treat+f.nwaves+
          (grade | f.id),data=cleandat)
printhead("Indegree-DV")
print(summary(m2))

m3=lmer(dv.outdegc~depress+dv.indegc+egodenuc+alterdistress+alterdistress*egodenuc+
          psamesexuc+freelunch+cms.r+grade+f.male+f.white+f.treat+f.nwaves+
          (grade | f.id),data=cleandat)
printhead("Outdegree-DV")
print(summary(m3))


"
for(m in 1:ncol(y))
{
  printhead(paste0('LM--no cluster or adjustment DV:',colnames(y)[m]))
  cols=1:ncol(y)
  ilev=factor(cleandat$f.id)
  ymat = y[,!(cols==m)]; cn=colnames(ymat)
  ymat = cbind(ymat,
               as.data.frame(apply(ymat,2,function(x) x^2))
  )
  colnames(ymat) = c(cn,paste0(cn,'^2'))
  options(na.action='na.pass')
  #xmat = model.matrix(~egodenuc*alterdistress + .,cbind(ymat,x))
  xmat = model.matrix(~ .,cbind(ymat,x,ilev))
  options(na.action='na.omit')
  print(
    summary(lm(y[,m]~xmat-1))
  )
}
"

sink()