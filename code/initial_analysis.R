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
print(box_wave('dv.distress'))
print(box_wave('dv.distress.d'))

print(box_wave('dv.outdegc'))
print(box_wave('dv.outdegc.d'))

print(box_wave('dv.indegc'))
print(box_wave('dv.indegc.d'))

dev.off()

#bivariate plots
#distress, indegc, outdegc scatter plot
plts = melt(cleandat %>% select(dv.distress,dv.indegc,dv.outdegc),id='dv.distress')
ggplot(plts, aes(x=dv.distress,y=value,color=variable)) + 
  geom_jitter(alpha=0.15) + geom_smooth() +
  xlab('Distress') + ylab('Degrees')

ggsave(paste0(imgdir,'bivariate_dv.pdf'))

#indeg & outdeg
ggplot(cleandat, aes(x=dv.indegc,y=dv.outdegc)) + geom_jitter(alpha=0.15) + geom_smooth()

ggsave(paste0(imgdir,'bivariate_degrees.pdf'))


#obs level associations
#controlling for sex,grade,treatment,cslun,cms,psamesex,alterdistress,reciprocity

x=cleandat %>% select(f.female,f.white,f.nwaves,grade,
                      alterdistress,psamesexuc,recipc,freelunch,cms.r)

y=cleandat %>% select(dv.distress,dv.indegc,dv.outdegc)

#options(na.action='na.pass')

sink(paste0(outdir,'lin_mods.txt'))

for(m in 1:ncol(y))
{
  printhead(paste0('LM--no cluster or adjustment DV:',colnames(y)[m]))
  cols=1:ncol(y)
  options(na.action='na.pass')
  xmat = model.matrix(~.,cbind(y[,!(cols==m)],x))
  options(na.action='na.omit')
  print(
    summary(lm(y[,m]~xmat-1))
    )
}


#options(na.action='na.omit')

sink()