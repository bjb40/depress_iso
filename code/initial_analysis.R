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

#dvs -- boxplots
print(box_wave('dv.distress'))
print(box_wave('dv.distress_d'))

print(box_wave('dv.outdegc'))
print(box_wave('dv.outdegc_d'))

print(box_wave('dv.indegc'))
print(box_wave('dv.indegc_d'))

#bivariate plots
#distress, indegc, outdegc scatter plot
plts = melt(cleandat %>% select(dv.distress,dv.indegc,dv.outdegc),id='dv.distress')
ggplot(plts, aes(x=dv.distress,y=value,color=variable)) + 
  geom_jitter(alpha=0.15) + geom_smooth() +
  xlab('Distress') + ylab('Degrees')

#indeg & outdeg
ggplot(cleandat, aes(x=dv.indegc,y=dv.outdegc)) + geom_jitter(alpha=0.15) + geom_smooth()
