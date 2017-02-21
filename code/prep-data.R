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


#load directory file and functions
source("~config.R"); source('funs.R')
#imgdir = for images
#outdir = for output
#rawdir = for raw data

#change display

#@@@@@@@@@@@@@@@@@@@@@
#Load, Prep means, lags, and firstobs dummy; Export for MPLUS
#@@@@@@@@@@@@@@@@@@@@@

#load data


#@@@@@@@@
#helper functions (loading only)

zipread = function(z){
  #require(haven)
  #require(sas7bdat)
  require(RevoScaleR)
  # loads sas files from a zipped folder
  #
  # Args:
  #   z: zipped folder
  #
  # Returns:
  #   dataframe
  
  cat('\n\nUnziping and creating dataframe(s) for:\n',z)
  
  #store working directory
  rtdir = getwd()
  
  #create temp directory for unzipping from root 
  #note ~ is ignored by .gitignore
  if(!dir.exists('tmp~')){dir.create('tmp~')}
  setwd(paste0(rtdir,'/tmp~'))
  
  #unzip and load stata file (these are old files so foreign works)
  unzip(z)
  f = list.files(recursive=TRUE,pattern='.sas7bdat')
  if(length(f)>0){
    tempdat = lapply(f,FUN=function(x) try(rxImport(x)))
    names(tempdat) = f
  } 
  
  #delete temporary directory
  setwd(rtdir)
  unlink('tmp~',recursive=TRUE,force=TRUE)
  
  return(tempdat)
}

#load all sas data -- note errors are captured
dats=zipread(paste0(rawdir,'Final Datasets_10_16.zip'))

#select appropriate df
dat = dats[[4]]; #rm(dats)

#@@@@@
#coding variables
#@@@@@

#select variables from frame by category
admin.vars=c('id','wave','cohort','cmty',
             'school','cond_treat','keepers_r')

demo.vars=c('tsex_rfinal_r','cwhtracer_r',
            'cms_r','cslun_r')


interest.vars=c('depr_2','cavgdepr')

calc.vars=c(    'psamesexuc','emcavgdepruc','emcavgdeprmc', 
                'boncentuc','etrnstvtyc','egodenuc',
                'outdeg','indeg','recip')



vars = c(admin.vars,demo.vars,interest.vars)

#NOTE: keepers are individuals whith network data 
#(i.e. selected to the network sampling frame)
dat1 = dats[[4]] %>% 
  select(one_of(vars)) %>% 
  filter(keepers_r == 1)
dat2=dats[[5]] %>% 
  select(matches(paste(c(admin.vars,calc.vars),collapse='|'))) 

#make everything lowercase for consistency
colnames(dat2) = tolower(colnames(dat2))
colnames(dat1) = tolower(colnames(dat1))

#missing dat
print(
  length(calc.vars) - 
  sum(grepl(paste(calc.vars,collapse='|'),colnames(dat2)))
)

#merge by matches 
dims=c(dim(dat1),dim(dat2))

#all.x and all.y include all rows from dataset
#merge by network dataframe
dat2$netdat = 1
dat=merge(dat1,dat2,all.x=TRUE,all.y=TRUE)

#print a sample set of individuals and check merge
ids=unique(dat1$id); samp = sample(ids,3) 

tst = lapply(list(dat1,dat2,dat),function(...)
  filter(...,id %in% samp))

#describe
print(lapply(tst,dim))

options(width=1000)

sink(paste0(outdir,'netlev-example.txt'))

print(tst[[2]])

sink()

sink(paste0(outdir,'missing-nets.txt'))
print(dat[is.na(dat$netdat),colnames(dat1)])
sink()

#@@@@@
#recodes and limits
#@@@@@

#clear cache
rm(list=c('dats', 'dat1', 'dat2'))

#limit to high school only (when depression was measured)
#f. indicates fixed
#limit to variables of most interest HERE
dat = dat %>% 
  filter(wave>=5) %>%
  mutate(grade=wave+4) %>%
  rename(
    f.id=id,
    f.treat=cond_treat,
    f.female=tsex_rfinal_r,
    f.white=cwhtracer_r,
    f.cohort=cohort,
    freelunch=cslun_r,
    dv.distress=cavgdepr, #averaged; depression + anxiety
    dv.outdegc=outdegc,
    dv.indegc=indegc,
    alterdistress=emcavgdepruc
  ) %>% select(-keepers_r,-wave,-depr_2,-emcavgdeprmc_es,-emcavgdepruc_es,-netdat,-toutdegree)

#draw initial observation of time-varying variables
fobs=dat %>%
  group_by(f.id) %>% arrange(grade) %>%
  slice(1) %>% ungroup %>%
  select(-f.treat,-f.white,-f.cohort)
colnames(fobs) = c('f.id',paste0(colnames(fobs)[2:ncol(fobs)],'_iobs'))

#calculate mean, lag and difference
calcdat = dat %>% 
  group_by(f.id) %>%
  mutate(f.nwaves=n()) %>%
  arrange(grade) %>%
  mutate_each(funs(mn=mean(.,na.rm=TRUE), 
                   lag=lag(.,order_by=grade),
                   d=mean(.,na.rm=TRUE) - .),
              matches('^[^(f\\.)|cmty|school]'))


cleandat=merge(calcdat,fobs,by='f.id')

#@@@@@
#data for use
#@@@@@

#summary stat -- dist of means -- only need calculated...... same as a
has.deltas = cleandat %>% 
  group_by(f.id)  %>%
  summarize_each(funs(sd)) %>% 
  ungroup %>%   select(-matches('_|^[(f\\.)]|cmty|school')) %>%
  summarize_each(
    funs(
      rnd(1-sum(is.nan(.)/length(.)))
    )
  )


sink(paste0(outdir,'means.txt'))

indiv.id = unique(cleandat$f.id)

#print summary
printhead('Data Overview')
cat(length(indiv.id), 'indoviduals observed for', nrow(cleandat), 'observations.\n')
cat('Proportion of individuals observed exactly once:',
    sum(cleandat$f.nwaves==1)/length(indiv.id),'\n')

printhead('Proporiton of Individuals with changes on each variable')
print(t(has.deltas))

printhead('Means, SD, and range')


cleandat %>% summarize_each(funs(max(.,na.rm=TRUE)))

colnames(cleandat) = gsub('_','.',colnames(cleandat))

sumstat = cleandat %>% 
  ungroup %>% select(-matches('.mn')) %>%
  summarize_each(funs(
    mean=rnd(mean(.,na.rm=TRUE)),
    sd=rnd(sd(.,na.rm=TRUE)),
    max=rnd(max(.,na.rm=TRUE)),
    min=rnd(min(.,na.rm=TRUE)),
    missing=rnd(sum(is.na(.))/n())
    ))

sumstat.tidy = sumstat %>% 
  gather(stat,val) %>% 
  separate(stat, into = c('var','stat'),sep='_') %>%
  spread(stat,val) %>%
  select(var,mean,sd,max,min,missing)

print(sumstat.tidy)
cat('\n\n Note: 
    .d is difference from individual mean
    .iobs is initial observation 
    .lag is lagged observation')

sink()

save(cleandat,file=paste0(outdir,'cleandat~/cleandat.RData'))

