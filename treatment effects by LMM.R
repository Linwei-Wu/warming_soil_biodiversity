

##treatment effects by linear mixed model
treatused<-read.csv("treatment info.csv",header = T,row.names = 1)
divindex<-read.csv("16S_alpha.csv",header = T,row.names = 1)
#divindex<-read.csv("ITS_alpha.csv",header = T,row.names = 1)

treatused$year<-treatused$year-2009  
divindex<-divindex[match(row.names(treatused),row.names(divindex)),] #match names
sum(row.names(divindex)==row.names(treatused)) #check if names are mached

divindex<-scale(divindex)  ##recale the alpha diversities

library(lme4)
library(car)
###note: the precipitation level is noted as 0.5, 1, 2 in LMMs. But it is better to be noted as -0.5, 0, 1 to make the ambient condition as '0' for easier
###interpretation. You can set: treatused$precip=treatused$precip-1 to re-run the LMMs (thus precip=-0.5, 0, 1), whose results are very similar to the current ones.

divs1<-sapply(1:ncol(divindex),function(j){
  message("Now j=",j," in ",ncol(divindex),". ",date())
  if (length(unique(divindex[,j]))<3){
    result<-rep(NA,38)
  } else {
    div<-data.frame(divtest=divindex[,j],treatused)
    
    fm1<-lmer(divtest~warm*precip*clip+(1|year)+(1|block),data=div)
    
    presult<-car::Anova(fm1,type=2)
    coefs<-coef(summary(fm1))[ , "Estimate"]  ##four coefs
    names(coefs)<-paste0(names(coefs),".mean")
    
    SEvalues<-coef(summary(fm1))[ , "Std. Error"] ##standard errors
    names(SEvalues)<-paste0(names(SEvalues),".se")
    
    tvalues<-coef(summary(fm1))[ , "t value"] ##t values
    names(tvalues)<-paste0(names(tvalues),".t")
    
    chisqP<-c(presult[,1],presult[,3])
    names(chisqP)<-c(paste0(row.names(presult),".chisq"),paste0(row.names(presult),".P"))
    
    
    
    result<-c(coefs,tvalues,SEvalues,chisqP)}
  result
})
colnames(divs1)<-colnames(divindex)

write.csv(divs1,"linear mixed model_16S_alpha.csv")
#write.csv(divs1,"linear mixed model_ITS_alpha.csv")

########treatment effects in each year#####
acdiv<-read.csv("16S_alpha.csv",header = T,row.names = 1)
fungidiv<-read.csv("ITS_alpha.csv",header = T,row.names = 1)
treat<-read.csv("treatment info.csv",header = T,row.names = 1)

##change year values to test the treatment effects for different years
divbac=data.frame(treat[treat$year==2010,],scale(bacdiv[treat$year==2010,]) )
divfun=data.frame(treat[treat$year==2010,],scale(fungidiv[treat$year==2010,]) )

fm1<-lmer(richness~warm*precip*clip+(1|block),data=divbac)
summary(fm1)
car::Anova(fm1,type=2)

fm2<-lmer(richness~warm*precip*clip+(1|block),data=divfun)
summary(fm2)
car::Anova(fm2,type=2)
