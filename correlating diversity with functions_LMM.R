
# function to get the R and p value from the linear mixed model
corenvs<-function(divtest,envs){
  sapply(colnames(envs),function(x){
    message(x)
    if(sum(abs(divtest-envs[,x]),na.rm = T)<0.01 | length(unique(envs[,x])) < 7 | length(unique(divtest)) < 7) { 
      result=list(r=NA,pvalue=NA)
    }else{
      div<-data.frame(divtest=divtest,env=envs[,x],dat)
      div<-div[(!is.na(div$divtest)) & (!is.na(div$env)),]
      if (length(unique(div$year))<2) {
        fm1<-lmer(env~divtest+(1|block),data=div)
      } else {
        fm1<-lmer(env~divtest+(1|year)+(1|block),data=div)
      }
      
      presult<-car::Anova(fm1,type=2)
      coefs<-coef(summary(fm1))[ , "Estimate"]
      pvalue=presult[,3]
      r2<-r.squaredGLMM(fm1)
      r=ifelse(coefs["divtest"]>0,(r2[1,1])^0.5,-(r2[1,1])^0.5)
      names(r)<-NULL
      result=list(r=r,pvalue=pvalue)
    }
    result
  },simplify = F)
}


dat<-read.csv("taxa group richness_and functions.csv",row.names = 1)

divs<-dat[,1:23]  ##richness of bacterial and fungal groups
envs<-dat[,30:38]   ##ecosystem functions
 
library(lme4)
library(MuMIn)
divs1<-sapply(colnames(divs),function(y){
  message(y)
  divtest=divs[,y]
  corenvs(divtest=divtest,envs = envs)
},simplify = F)

test<-sapply(divs1, function(x){
  sapply(x, function(y){
    y$r
  })
})

write.csv(t(test),"LMM_R_taxa richness and functions.csv")

test2<-sapply(divs1, function(x){
  sapply(x, function(y){
    y$pvalue
  })
})

write.csv(t(test2),"LMM_Pvalue_taxa richness and functions.csv")


##ADJUST P

pvalues<-t(test2)

library(Hmisc)
adjust.p<-p.adjust (as.matrix(pvalues), method="fdr")
ad.pmatrix<-matrix(adjust.p,nrow=nrow(pvalues))
row.names(ad.pmatrix)=row.names(pvalues)
colnames(ad.pmatrix)=colnames(pvalues)
write.csv(ad.pmatrix,"LMM_adjust P_taxa richness and functions.csv")

library(corrplot)
corrplot(as.matrix(t(test)),is.corr = FALSE,win.asp = .5, method= "square",tl.col="black",tl.cex = 0.6,tl.srt=45,p.mat =as.matrix(ad.pmatrix),sig.level = c(.001, .01, .05), pch.cex = .8,
         insig = "label_sig", pch.col = "white",cl.cex=0.6,cl.ratio=0.35,cl.align.text="l",cl.offset=0.2)


