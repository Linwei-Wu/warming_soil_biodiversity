

otutab<-read.csv("otu_table_resampled_16S.txt",sep="\t",row.names = 1)  ##this file is too big. Cannot be uploaed to github
#otutab<-read.csv("otu_table_resampled.guilds_ITS.txt",sep="\t",row.names = 1)

classifier<-otutab$taxonomy
#classifier<-otutab[,c("Trophic.Mode","Guild")]

treat<-read.csv("treatment info.csv",row.names = 1)
otutab<-otutab[,match(row.names(treat),colnames(otutab))]
comm<-t(otutab)
sum(row.names(treat)==row.names(comm))

classifier<-as.character(classifier)
cat1 = sapply(strsplit(classifier, ";"), function(x) x[2])
cat2= sapply(strsplit(classifier, ";"), function(x) x[3])

cat1<-gsub(" D_1__","",cat1)
cat2<-gsub(" D_2__","",cat2)

#cat1<-gsub("p__","",cat1)
#cat2<-gsub("c__","",cat2)

#proteobacteria at class level
groups<-sapply(1:length(cat1),function(i){
  ifelse(cat1[i]=="Proteobacteria",cat2[i],cat1[i])
})

#fungi functional guilds
#groups<-sapply(1:nrow(classifier), function(i){
#  ifelse(classifier[i,1]=="Saprotroph","Saprotroph",ifelse(classifier[i,2]=="Plant Pathogen","Pathogen",ifelse(classifier[i,2]=="Arbuscular Mycorrhizal","AM.Fungi","others")))
#})



#phylum level
#groups=cat1 #for fungi phyla

groupsplits1=split(as.data.frame(t(comm)),as.factor(groups))
#richness of different groups
richness1<-sapply(groupsplits1,function(x){
  colSums(x>0)})


#linear mixed modeling#
library(lme4)
library(car)
treatused<-treat
treatused$year<-treatused$year-2009

divindex<-richness1
divindex<-divindex[match(row.names(treatused),row.names(divindex)),]

sum(row.names(treatused)==row.names(divindex))
divindex<-scale(divindex)


divs2<-sapply(1:ncol(divindex),function(j){
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

colnames(divs2)<-colnames(divindex)

write.csv(divs2,"Richness_phylum_treatment effects.csv")

##the effect sizes of warming were then used for plot
