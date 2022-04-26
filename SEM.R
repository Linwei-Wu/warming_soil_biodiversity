

dat<-read.csv("for SEM_plot mean diversity and envs.csv",header = T,row.names = 1)

library(lavaan)

dat<-dat[,-(1:2)]


##half precipitation and double precipitation
wetting<-(dat$precip>1)*1
drying<-(dat$precip<1)*1
all<-data.frame(wetting=wetting,drying=drying,dat)

all$NO3.N=log10(all$NO3.N)
all.scaled<-data.frame(scale(all,center = F))


model <- '
# regressions
#soil.climate<~ temperature_annual+1*annual_moisture

temperature_annual~warm+clip
annual_moisture~warm+wetting+drying
plant.richness~warm+wetting+drying+clip
FlC3~plant.richness+clip+temperature_annual
NO3.N~annual_moisture+clip
X16S~temperature_annual+annual_moisture+plant.richness+pH+NO3.N+FlC3+X18S
X18S~X16S+temperature_annual
pH~warm+NO3.N
annual_moisture ~~ NO3.N
'
Fit <- lavaan::sem(model, data=all.scaled)
summary(Fit, rsquare=T, standardized=T,fit.measures=TRUE)
residuals(Fit, type="cor")
modificationIndices(Fit,standardized=F)

#ITS model
model <- '
# regressions

temperature_annual~warm+clip
annual_moisture~warm+wetting+drying
plant.richness~warm+wetting+drying+clip
FlC4~clip+plant.richness+NH4.N+annual_moisture
NH4.N~warm+wetting+drying
ITS~temperature_annual+annual_moisture+pH+FlC4+NH4.N+plant.richness
temperature_annual~~0*annual_moisture
pH~warm+clip+wetting+drying
'
Fit <- lavaan::sem(model, data=all.scaled)
summary(Fit, rsquare=T, standardized=T,fit.measures=TRUE)
summary(Fit, rsquare=T, standardized=T)

standardizedSolution(Fit)
residuals(Fit, type="cor")
modificationIndices(Fit,standardized=F)

#raw SEM plot
library(semPlot)

semPaths(Fit)
semPaths(Fit,"std",residuals=FALSE,nCharNodes=0,layout="tree2",edge.label.cex = 1,sizeMan =12)
