

dat<-read.csv("for SEM_plot mean diversity and envs.csv",header = T,row.names = 1)

library(lavaan)

dat<-dat[,-(1:2)]
dat$NO3.N=log10(dat$NO3.N)
dat.scaled<-data.frame(scale(dat,center = F))  #recale the variables



##ITS model##

model <- '
# regressions

temperature_annual~warm+clip
annual_moisture~warm+precip
plant.richness~warm+precip+clip
FlC4~clip+plant.richness+NH4.N+annual_moisture
NH4.N~warm+precip
ITS~temperature_annual+annual_moisture+pH+FlC4+NH4.N+plant.richness
temperature_annual~~0*annual_moisture
pH~warm+clip+precip
'
Fit <- lavaan::sem(model, data=dat.scaled)
summary(Fit, rsquare=T, standardized=T,fit.measures=TRUE)

#residuals(Fit, type="cor")

##16S model##

model <- '
# regressions

temperature_annual~warm+clip
annual_moisture~warm+precip
plant.richness~warm+precip+clip
FlC3~plant.richness+clip+temperature_annual
NO3.N~annual_moisture+clip
X16S~temperature_annual+annual_moisture+plant.richness+pH+NO3.N+FlC3
pH~warm+NO3.N
annual_moisture ~~ NO3.N
'
Fit <- lavaan::sem(model, data=dat.scaled)
summary(Fit, rsquare=T, standardized=T,fit.measures=TRUE)

residuals(Fit, type="cor")

#raw SEM plot
library(semPlot)

semPaths(Fit)
semPaths(Fit,"std",residuals=FALSE,nCharNodes=0,layout="tree2",edge.label.cex = 1,sizeMan =12)
