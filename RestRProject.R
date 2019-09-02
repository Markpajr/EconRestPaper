dardendata <-read.csv("Rsdarden.csv", header=T)
dardendata <-ts(dardendata,start=c(1995),frequency = 12)
# summary(dardendata)
dardreg <- lm(DRCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dardendata)
summary(dardreg)
#plot(dardreg)
#### Check Model Specification
dardreg <- lm(DRCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dardendata)
dardYHat <-fitted(dardreg)
dardreg2 <-lm(log(DRCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI), data=dardendata)
lndardYHat<- fitted(dardreg2)
dardZ1t<-log(dardYHat)-lndardYHat
DardMWD<-lm(DRCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI +dardZ1t, data=dardendata)
summary(DardMWD)
dardZ2t<-exp(lndardYHat) - dardYHat
DardMWD2 <-lm(log(DRCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI) + dardZ2t, data=dardendata)
summary(DardMWD2)
# petest(dardreg,dardreg2) ###Built in MWD Test
#### Check for Heteroskedasticity (patterns in residual plot)
DRResidual<-resid(dardreg)
plot(dardendata[,"PPIF"],DRResidual)
drfit <- glm(DRResidual~ dardendata[,"PPIF"] )
drco <- coef(drfit)
abline(drfit, col="green", lwd=2)
#### Park Test for Heteroskedasticity/Autocorrelation (Interpret t-value on Logbhat)
DRResid2 <-DRResidual^2
DRYhat <-fitted(dardreg)
DRParktest <-lm(log(DRResid2) ~ log(DRYhat))
summary(DRParktest)
#### Checking multicolinearity for independent variables.
vif(dardreg)
# vif(lm(PPIF ~ PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dardendata))
# vif(lm(PPID ~ PPIF + PPIMPF + PPIS + URATE + CBPR + CSI, data=dardendata))
# vif(lm(PPIMPF ~ PPID + PPIF + PPIS + URATE + CBPR + CSI, data=dardendata))
# vif(lm(PPIS ~ PPID + PPIMPF + PPIF + URATE + CBPR + CSI, data=dardendata))
# vif(lm(URATE ~ PPID + PPIMPF + PPIS + PPIF + CBPR + CSI, data=dardendata))
# vif(lm(CBPR ~ PPID + PPIMPF + PPIS + URATE + PPIF + CSI, data=dardendata))
# vif(lm(CSI ~ PPID + PPIMPF + PPIS + URATE + CBPR + PPIF, data=dardendata))
#### RESET Test for Omitted Variable Bias (Returns F-Statistic, Use F-Table to interpret)
dardreg <- lm(DRCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dardendata)
dardYHat <-fitted(dardreg)
dardYHat2 <- dardYHat^2 ; dardYHat3 <- dardYHat^3
dardOmitt <-lm(DRCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI + dardYHat2 + dardYHat3, data=dardendata)
resettest(dardOmitt)
# resettest(dardreg, power=2:4, type=c("fitted"))
#### Durbin-Watson Test for Autocorrelation (Returns d-Statistic, Interpret using D-Chart and D Critical Values)
dwtest(dardreg)
dwt(dardreg)


yumdata<-read.csv("Rsyumbrands.csv", header=T)
yumdata<-ts(yumdata,start=c(1997),frequency = 12)
# summary(yumdata)
yumreg <- lm(YBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=yumdata)
summary(yumreg)
# plot(yumreg)
#### Check Model Specification
yumreg <- lm(YBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=yumdata)
yumYHat <-fitted(yumreg)
yumreg2 <-lm(log(YBCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI), data=yumdata)
lnyumYHat<- fitted(yumreg2)
yumZ1t<-log(yumYHat)-lnyumYHat
YumMWD<-lm(YBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI +yumZ1t, data=yumdata)
summary(YumMWD)
yumZ2t<-exp(lnyumYHat) - yumYHat
YumMWD2 <-lm(log(YBCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI) + yumZ2t, data=yumdata)
summary(YumMWD2)
#### Check for Heteroskedasticity (patterns in residual plot)
YBResidual<-resid(yumreg)
plot(yumdata[,"PPIF"],YBResidual)
ybfit <- glm(YBResidual~ yumdata[,"PPIF"] )
ybco <- coef(ybfit)
abline(ybfit, col="blue", lwd=2)
#### Park Test for Heteroskedasticity/Autocorrelation (Interpret t-value on Logbhat)
YBResid2 <-YBResidual^2
YBYhat <-fitted(yumreg)
YBParktest <-lm(log(YBResid2) ~ log(YBYhat))
summary(YBParktest)
#### Checking multicolinearity for independent variables.
vif(yumreg)
# vif(lm(PPIF ~ PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=yumdata))
# vif(lm(PPID ~ PPIF + PPIMPF + PPIS + URATE + CBPR + CSI, data=yumdata))
# vif(lm(PPIMPF ~ PPID + PPIF + PPIS + URATE + CBPR + CSI, data=yumdata))
# vif(lm(PPIS ~ PPID + PPIMPF + PPIF + URATE + CBPR + CSI, data=yumdata))
# vif(lm(URATE ~ PPID + PPIMPF + PPIS + PPIF + CBPR + CSI, data=yumdata))
# vif(lm(CBPR ~ PPID + PPIMPF + PPIS + URATE + PPIF + CSI, data=yumdata))
# vif(lm(CSI ~ PPID + PPIMPF + PPIS + URATE + CBPR + PPIF, data=yumdata))
#### RESET Test for Omitted Variable Bias (Returns F-Statistic, Use F-Table to interpret)
yumreg <- lm(YBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=yumdata)
yumYHat <-fitted(yumreg)
yumYHat2 <- yumYHat^2 ; yumYHat3 <- yumYHat^3
yumOmitt <-lm(YBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI + yumYHat2 + yumYHat3, data=yumdata)
resettest(yumOmitt)
#### Durbin-Watson Test for Autocorrelation (Returns d-Statistic, Interpret using D-Chart and D Critical Values)
dwtest(yumreg)


crackerdata<-read.csv("Rscrackerbarrel.csv", header=T)
crackerdata<-ts(crackerdata,start=c(1991),frequency = 12)
# summary(crackerdata)
crackerreg <- lm(CBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=crackerdata)
summary(crackerreg)
# plot(crackerreg)
#### Check Model Specification
crackerreg <- lm(CBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=crackerdata)
crackerYHat <-fitted(crackerreg)
crackerreg2 <-lm(log(CBCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI), data=crackerdata)
lncrackerYHat<- fitted(crackerreg2)
crackerZ1t<-log(crackerYHat)-lncrackerYHat
crackerMWD<-lm(CBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI +crackerZ1t, data=crackerdata)
summary(crackerMWD)
crackerZ2t<-exp(lncrackerYHat) - crackerYHat
crackerMWD2 <-lm(log(CBCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI) + crackerZ2t, data=crackerdata)
summary(crackerMWD2)
#### Check for Heteroskedasticity (patterns in residual plot)
CBResidual<-resid(crackerreg)
plot(crackerdata[,"PPIF"],CBResidual)
cbfit <- glm(CBResidual~ crackerdata[,"PPIF"] )
cbco <- coef(cbfit)
abline(cbfit, col="blue", lwd=2)
#### Park Test for Heteroskedasticity/Autocorrelation (Interpret t-value on Logbhat)
CBResid2 <-CBResidual^2
CBYhat <-fitted(crackerreg)
CBParktest <-lm(log(CBResid2) ~ log(CBYhat))
summary(CBParktest)
#### Checking multicolinearity for independent variables.
vif(crackerreg)
# vif(lm(PPIF ~ PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=crackerdata))
# vif(lm(PPID ~ PPIF + PPIMPF + PPIS + URATE + CBPR + CSI, data=crackerdata))
# vif(lm(PPIMPF ~ PPID + PPIF + PPIS + URATE + CBPR + CSI, data=crackerdata))
# vif(lm(PPIS ~ PPID + PPIMPF + PPIF + URATE + CBPR + CSI, data=crackerdata))
# vif(lm(URATE ~ PPID + PPIMPF + PPIS + PPIF + CBPR + CSI, data=crackerdata))
# vif(lm(CBPR ~ PPID + PPIMPF + PPIS + URATE + PPIF + CSI, data=crackerdata))
# vif(lm(CSI ~ PPID + PPIMPF + PPIS + URATE + CBPR + PPIF, data=crackerdata))
#### RESET Test for Omitted Variable Bias (Returns F-Statistic, Use F-Table to interpret)
crackerreg <- lm(CBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=crackerdata)
crackerYHat <-fitted(crackerreg)
crackerYHat2 <- crackerYHat^2 ; crackerYHat3 <- crackerYHat^3
crackerOmitt <-lm(CBCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI + crackerYHat2 + crackerYHat3, data=crackerdata)
resettest(crackerOmitt)
#### Durbin-Watson Test for Autocorrelation (Returns d-Statistic, Interpret using D-Chart and D Critical Values)
dwtest(crackerreg)




dedata<-read.csv("Rsdineequity.csv", header=T)
dedata<-ts(dedata,start=c(1991),frequency = 12)
# summary(dedata)
dereg <- lm(DECSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dedata)
summary(dereg)
#plot(dereg)
#### Check Model Specification
dereg <- lm(DECSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dedata)
deYHat <-fitted(dereg)
dereg2 <-lm(log(DECSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI), data=dedata)
lndeYHat<- fitted(dereg2)
deZ1t<-log(deYHat)-lndeYHat
deMWD<-lm(DECSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI +deZ1t, data=dedata)
summary(deMWD)
deZ2t<-exp(lndeYHat) - deYHat
deMWD2 <-lm(log(DECSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI) + deZ2t, data=dedata)
summary(deMWD2)
#### Check for Heteroskedasticity (patterns in residual plot)
DEResidual<-resid(dereg)
plot(dedata[,"PPIF"],DEResidual)
defit <- glm(DEResidual~ dedata[,"PPIF"] )
deco <- coef(defit)
abline(defit, col="blue", lwd=2)
#### Park Test for Heteroskedasticity/Autocorrelation (Interpret t-value on Logbhat)
DEResid2 <-DEResidual^2
DEYhat <-fitted(dereg)
DEParktest <-lm(log(DEResid2) ~ log(DEYhat))
summary(DEParktest)
#### Checking multicolinearity for independent variables.
vif(dereg)
# vif(lm(PPIF ~ PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dedata))
# vif(lm(PPID ~ PPIF + PPIMPF + PPIS + URATE + CBPR + CSI, data=dedata))
# vif(lm(PPIMPF ~ PPID + PPIF + PPIS + URATE + CBPR + CSI, data=dedata))
# vif(lm(PPIS ~ PPID + PPIMPF + PPIF + URATE + CBPR + CSI, data=dedata))
# vif(lm(URATE ~ PPID + PPIMPF + PPIS + PPIF + CBPR + CSI, data=dedata))
# vif(lm(CBPR ~ PPID + PPIMPF + PPIS + URATE + PPIF + CSI, data=dedata))
# vif(lm(CSI ~ PPID + PPIMPF + PPIS + URATE + CBPR + PPIF, data=dedata))
#### RESET Test for Omitted Variable Bias (Returns F-Statistic, Use F-Table to interpret)
dereg <- lm(DECSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=dedata)
deYHat <-fitted(dereg)
deYHat2 <- deYHat^2 ; deYHat3 <- deYHat^3
deOmitt <-lm(DECSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI + deYHat2 + deYHat3, data=dedata)
resettest(deOmitt)
#### Durbin-Watson Test for Autocorrelation (Returns d-Statistic, Interpret using D-Chart and D Critical Values)
dwtest(dereg)



mcdodata<-read.csv("Rsmcdonalds.csv", header=T)
mcdodata<-ts(mcdodata,start=c(1991),frequency = 12)
# summary(mcdodata)
mcdoreg <- lm(MDCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=mcdodata)
summary(mcdoreg)
#plot(mcdoreg)
#### Check Model Specification
mcdoreg <- lm(MDCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=mcdodata)
mcdoYHat <-fitted(mcdoreg)
mcdoreg2 <-lm(log(MDCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI), data=mcdodata)
lnmcdoYHat<- fitted(mcdoreg2)
mcdoZ1t<-log(mcdoYHat)-lnmcdoYHat
mcdoMWD<-lm(MDCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI +mcdoZ1t, data=mcdodata)
summary(mcdoMWD)
mcdoZ2t<-exp(lnmcdoYHat) - mcdoYHat
mcdoMWD2 <-lm(log(MDCSP) ~ log(PPIF) + log(PPID) + log(PPIMPF) + log(PPIS) + log(URATE) + log(CBPR) + log(CSI) + mcdoZ2t, data=mcdodata)
summary(mcdoMWD2)
#### Check for Heteroskedasticity (patterns in residual plot)
MDResidual<-resid(mcdoreg)
plot(mcdodata[,"PPIF"],MDResidual)
mdfit <- glm(MDResidual~ mcdodata[,"PPIF"] )
mdco <- coef(mdfit)
abline(mdfit, col="blue", lwd=2)
#### Park Test for Heteroskedasticity/Autocorrelation (Interpret t-value on Logbhat)
MDResid2 <-MDResidual^2
MDYhat <-fitted(mcdoreg)
MDParktest <-lm(log(MDResid2) ~ log(MDYhat))
summary(MDParktest)
#### Checking multicolinearity for independent variables.
vif(mcdoreg)
vif(lm(PPIF ~ PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=mcdodata))
vif(lm(PPID ~ PPIF + PPIMPF + PPIS + URATE + CBPR + CSI, data=mcdodata))
vif(lm(PPIMPF ~ PPID + PPIF + PPIS + URATE + CBPR + CSI, data=mcdodata))
vif(lm(PPIS ~ PPID + PPIMPF + PPIF + URATE + CBPR + CSI, data=mcdodata))
vif(lm(URATE ~ PPID + PPIMPF + PPIS + PPIF + CBPR + CSI, data=mcdodata))
vif(lm(CBPR ~ PPID + PPIMPF + PPIS + URATE + PPIF + CSI, data=mcdodata))
vif(lm(CSI ~ PPID + PPIMPF + PPIS + URATE + CBPR + PPIF, data=mcdodata))
#### RESET Test for Omitted Variable Bias (Returns F-Statistic, Use F-Table to interpret)
mcdoreg <- lm(MDCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI, data=mcdodata)
mcdoYHat <-fitted(mcdoreg)
mcdoYHat2 <- mcdoYHat^2 ; mcdoYHat3 <- mcdoYHat^3
mcdoOmitt <-lm(MDCSP ~ PPIF + PPID + PPIMPF + PPIS + URATE + CBPR + CSI + mcdoYHat2 + mcdoYHat3, data=mcdodata)
resettest(mcdoOmitt)
#### Durbin-Watson Test for Autocorrelation (Returns d-Statistic, Interpret using D-Chart and D Critical Values)
dwtest(mcdoreg)






par(mfrow=c(2,3))
DRResplot<- plot(dardendata[,"PPIF"],DRResidual);abline(drfit, col="blue", lwd=2)
YBResplot<- plot(yumdata[,"PPIF"],YBResidual);abline(ybfit, col="red", lwd=2)
CRResplot<- plot(crackerdata[,"PPIF"],CBResidual);abline(cbfit, col="green", lwd=2)
DEResplot<- plot(dedata[,"PPIF"],DEResidual);abline(defit, col="black", lwd=2)
MDResplot<- plot(mcdodata[,"PPIF"],MDResidual);abline(mdfit, col="yellow", lwd=2)











##### SPARE CODE
# ### Gleiser Test for Heteroskedasticity/Autocorrelation
# YBGleiser <- lm(abs(YBResidual) ~ YBYhat)
# summary(YBGleiser)
# # ## Breusch-Pagan Test
# library(lmtest)
# library(car)
# bptest(yumreg)

