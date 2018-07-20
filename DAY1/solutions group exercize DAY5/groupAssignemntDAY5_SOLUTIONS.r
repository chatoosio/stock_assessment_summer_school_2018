#####################################
# Assignment SPICT GROUP
#####################################


#1 Load data from Sardine in the Adriatic (CatchPIL.csv) and fit spict model. The biomass index was build by collapsinge the ECHO WEST acoustic index (WAA * NAA). Check all diagnostics (convergence, residuals and retro)!

#2 If no or poor convergence, put initial values for K and priors. To see how to do it, check in the SPICT Vignette how to do this.


#3 Is the timing of the survey correct as being set on an annual basis? # Set it to the correct one and rerun to see if it improves the fit.
# Ask Ilaria Costantini the exact months when the survey was actually performed. Hint, it might not converge with the strating values of K and the priors turned on.
# up to 2014 survey was in September
# from 2015 included survey performed in June/July


#4 If you have time, Run a short term forecast where you provide a short term forecast (+1 and +3 years) with predicted catch under the following scenario:
#Keep fishing at the F of the terminal year (Fstatus quo)
#Fishing at Fmsy or F0.1


# 5 At the end of the presentations: 
# Compare SPCIT fit to a4a fit. Is the perception of the stock status similiar or different in the recent years?



########################################################################## SOLUTION SPICT RUN CASE STUDY ANCHOVY
#
#### Load libraries ####
library(FLCore)
library(FLAssess)
library(FLXSA)
library(FLa4a)
library(ggplotFL)
library(spict)

PIL <- read.csv("catchPIL.csv")

pil <- vector("list")

# Import Catch data (Landings plus Discards)

pil$obsC  <- as.vector(PIL$catch)  #catch observations
pil$timeC <- seq(1975, 2016)   # time of catch observations


pil$obsI <- as.vector(PIL$index)
pil$timeI <- seq(1975, 2016)   # time of catch observations


# Lets have a look
x11()
plotspict.data(pil)

#Plot inital guesses on the model initial values

plotspict.ci(pil)

# Set starting values for K, a good hint is the 4 time the maximum catch in the series

pil$ini$logK <- log(4 * max(ane$obsC))
pil$priors$logK <- c(log(4 * max(ane$obsC)), 2, 1)

# Set priors for alpha and bets, see spict vignette manual
pil$priors$logn <- c(1, 1, 0)
pil$priors$logalpha <- c(1, 1, 0)
pil$priors$logbeta <- c(1, 1, 0)

# Minimize the SD on catch, prevents convergence in this case
#ane$phases$logsdc <- -1
#ane$ini$logsdc <- log(1e-3)

# TURNO ON ROBUST ESTIMATION, does not seem to make any difference
#ane$ini$robflagc <- 1
#ane$phases$logp1robfac <- -1
#ane$phases$logitpp <- -1
#ane$ini$logitpp <- qlogis(0.95)
#ane$ini$logp1robfac <- log(10)

## Specify the correct months for the survey
# up to 2014 survey was in September
# from 2015 included survey performed in June/July

pil$timeI[1:40] <- pil$timeI[1:40] + 9/10
pil$timeI[41:42] <- pil$timeI[41:42] + 6.5/10

pil$eulertype = "soft"


pilfit <- fit.spict(pil)


# Explore convergence
capture.output(summary(pilfit))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

pilfit_diagn <- calc.osa.resid(pilfit)

plotspict.diagnostic(pilfit_diagn)


# Diagnostics, run it by taking away the last 4 years, one at a time
pilfit_retro <- retro(pilfit, nretroyear = 4)

plotspict.retro(pilfit_retro)


# Fit Summary
summary(pilfit)

x11()
plot(pilfit)





#############################################################
# Assignment a4a GROUP
#############################################################

# 1 import data from SARDINE FLStock and FLindex
# verify the correct setting of m.spawn and harvest.spawn. 

# 2 Find the best Fmodel, Qmodel and possibly SR model
# Use all the available diagnostics

# 3 Determine the best fitting model, make sure that the Hessian matrix is computed and the a4a model converged.
# Through simulation compute uncertainity on the estimated parameters

# 4 Change the harvest.spwn = 0.5 in the final run to check the effect on the assessment estimates

# 5 Compute reference points for this stock and compare Fbar to the reference point of choice (F0.1 or Fmsy or something else)

# If you have time, Run a short term forecast where you provide a short term forecast (+1 and +3 years) with predicted catch under the following scenario:
#  Keep fishing at the F of the terminal year (Fstatus quo)
#Fishing at Fmsy or F0.1

# At the end of the presentations: 
# Compare a4a fit to SPCIT fit. Is the perception of the stock status similiar or different in the recent years?



# RESULTS OF FIT OF A4A MODEL
############################################################################################################################################################################################################



# Now let's run the a4a assessment
#==============================================================================
# rm(list=ls())

library(FLa4a)
library(FLash)
library(FLAssess)
library(ggplotFL)
library(FLBRP)
library(FLSAM)


### ============================================================================
### Prepare stock object for assessment
### ============================================================================
#Load object

load("SARDINE.RData")


### ============================================================================
### Prepare index object for assessment
### ============================================================================
#Load and modify all numbers at age data

load("SARDINEtun.RData")


# Let's create the biomass index for a4a:
dnms <- list(age="all",year=2003:2012)
bioidx <- FLIndexBiomass(FLQuant(NA, dimnames=dnms))
index(bioidx) <- c(213410, 213477, 107902, 246593, 136907, 131542, 231809, 125031, 79372, 89329)
range(bioidx) [c("startf","endf")] <- c(0.75,0.83)
EchoEastBiomass <- FLIndices(EchoEastBiomass=bioidx)

SARDINE.tun <- FLIndices(list(SARDINE.tun[["Echo West"]], SARDINE.tun[["Echo East"]],EchoEastBiomass[["EchoEastBiomass"]]))
names(SARDINE.tun)<-c("Echo West","Echo East","Echo East Biomass")

### ============================================================================
### Apply plusgroup to all data sets
### ============================================================================
pg <- 4

#- This function already changes the stock and landings.wts correctly
SARDINE <- setPlusGroup(SARDINE,pg)


aa.stk<-SARDINE
aa.idx<-SARDINE.tun


#####
# Catchability model
qmod2 <- list(~s(age, k = 3), ~s(age, k = 3), ~s(year, k = 6))
#qmod3 <- list(~s(age, k = 5), ~1)
#qmod4 <- list(~s(age, k = 5), ~s(age, k = 5))
#qmod5 <- list(~s(age, k=5) + s(year, k=4), ~s(age, k=5) + s(year, k=4)) # final best model: depending on age (with 5 nodes) and year (with 4 nodes) for each survey

#####
#F model
#fmod2 <- ~ s(age, k = 5) + factor(year)
#fmod2 <- ~ s(year, k = 7) + factor(age)
#fmod2<- ~ s(year, k = 5)
fmod2 <- ~ factor(year) # super bad
fmod3 <- ~ s(replace(age, age>=3,3), k = 3) + s(year, k=22)
fmod4 <- ~ s(age, k = 3) + factor(year) 
fmod6 <- ~ s(age, k = 3) + te(age, year, k = c(4,6))
fmod7 <- ~ te(age, year, k = c(4,12))
fmod8 <- ~ s(age, k = 3) + s(year, k=18) + te(age, year, k = c(4,5))    
#####
# Recruitment model 
rmodel1 <- ~s(year, k=5)  
rmodel2 <- ~geomean(CV=0.4)
rmodel3 <- ~bevholt(CV=0.2)
rmodel4 <- ~s(year, k=35)                                               
rmodel5 <- ~ricker(CV=0.2)

#### 
# Fit a4a

fit4 <- a4aSCA(aa.stk, aa.idx, srmodel=rmodel4, fmodel = fmod3, qmodel=qmod2) 

wireframe(data ~ age + year, data = as.data.frame(harvest(fit4)), drape = TRUE, screen = list(x = -90, y=-45))

# diagnostics
res4 <- residuals(fit4, aa.stk, aa.idx)
plot(res4, main="Residuals")
bubbles(res4)
qqmath(res4)
plot(fit4,aa.stk)
plot(fit4,aa.idx[1])
plot(fit4,aa.idx[2])

# update stock object
stk4 <- aa.stk + fit4
plot(stk4, main="Stock summary")



#--------------------------------------------------------------------
# Simulate
#--------------------------------------------------------------------
fitdef <- a4aSCA(aa.stk, aa.idx, srmodel=rmodel4, fmodel = fmod3, qmodel=qmod2) 


fits <- simulate(fitdef, 250)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fitdef))

keylst <- list(points=FALSE, lines=TRUE, space="right")
xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")), auto.key=keylst)

stks <- aa.stk + fits
plot(stks)




##################################################################################################################################################################################

