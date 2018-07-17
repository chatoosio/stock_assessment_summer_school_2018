remove(list = ls())
library(devtools)
library (ellipse)
# install_github("mawp/spict/spict")
# install_github("mawp/spict/spict", ref="dev")
# Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.21/bin/gswin64c.exe")

# SPICT ASSESSMENT NEP 17-18
library(ggplot2)
library(plyr)
library(spict)
library(reshape2)
library(data.table)

# Data Structure

# Let's work with a good time series for norway lobster in GSAs1718.
# What is available: 
# catch since 1970
# biomass index since 1994

# Load data ####

lands17_18=fread("landings17_18.csv")
names(lands17_18)=c("year","landings")

# look at the file, what do we have there?
View(lands17_18)

MEDITS=fread("MEDITS17_18.csv")
names(MEDITS)=c("index","year")

# look at the file, what do we have there?
View(MEDITS)

nep_pomo=fread("nep_pomo.csv")
View(nep_pomo)
froglia=fread("froglia.csv")
View(froglia)



## Build spict stock
nep1718 <- vector("list")

# Import Catch data (basically only landings because there isn't discard for this species)
nep1718$obsC  <- lands17_18$landings #catch observations
nep1718$timeC <- lands17_18$year # time of catch observations

# TUNING INDEXES
# Pick surveys index, in this case an trawl survey biomass index (MEDITS)
nep1718$obsI <- list()
nep1718$obsI[[1]] <- froglia$kh_hour_m
nep1718$obsI[[2]] <- nep_pomo$CPUE_NEP_Combined
nep1718$obsI[[3]] <- MEDITS$index[MEDITS$year > 1993]

nep1718$timeI <- list()
nep1718$timeI[[1]]= as.numeric(froglia$year)
nep1718$timeI[[2]]=nep_pomo$year
nep1718$timeI[[3]]=MEDITS$year[MEDITS$year > 1993]
  
# Inspect the file
nep1718

# Plot your data
plotspict.data(nep1718) # Notice color coding of the month! 

#Plot inital guesses on the model initial values
plotspict.ci(nep1718)

#The two top plots come from plotspict.data, with the dashed horizontal line representing a guess of MSY.
#This guess comes from a linear regression between the index and the catch divided by the index (middle row,left). This regression is expected to have a negative slope. A similar plot can be made showing catch versus catch/index (middle row, right) to approximately find the optimal effort (or effort proxy). The proportional increase in the index as a function of catch (bottom row, right) should show primarily positive increasesin index at low catches and vice versa. Positive increases in index at large catches could indicate model violations (Source SPICT Vignette)

# Fit base model
nep1718fit <- fit.spict(nep1718)

# Now what ? Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

# Explore convergence
capture.output(summary(nep1718fit))[1:4]

# Calculate residuals and main diagnostics
nep1718fit_diagn <- calc.osa.resid(nep1718fit)

plotspict.diagnostic(nep1718fit_diagn)

# Retrospective analysis
# Diagnostics, run it by taking away the last 4 years, one at a time
nep1718fit_retro <- retro(nep1718fit, nretroyear = 4)

# now plot it!
plotspict.retro(nep1718fit_retro)



x11()
plot(nep1718fit)
par(mfrow=c(1,1))
plotspict.f(nep1718fit,rel.axes = F,rel.ci = F,stamp ="")
plotspict.bbmsy(nep1718fit)
plotspict.ffmsy(nep1718fit)
plotspict.tc(nep1718fit)
plotspict.biomass(nep1718fit)
plotspict.catch(nep1718fit)
plotspict.fb(nep1718fit)
summary(nep1718fit)
capture.output(summary(nep1718fit))


# So the model fits quite well, diagnostics are good, we can have a look at the final results.
# Fit Summary
summary(nep1718fit)

x11()
plot(nep1718fit)

par(mfrow=c(3, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(nep1718fit)
plotspict.ffmsy(nep1718fit, qlegend=FALSE)
plotspict.catch(nep1718fit, qlegend=FALSE)
plotspict.fb(nep1718fit, man.legend=FALSE)
plotspict.tc(nep1718fit)
x11()
plotspict.ffmsy(nep1718fit, qlegend=FALSE)

# Check the timing of when MEDITS survey was performed. 
# MEDITS survey was in month 5-8 aside 2007 and 2014 (month 9-12)
# Since the model needs to account for growth, the weight of the fish in the survey needs to be acounted for
# Add to the year the fraction of the month e.g. 6.5/12 = 0.54 and 10.5/12 = 0.0875
nep1718$timeI
nep1718$timeI[[3]][1:13] <- nep1718$timeI[[3]][1:13] + 0.54
nep1718$timeI[[3]][15:20] <- nep1718$timeI[[3]][15:20] + 0.54
nep1718$timeI[[3]][22:23] <- nep1718$timeI[[3]][22:23] + 0.54
nep1718$timeI[[3]][14] <- nep1718$timeI[[3]][14] + 0.875
nep1718$timeI[[3]][21] <- nep1718$timeI[[3]][21] + 0.875
nep1718$eulertype = "soft"

# how does it look? 
plotspict.data(nep1718)


nep1718fitTA <- fit.spict(nep1718)


# Explore convergence
capture.output(summary(nep1718fitTA))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

nep1718fitTA_diagn <- calc.osa.resid(nep1718fitTA)

plotspict.diagnostic(nep1718fitTA_diagn)


# Diagnostics, run it by taking away the last 4 years, one at a time
nep1718fitTA_retro <- retro(nep1718fitTA, nretroyear = 4)
plotspict.retro(nep1718fitTA_retro)


# Fit Summary
summary(nep1718fitTA)

x11()
plot(nep1718fitTA)
# So what do you think, is this a better fit than the previous one?


############################################################################################################################

#######################################################################################
# CASE STUDY COMPARISON with a age based model

data(ple4.index)
data(ple4)

# Take an ICES Stock, like Plaice, this is normally assessed at age with an XSA or other model, the example contains the estimates in the FLStock.

#We want to compare the fit of Plaice with a SPICT production model and that of the original stock assessment

#The FLStock of plaice and the FLIndex, are both at age, and the index has only the abundance (Catch.n)

# First step is to take what is needed from the FLStock and collapse along the age dimensions.

as.vector(catch(ple4))

#Create an emplty list for Spict Object

ple <- vector("list")

# Import Catch data (Landings plus Discards)

ple$obsC  <- as.vector(catch(ple4))  #catch observations
ple$timeC <- seq(1957, 2008)   # time of catch observations


# For the index we need to build an index by biomass, we only have abundance, so as an acceptable hack we take the stock.wt * catch.n of the index to derive the total abundance by age. This is then summed by year

## weight at age index
Iwa <- catch.n(ple4.index) * trim(stock.wt(ple4),
                                  year = dimnames(ple4.index@catch.n)$year,
                                  age = dimnames(ple4.index@catch.n)$age)

## sum the index
as.vector(quantSums(Iwa))

ple$timeI <- seq(1985,2008)

ple$obsI <- as.vector(quantSums(Iwa))

# there is a very low point in the IWA
ple$obsI[[13]] <- NA

ple

# Lets have a look
x11()
plotspict.data(ple)
x11()
plot(ple4)

#Plot inital guesses on the model initial values

plotspict.ci(ple)

plefit <- fit.spict(ple)


# Explore convergence
capture.output(summary(plefit))[1:4]

plot(plefit)

# Short term forecast
#To make a catch forecast a forecast interval needs to be specified. This is done by specifying the start of the interval (inp$timepredc) and the length of the interval in years (inp$dtpredc). 

#For example, if a forecast of the annual catch of 2018 is of interest, then inp$timepredc = 2018 and inp$dtpredc = 1. 

#In addition to the forecast interval a fishing scenario needs to be specified. This is done by specifying a factor (inp$ffac) to multiply the current fishing mortality by (i.e. the F at the last time point of the time period where data are available) and the time that management should start (inp$manstart). 

#The time point of the reported forecast of biomass and fishing mortality can be controlled by setting inp$timepredi. Producing short-term forecasts entails minimal additional computing time.

nep1718$manstart <- 2017 # When management will start
nep1718$timepredc <- 2020 # Time when we want predicted catch
nep1718$dtpredc <- 1 # Time interval in years for prediction
nep1718$timepredi <- 2020
nep1718$ffac <- 0.48

nepspict <- fit.spict(nep1718)
summary(nepspict)
plot(nepspict)

sumspict.predictions(nepspict)

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(nepspict)
plotspict.ffmsy(nepspict, qlegend=FALSE)
plotspict.catch(nepspict, qlegend=FALSE)
plotspict.fb(nepspict, man.legend=FALSE)


# BUILD MANAGEMENT SCENARIOS
# The package has a function that runs several predefined management scenarios, which can be presented in a forecast table.
res <- manage(nepspict)
df <- mansummary(res)
df

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(res)
plotspict.ffmsy(res, qlegend=FALSE)
plotspict.catch(res, qlegend=FALSE)
plotspict.fb(res, man.legend=FALSE)

# table of forecast for  +1 year
mansummary(res, ypred=1, include.unc = FALSE)

# table of forecast for  +4 year, in our case the period 2019-2020
mansummary(res, ypred=4, include.unc = FALSE)

# 
# 
# 
# # Management scenario ####
# # Keep current catch
# # Keep current F
# # Fish at Fmsy
# # No fishing
# # Reduce F25%
# # Increase F25%
# nep1718fit <- manage(nep1718fit)
# dfc <- mansummary(nep1718fit,ypred=4) 
# 
# #ypred assign time range prediction 1yr 2yrs ect 
# # dfc <- mansummary(nep1718afit, ypred = 2)
# dfc
# set.seed(123)
# check.ini(nep1718c, ntrials=4)
# nep1718c$phases$logsdb <- -1
# nep1718c$ini$logsdb <- log(0.1)
# nep1718cfit2 <- fit.spict(nep1718c)
# summary(nep1718cfit2)
# x11()
# par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
# #plotspict.bbmsy(nep1718cfit,qlegend = F)
# plotspict.bbmsy(nep1718cfit2)
# plotspict.ffmsy(nep1718cfit2, qlegend=TRUE)
# #plotspict.ffmsy(nep1718cfit2, qlegend=FALSE)
# plotspict.catch(nep1718cfit2, qlegend=FALSE)
# #plotspict.fb(nep1718cfit, man.legend=FALSE)
# plotspict.fb(nep1718cfit2, man.legend=FALSE)
# 
# 
# 
# # Extract table of predictions according to different scenarios
# sc1 <- data.frame(get.par("logB", nep1718cfit$man[[1]], exp = TRUE))
# sc2 <- data.frame(get.par("logB", nep1718cfit$man[[2]], exp = TRUE))
# sc3 <- data.frame(get.par("logB", nep1718cfit$man[[3]], exp = TRUE))
# sc4 <- data.frame(get.par("logB", nep1718cfit$man[[4]], exp = TRUE))
# sc5 <- data.frame(get.par("logB", nep1718cfit$man[[5]], exp = TRUE))
# sc6 <- data.frame(get.par("logB", nep1718cfit$man[[6]], exp = TRUE))
# 
# sc1$scenario <- rep("sc1", length(sc1$ll))
# sc2$scenario <- rep("sc2", length(sc2$ll))
# sc3$scenario <- rep("sc3", length(sc3$ll))
# sc4$scenario <- rep("sc4", length(sc4$ll))
# sc5$scenario <- rep("sc5", length(sc5$ll))
# sc6$scenario <- rep("sc6", length(sc6$ll))
# 
# sc <- rbind(sc1,sc2, sc3,sc4,sc5,sc6)
# sc$year <- rownames(sc)
# sc2 <- sc[sc$year >2016, ]
# 
# 
# # FROM ALEX
# FBC3 <- lapply(nep1718cfit$man, function(x) {
#   estcol <- 2
#   b <- get.par("logB", x, exp = TRUE)
#   t <- as.numeric(row.names(b))
#   f <- get.par("logF", x, exp = TRUE)
#   c.ann <- data.frame(year = x$inp$timeCpred, C = get.par("logCpred", x, exp = TRUE) [, estcol])
#   with(spict:::annual(t, b[, estcol]),
#        b.ann <<- data.frame(year = as.numeric(anntime), B = annvec))
#   with(spict:::annual(t, f[, estcol]),
#        f.ann <<- data.frame(year = as.numeric(anntime), F = annvec))
#   res <- merge(merge(f.ann, b.ann), c.ann)
#   dplyr::filter(res, year >= 2016)
# }) 
# 
# names(FBC3)=c("Keep_current_catch",
#               "Keep_current_F",
#               "Fish@Fmsy",
#               "No_fishing",
#               "Reduce_F25%",
#               "Increase_F25%")
# 
# FBC3
# 
# 
# # SUMMARY SHEET EXTRACTION
# nep_summary3 <- lapply(nep1718cfit$man, function(x) {
#   estcol <- 2
#   b <- get.par("logBBmsy", x, exp = TRUE)
#   t <- as.numeric(row.names(b))
#   f <- get.par("logFFmsy", x, exp = TRUE)
#   c.ann <- data.frame(year = x$inp$timeCpred, C = get.par("logCpred", x, exp = TRUE) [, estcol])
#   with(spict:::annual(t, b[, estcol]),
#        b.ann <<- data.frame(year = as.numeric(anntime), B = annvec))
#   with(spict:::annual(t, f[, estcol]),
#        f.ann <<- data.frame(year = as.numeric(anntime), F = annvec))
#   res <- merge(merge(f.ann, b.ann), c.ann)
#   dplyr::filter(res, year <= 2016)
# })
# nep_summary3
# summary(nep1718cfit)
# 
