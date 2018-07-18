# Class structure
# load data from Mediterranean Hake in GSA 9-10-11
rm(list=ls())
library(FLCore)
library(ggplotFL)

load("HKE_09_10_11_EWG15_11.RData")

# Rename the Stock object to something more handy and faster to write
hke <- HKE_09_10_11_EWG15_11

class(hke)

slotNames(hke)

# look at the structure
str(hke)

# Look  at the name of the stock, why is it different after we changed the object name? 
# name(hke) != object name
# If you prefer change it #
# name(hke)[]="hke"

# Description
desc(hke)

# Range of ages, years, plus group and fbar
range(hke)

#let's plot it'
plot(hke)



# catch =~ landings + discards, in case discards had already been included in landings,
# so are 0 here. Normally discards should be reported in the discard quant
landings(hke) + discards(hke)
catch(hke)

catch(hke) <- landings(hke) + discards(hke)

landings.n(hke) + discards.n(hke)
catch.n(hke)

catch.n(hke) <- landings.n(hke) + discards.n(hke)
landings.n(hke)[,"2006",,,,]
catch.n(hke)[,"2006",,,,]

catch.wt(hke)

# Compute catch in weight for 2006 only,
# this is the product of number of fish @ age times the mean weight of fish @ age
quantSums(catch.n(hke)[,"2006",,,,] * catch.wt(hke)[,"2006",,,,])
# 8.2424e+04*0.0097962

# is it in line with what is reported with the hke stock?
catch(hke)[,"2006",,,,]

# m, m.spwn
m(hke) # natural mortality

# What is the mean natural mortality applied to the stock?
mean(m(hke))
quantMeans(m(hke))
quantMeans(m(hke)[c("0","1"),,,,,,])

# exp(-m(hke)["0",,,,,,])

# m.spwn
m.spwn(hke) # fraction of the natural mortality ocurring before spawning

# equivalently you have some fishing mortality (harvest) occurring on the fish population before spawning.
# This is important when you have population that spawn in a narrow window of time, in particular with a short life cycle.
harvest.spwn(hke)

# maturity
mat(hke)

# weight
catch.wt(hke)
stock.wt(hke)
stock.wt(hke)/catch.wt(hke)


# ESTIMATED PARAMETERS ##

# harvest e.g. fishing mortality or harvest rate, normally defined as F
harvest(hke)
d\\11111xyplot(data~age,groups=year,data=harvest(hke),type=c("l","p"))

# Size of the estimated population at sea, in terms of total biomass,total numbers at age.
stock.n(hke)
stock.wt(hke)

# So we compute it
computeStock(hke)

# and assign it to the slot stock
stock(hke) <- computeStock(hke)

# Recruitment
rec(hke)
# stock.n(hke)["0",,,,,,]

# Spawning Stock Biomass (SSB)
# SSB = stock.n * exp(-F * F.spwn - M * M.spwn) * stock.wt * mat
# quantSums(stock.n(hke)*stock.wt(hke)*mat(hke))
ssb(hke)

# METHODS Fbar = mean(F between fbar ages)
fbar(hke)

# METHODS fapex = max F per year
fapex(hke)

#METHODS Z = total mortality (F+M)
z(hke)
units(m(hke))[]="m"
z(hke)

# Other stuff
range(hke)
smallhke <- window(hke, start = 2008, end = 2013)

# Check new year range
range(smallhke)
summary(smallhke)

# SUBSET by year
# temp <- hke[,c("2008", "2009", "2010", "2011")]
# # or
# temp <- hke[,as.character(2008:2011)]

# replace using logical values
a=catch(hke)[[6]]
catch(hke)[[6]]<-99
plot(catch(hke))
catch(hke)[catch(hke)==99] <- 5000
plot(catch(hke))
catch(hke)[[6]]=a
plot(catch(hke))


# plot the FLStock
plot(hke)

# or individual parts
plot(stock(hke))
plot(stock.n(hke))
plot(landings(hke))


# METHODS convert to data frame
# entire FlStock
temp<-as.data.frame(hke)
summary(temp)
# or only some slots
head(as.data.frame(FLQuants(catch.n=catch.n(hke), stock.n=stock.n(hke))))






# flq <- FLQuant(rlnorm(60), dimnames=list(age=1:4, year=2012:2017), units="t")
