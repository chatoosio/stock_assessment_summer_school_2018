# loading_data_into_FLR.R - How to load data from outside R into FLR objects
# loading_data_into_FLR.R

# Copyright 2013 JRC FISHREG. Distributed under the GPL 2 or later
# Maintainer: JRC FISHREG
# $Id: $
# Created: 14/03/2013
# Modified:

#---------------------------------------------------------------
# Basics
#---------------------------------------------------------------

# Load FLCore library
library(FLCore)

# Checking the working directory
getwd()

# Setting the working directory
#setwd("directory name")

# Case is important
# Use // or \ for separating folders and directories in Windows 

#---------------------------------------------------------------
# Reading into R, then creating an FLR object
#---------------------------------------------------------------

# read.table and its friends
?read.table

# Key options to look out for:
# file (obviously)
# header
# sep
# row.names
# col.names

# Save your data as a *.csv file (comma separated file)
# Example file in /Data/catch_numbers.csv
# Note that we have row and column names (ages and years)

# Read this in using read.table() with default options
catch.n <- read.table("hke_9_10_11_catch_n.csv")
catch.n
# Looks terrible
# what just happened?
# The separator in our file is a comma , so we need to specify that
catch.n <- read.table("hke_9_10_11_catch_n.csv", sep=',')

# Better but the column and row names have been included as data
# We can try to fix this using the header and row.names options
catch.n <- read.table("hke_9_10_11_catch_n.csv", header=TRUE, sep=',')


# The column names are ugly (with the Xs) but that is OK for now

# Can use read.csv() instead - same as read.table() but different default options
catch.n <- read.csv("hke_9_10_11_catch_n.csv")
catch.n <- read.csv("hke_9_10_11_catch_n.csv",header = TRUE)

# We have read in the data as a data.frame
class(catch.n)

# There is an FLQuant contructor that uses a data.frame, but here our data.frame is not set up the right way
# Instead we can convert the object to a matrix
catch.n.matrix <- as.matrix(catch.n[1:7, -1])
catch.n.matrix

# We need to specify the dimnames
catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=0:6, year = 2006:2014))
catch.n.flq

# Another option for reading in the data is to omit column and row names from the csv data
# You would use header=FALSE (else you lose the first row of data)
# Also leave the row.names argument empty
catch.n <- read.csv("hke_9_10_11_catch_n.csv", header=FALSE)
catch.n.matrix <- as.matrix(catch.n[2:8, -1])
catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=0:6, year = 2006:2014))

# Read Mean weight at age (catch.wt)
catch.wt <- read.csv("hke_9_10_11_catch_wt.csv", header=FALSE)
catch.wt.matrix <- as.matrix(catch.wt[2:8, -1])
catch.wt.flq <- FLQuant(catch.wt.matrix, dimnames=list(age=0:6, year = 2006:2014))

# So we have two FLquants, one for catch.n and one for catch.wt, we want to combine them in an FLStock. 

# create an FLStock
hke.stk <- FLStock(catch.n = catch.n.flq, catch.wt = catch.wt.flq)
units(harvest(hke.stk)) <- "f"


plot(hke.stk)


# What do we need now to have all the data for a VPA, SCA? 
# NEXT STEPS are to Load

#   discards.n discards.wt
#   landings.n  landings.wt

# Read Natural mortality at age (M)
m <- read.csv("hke_9_10_11_m.csv", header=FALSE)
m.matrix <- as.matrix(m[2:8, -1])
m.flq <- FLQuant(m.matrix, dimnames=list(age=0:6, year = 2006:2014))

#   mat (0, 0.22, 0.87, 1)
mat.flq <- FLQuant(matrix(c(0, 0.22, 0.87, 1, 1, 1, 1), nrow=7, ncol=9), dimnames=list(age=0:6, year = 2006:2014))

#m.spwn(0)
m.spawn <-  FLQuant(matrix(rep(0), nrow=7, ncol=9), dimnames=list(age=0:6, year = 2006:2014))

#f.spawn(0)  
harvest.spawn <-  FLQuant(matrix(rep(0), nrow=7, ncol=9), dimnames=list(age=0:6, year = 2006:2014))
  
# Assemble the full FLStock for HAKE
hke.stk <- FLStock(catch.n = catch.n.flq, 
                   catch.wt = catch.wt.flq,
                   harvest.spwn = harvest.spawn,
                   m.spwn = m.spawn,
                   m = m.flq,
                   mat = mat.flq)

# Add units for example on fising mortality
units(harvest(hke.stk)) <- "f"

# Compute total catch
catch(hke.stk) <- computeCatch(hke.stk)

plot(catch(hke.stk))

# If you get your data into R you can get it into an FLQuant.
# If you get your data into an FLQuant you get it into any FLR object!

# What else do we need to run a stock assessment like? A tuning index, like a 
# trawl survey relative abundance index or a commercial CPUE.

# Load an FLIndex
# take the Tuning index from MEDITS
catch.n_idx <- read.csv("hke_9_10_11_idx.csv", header = TRUE, row.names = 1)
catch.n_idx.matrix <- as.matrix(catch.n_idx[1:6, ])

hke.idx <- FLQuant(catch.n_idx.matrix, dimnames=list(age=0:5, year = 2006:2014))

hke.idx <- FLIndex(catch.n = hke.idx)
hke.idx <- FLIndices(hke.idx)

# plot the only filled slot, catch.n
plot(catch.n(hke.idx[[1]]))

# last thing, Set the timing of the survey!
hke.idx[[1]]@range[c('startf', 'endf')] <- c(0.66,0.75)


#---------------------------------------------------------------
# Reading directly into an FLR object
#---------------------------------------------------------------

# Used for reading data that has already been prepared in a
# specific format:
# Lowestoft VPA suite
# Adapt
# CSA
# ISA

# readVPAFile()
# The Lowestoft VPA format uses lots of text files, with a single index file.
# readVPAFile() reads in a single file (e.g. fishing mortality, catch numbers etc.)
# Here we read in the catches at age of herring in VIa. This is in the file 'canum.txt'
# Look at this raw data file in a text editor
# Read in the file using readVPAFile("name_of_file") (include the file name extension, e.g. .txt)
catch.n <- readVPAFile("data/her-irlw/canum.txt")
# Gives you an FLQuant 
catch.n

# Other way to load your data, if you already have an FLStock saved as .RData, you can load it directly in your R workspace by using the command load()

load("HKE_09_10_11_stk.Rdata")


# readFLStock()
# Reads in a whole FLStock object - providing that the input data has been set up OK!
# Can take formats Lowestoft VPA, Adapt, CSA, ICA
# Example using Lowestoft VPA Format.
# The VPA format uses lots of text files, with a single index file. Here, the index file is "index.txt"
# Open the file "index.txt" in a text editor and take a look at it
# To read in the stock, pass readFLStock() the name of the index file. 
# You also need to tell R where the file is (i.e. which directory or folder it is in).
her <- readFLStock("data/her-irlw/index.txt")
# Gives us FLStock
class(her)
summary(her)

# Easy!
# But note that we have only read in the data used by the stock assessment
# This does not include the estimated harvest rates or stock abundance
harvest(her)
stock.n(her)
# So we need to load these in separately using readVPAFile
stock.n <- readVPAFile("data/her-irlw/n.txt")
stock.n
# set the stock.n slot of her
stock.n(her) <- stock.n
# Do it all in one step for fishing mortality
harvest(her) <- readVPAFile("data/her-irlw/f.txt")
# Note that the units of the harvest slot have not been set - good idea to do this
units(harvest(her)) <- "f"

# Also need to do some tidying up
# Some of the data is inconsistent
# Total landings = sum(numbers * weight)
apply(landings.n(her) * landings.wt(her), 2, sum)
landings(her)
# So make consistent - use the computeLandings() method
landings(her)=computeLandings(her)
# No discard information
discards.wt(her)
discards.n(her)
# Set up the discards and catches
discards.wt(her)=landings.wt(her)
discards.n(her)=0
discards(her)=computeDiscards(her)
catch(her)=landings(her)
catch.wt(her)=landings.wt(her)
catch.n(her)=landings.n(her)
# Set fbar range
range(her)
range(her)[c("minfbar","maxfbar")]=c(3,6)
# Set plusgroup as the working group  (this may need to be done before stock.n and harvest are read in due to change in max. age)
her <- setPlusGroup(her,plusgroup=7)

# Also methods and functions available:
# readFLIndex() - read an abundance index
# readFLIndices() - read several abundance indices
# readMFCL() - for Multifan-CL
# readADMB() - for ADMB

