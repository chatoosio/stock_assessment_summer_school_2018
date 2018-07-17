# EX FLQuant S4.R - DESC
# EX FLQuant S4.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $


library(FLCore)

# CREATE

# (1) Create an FLQuant object with elements numbered sequentially (i.e. 1 to N) and with ages from 1 to 6, years from 2003 to 2012 and four seasons (hint: its the fourth dimension). Call it flq.

# (2) Create an FLQuant with dimensions 6,10,1,1,1,100 and normally-distributed random numbers. Call it flq2.

# SUBSET

# (3) Extract from flq the values for the first three years


# (4) Select from flq leaving out the last age

# (5) What if do not know the precise last age name?


# APPLY

# (6) Calculate the proportion-at-age per year for flq. Suggestion: 
# First step: create an flquant with the total numbers by year and age (i.e. you need to sum all the seasons). 
# Second step: create another flquant with the total numbers by year (i.e. sum all the ages). 
# Third step: create an flquant and fill it with the total numbers by year repeated for all ages.
# Last step: divide your flquant with the total numbers by year and age (first step) by the flquant obtained in the third step to get the proportion at age per year.
#This is the long way... there are also shorter way :)

# (7) How many values in flq are greater than 0?
