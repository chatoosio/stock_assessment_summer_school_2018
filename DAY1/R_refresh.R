# Refresher on R data types and operations on data

# EXPRESSIONS

# First operations

# Assignment

# To create objects in our workspace, use <- (alternatively, =)

oa <- 4

ob = 5

# Type its name to inspect

ob

# Calling functions, always use ()

length(ob)

log(oa)

exp(ob)

sqrt(oa)

# Arithmetic

ob + ob

ob + 10

oa * 2

ob ^ oa

# Nesting operations

log(((ob + 2) * (oa / 3)) ^ 2)

# Comparison

ob > oa

ob <= oa

# List variables in workspace

ls()


# NUMERIC VECTORS

# Vectors can be created as a sequence, using : or seq()

ov <- 1:10

seq(1, 10, by=2)
seq(0, 10, length=20)

length(seq(1,10, by=0.5))

# or by contatenating elements, using c()

on <- c(1,5,7,6,3,4,4,1,2,3)

# Many functions operate on vectors naturally

mean(on)

var(on)

# Arithmetics

ov + ov

ov + 10

on * 2

ov ^ on

# Subsetting can select one or more elements, including or excluding

ov[3]

ov[3:5]

ov[-1]

ov[c(3,5,6)]

ov[-c(6,7,8)]

ov[length(ov)]

ov[length(ov)-1]

ov[ov>2]

ov[ov>2 & ov<=6]

# Information on a vector, like
# length

length(ov)

# summary of values

summary(ov)


# When in doubt, use is()
is(ov)

is.numeric(ov)
is.vector(ov)

# STRINGS (CHARACTERS)

# Vectors can also consist of strings

# Use quotes to create them

a <- "welcome to"
b <- "Module II"

# We can combine them in a longer string

msg <- c(a,b)

# Using paste converts to character if needed
paste(a, b, 2018)

# Checking data type
is.vector(msg)

is.numeric(msg)

is.character(msg)


# FACTOR

# Character data with a limited set of possible values

dire <- c("N", "S", "W", "E")

dire <- as.factor(dire)

levels(dire)

dire[1] <- "S"

# Values not in levels() are not valid

dire[2] <- "P"

# To drop unused levels use factor()

dire <- factor(dire)

# LOGICAL

# R has explicit logical elements: TRUE and FALSE

res <- c(TRUE, FALSE)


# what are they?

is(res)

is.logical(res)

# Logicals are commonly used for subsetting

dat <- 1:4

dat[c(TRUE, FALSE, FALSE, TRUE)]
dat[c(TRUE, TRUE, FALSE, TRUE)]


# SPECIAL VALUES

# R has special representations of

# Inf

Inf

1/0

# Not-a-number

NaN

1/0 - 1/0


# Not Available

NA

dat <- c(1, 4, 8, 9, NA, 12)

dat

is.na(dat)

# Some functions can deal with NA as required
mean(dat)

res <- mean(dat, na.rm=TRUE)

sum(dat, na.rm=TRUE)


# Exercise 01

# 1. Create a vector of length 15, elements 1-7 increasing from 1, and 8-15 decreasing from 25

# How many ways can you find of doing it?

# 2. Substitute the 3rd and 7th element with NA

# 3. Calculate mean of the vector

# 4. Change all NA to 0




# MATRIX

# A 2D structure of either numeric or character elements

# Constructed using matrix()

matrix(1:10, ncol=10, nrow=10)

matrix(rnorm(10), ncol=10, nrow=10)

mat <- matrix(rnorm(100), ncol=10, nrow=10)

# Subsetting 

# always remember that a matrix has 2 dimensions and you always have to sepcify both of them when subsetting with []

mat[1, 2]

mat[1,]

mat[1:4,]

# Get size using dim

dim(mat)

length(mat)

# R works column first, unless instructed otherwise

a <- matrix(1:16, nrow=4)

b <- matrix(1:16, nrow=4, byrow=TRUE)

# An important method for matrices is apply()

mat <- matrix(1:10, ncol=10, nrow=10)

apply(mat, 2, sum)

apply(mat, 1, sum)

# Arithmetics work element-wise

mat + 2

mat * 2

mat - mat


# Exercise 02

# 1. Create a matrix with 10 rows and 15 columns, containing random numbers from a normal distribution

# 2. Subset the first 5 rows and columns of this matrix (5*5)

# 3. Sum the matrix by row



# ARRAY

# An array is an n-dimensional extension of a matrix

# Created by array(), specifying dim and dimnames

array(1:100, dim=c(5, 5, 4))

arr <- array(1:25, dim=c(5, 5, 4))

# You can have as many dimensions as you want

array(1:100, dim=c(5, 5, 2, 2))

# Subsetting works as in matrix, on all dims (count the commas)

arr[1:3, 1:5, 4]

# but be careful with dimensions collapsing

arr[1,,]

arr[1,3,]

# Arithmetic (element by element) is well defined

arr * 2

arr + (arr / 2)


# DATA FRAME 

# A 2D table of data

# Example:

year <- seq(2000, 2010)
catch <- c(900, 1230, 1400, 930, 670, 1000, 960, 840, 900, 500,400)

dat <- data.frame(year=year, catch=catch)

# A data.frame can be inspect using

# summary

summary(dat)

# or head/tail

head(dat)
tail(dat)

# and its size with dim

dim(dat)

# Access individual columns using $

dat$year

# or, either by name

dat[, 'catch']

# or position

dat[, 2]

# Same for rows

dat[1:5,]

# Selection based on boolean logic comes handy
# e.g. select those rows matching year=2004

dat[dat$year==2004,]

# Adding extra columns of various types
dat$area <- rep(c("N", "S"), length=11)

dat$survey <- c(TRUE, FALSE, FALSE, rep(TRUE, 5), FALSE, TRUE, TRUE)

dat


# Exercise 03

# 1. Create one numeric vector of random numbers and one character vector of the same length

# 2. Create a data frame with them

# 3. Add a column with 0 or 1 for values in numeric column > or <= 5

# 4. Extract rows with value of new column equal to 1



# LIST

# A list is a very flexible container. Element can be of any class and size

lst <- list(data=dat, description="Some data we cooked up")

# Extract elements by name using $

lst$dat

is.data.frame(lst$dat)

# or using [ and [[
# [ name/position subsets the list

lst[1]

is(lst[1])

# while [[ extracts the subset element
lst[[1]]

is(lst[[1]])


# Lists can be nested. Use with caution!

lst$repetition <- lst






