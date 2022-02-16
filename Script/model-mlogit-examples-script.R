
### Test script for the use of mlogit model 

## Install packages: 

install.packages("mlogit")
install.packages("Ecdat")
install.packages("AER")
install.packages("dfidx")
library(mlogit)
library(Ecdat)
library(AER)
library(dfidx)

# Load the data: 

data("Fishing", package = "mlogit")
Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")

summary(mlogit(mode ~ price + catch, data = Fish)) # a pure conditional model 

summary(mlogit(mode ~ 0 | income, data = Fish)) # a multinomial model 

m <- mlogit(mode ~ price + catch | income, data = Fish) # a mixed model 
summary(m)

## same model with a subset of alternatives : charter, pier, beach
m <- mlogit(mode ~ price + catch | income, data = Fish,
            alt.subset = c("charter", "pier", "beach"))
summary(m)

# a mixed logit model 

rpl <- mlogit(mode ~ price + catch | income, Fishing, varying = 2:9,
              rpar = c(price= 'n', catch = 'n'), correlation = TRUE,
              alton = NA, R = 50)
summary(rpl)


# OTHER EXCERICE 

# Reshape the data using Raw using the mlogit.data function

data("ModeChoice", package = "Ecdat")
head(ModeChoice, 5)

Mo <- mlogit.data(ModeChoice, choice = "mode", shape = "long", 
                  alt.levels = c("air","train", "bus", "car"))

# OTHER EXERCICE: 
  
data("TravelMode", package = "AER")
head(TravelMode, 5)

TravelMode <- mlogit.data(TravelMode, choice = "choice", shape = "long",
                          alt.var = "mode", chid.var = "individual",
                          drop.index = FALSE)

## Create a variable of income only for the air mode

TravelMode$avinc <- with(TravelMode, (mode == 'air') * income)

## Estimate the model on all alternatives, with car as the base level
## like in Greene's book.

x <- mlogit(choice ~ wait + gcost + avinc, TravelMode, reflevel = "car")

## Estimate the same model for ground modes only (the variable avinc
## must be dropped because it is 0 for every observation

g <- mlogit(choice ~ wait + gcost, TravelMode, reflevel = "car",
            alt.subset = c("car", "bus", "train"))

## Compute the test

hmftest(x,g)

# OTHER EXERCISE 

data("TravelMode", package = "AER")
head(TravelMode, 5)

# the first two columns contain the index

TM11 <- dfidx(TravelMode)
head(TM11, 5)

# explicitely indicate the two indexes using either a vector or a
# list of two characters

TM22 <- dfidx(TravelMode, idx = c("individual", "mode"))
head(TM22, 5)

TM3 <- dfidx(TravelMode, idx = list("individual", "mode"))
head(TM3, 5)

# rename one or both indexes

TM3b <- dfidx(TravelMode, idnames = c(NA, "trmode"))
head(TM3b, 5)

# for balanced data (with observations ordered by the first, then
# by the second index

# use the name of the first index

TM4 <- dfidx(TravelMode, idx = "individual", idnames = c("individual", "mode"))
head(TM4, 5)

# or an integer equal to the cardinal of the first index

TM5 <- dfidx(TravelMode, idx = 210, idnames = c("individual", "mode"))
head(TM5, 5)

# Indicate the values of the second index using the levels argument

TM5b <- dfidx(TravelMode, idx = 210, idnames = c("individual", "mode"),
              levels = c("air", "train", "bus", "car"))
head(TM5b, 5) 

str(TM5b)

# Nesting structure for one of the index

data("JapaneseFDI", package = "mlogit")
JapaneseFDI <- dplyr::select(JapaneseFDI, 1:8)
JP1b <- dfidx(JapaneseFDI, idx = list("firm", c("region", "country")),
              idnames = c("japf", "iso80"))

# Data in wide format

data("Fishing", package = "mlogit")
Fi <- dfidx(Fishing, shape = "wide", varying = 2:9, idnames = c("chid", "alt"))

# OTHER EXERCISE: 
  
data("Electricity", package = "mlogit")
head(Electricity, 5)

# First, we need to coerce the data to a dfidx object
# This allows for a panel with multiple indices
# For further documentation, see dfidx.

Electricity$index <- 1:nrow(Electricity)
elec = dfidx(Electricity, idx = list(c("index", "id")),
             choice = "choice", varying = 3:26, sep = "")

# We then estimate individual choice over electricity providers for
# different cost and contract structures with a suppressed intercept

my_mixed_logit = mlogit(data = elec, 
                        formula = choice ~ 0 + pf + cl + loc + wk + tod + seas,
                        # Specify distributions for random parameter estimates
                        # "n" indicates we have specified a normal distribution
                        # note pf is omitted from rpar, so it will not be estimated as random
                        rpar = c(cl = "n", loc = "u", wk = "n", tod = "n", seas = "n"), 
                        # R is the number of simulation draws
                        R = 100, 
                        # For simplicity, we won't include correlated parameter estimates
                        correlation = FALSE, 
                        # This data is from a panel
                        panel = TRUE)

# Results
summary(my_mixed_logit)

# Note that this output will include the simulated coefficient estimates, 
# simulated standard error estimates, and distributional details for the
# random coefficients (all, in this case)
# Note also that pf is given as a point estimate, and mlogit does not generate
# a distribution for it as it does the others

# You can extract and summarize coefficient estimates using the rpar function

marg_loc = rpar(my_mixed_logit, "loc")
summary(marg_loc)

# You can also normalize coefficients and distributions by, say, price

cl_by_pf = rpar(my_mixed_logit, "cl", norm = "pf")
summary(cl_by_pf)

