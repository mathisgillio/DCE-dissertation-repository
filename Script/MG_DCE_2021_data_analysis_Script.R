################################################################################
##                         EES 4th year Disseration                           ##
##                          Analysis for the DCE                              ##
##                          Date: 31-January-2021                             ##
##                            Written by:                                     ##
##              Mathis Gillio (s1843841@ed.ac.uk)                             ##
################################################################################

## 1. Load libraries ---- 

#install.packages("gmnl")
#install.packages("mlogit")
library(tidyverse)
library(dplyr)
library(mlogit)
library(survival) # used for the clogit function 
library(lme4) 
library(stargazer)
library(lmtest)
library(gmnl)

## 2. Load the data ---- 

finaldata <- read.csv("Data/finaldata.csv")
finaldatadummy <- read.csv("Data/finaldata-dummy.csv")

str(finaldata)
str(finaldatadummy)

finaldatadummy$price <- as.numeric(as.character(finaldatadummy$price))

## Create new columns for the indexes 

finaldata$cs.personid <- paste(finaldata$cs, finaldata$personid, sep = "_")
finaldata$index <- 1:nrow(finaldata)

# Make the choice logical (TRUE/FALSE)

finaldata <- finaldata %>% 
  mutate(choice = as.logical(choice))

# Create the data to be used in mlogit

finaldataclean <- dfidx(finaldata, choice = "choice", idx = list("cs.personid", "alt"), 
                        idnames = c("cs", "alt"))  

head(finaldataclean, 5) # check the indexes created 


#### 2. Data analysis ---- 

# The missing coeffecients (wat3, det3, cong3, bio3 and pri4) can be calculated as 
# the negative sum of the other coefficients in the attributes 

### 2.a CLM model ---- 

conditional_logit_model <- clogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                    cong1 + cong2 + bio1 + bio2 + pri1 + pri2 
                                  + pri3 + strata(cs), data = finaldata)

conditional_logit_model # gives the outputs of the model 

### 2.b MNL model ---- 

multinomial_logit_model_1 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                      cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | 0, # 0 or _1 removes the intercept so just remove it 
                                    finaldataclean) 


multinomial_logit_model_2 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                      cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3,
                                    finaldataclean) 

multinomial_logit_model_4 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                      cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | age,
                                    finaldataclean) 


multinomial_logit_model_3 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                      cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | 0,
                                    finaldataclean,
                                    rpar = c(wat1 = "n", wat2 = "n", det1 = "n", det2 = "n",
                                             cong1 = "n", cong2 = "n", bio1 = "n", bio2 = "n", 
                                             pri1 = "n", pri2 = "n", pri3 = "n"), 
                                    R = 100,
                                    correlation = TRUE,
                                    halton = NA)

## Give summary of the model outputs 

summary(multinomial_logit_model_1)
summary(multinomial_logit_model_2)
summary(multinomial_logit_model_3)
summary(multinomial_logit_model_4)

# Save the output of the model in table 

stargazer(multinomial_logit_model_2, type="text", out="multi.htm")


### 2.c Mixed-effect model ---- 

mixed.lmer <- lmer(choice ~ wat1 + wat2 + det1 + det2 + 
                     cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 + 
                     (1|age) + (1|gender) + (1|study_level), data = finaldata) # no tendency to vary the intercept 

summary(mixed.lmer) # gives summary of the model 

## Look at plot to check assumptions 

plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

## Save the outputs of the model as a table 

stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


### 2.d XLM model ---- (comes out with an error)

## Create mlogit data for XLM model (depricated function)

finaldatacleanxlm <- mlogit.data(finaldata, choice = "choice", shape = "long", 
                                 alt.var = "alt", idx = c("personid", "id"))


finaldatacleanxlm2 <- mlogit.data(finaldata, choice="choice", shape = "long", 
                                  alt.var = "alt", idx = c("personid", "cs"))

head(finaldatacleanxlm2, 5) # check the indexes 

mixed_logit_model_1 <- mlogit(formula = choice ~ wat1 + wat2 + det1 + det2 + 
                                cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | -1 | 0, 
                              finaldatacleanxlm2,
                              rpar = c(wat1 = "n", wat2 = "n", det1 = "n", det2 = "n",
                                       cong1 = "n", cong2 = "n", bio1 = "n", bio2 = "n", 
                                       pri1 = "n", pri2 = "n", pri3 = "n"),
                              halton = NA, 
                              R = 100, 
                              print.level = 0, 
                              panel = TRUE)

mixed_logit_model_2 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | -1 | 0, 
                              finaldataclean,
                              rpar = c(wat1 = "n", wat2 = "n", det1 = "n", det2 = "n",
                                       cong1 = "n", cong2 = "n", bio1 = "n", bio2 = "n", 
                                       pri1 = "n", pri2 = "n", pri3 = "n"),
                              halton = NA, 
                              R = 100, 
                              print.level = 0,
                              panel = TRUE)

finaldata$alt <- as.character(finaldata$alt)

finaldatacleanxlm2 <- mlogit.data(finaldata, choice = "choice", shape = "long", 
                                  alt.var = "alt", id.var = "index")

gmnl(formula = choice ~ 0 + wat1 + wat2 + det1 + det2 + 
       cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3, data = finaldatacleanxlm2,
     model = "mixl", ranp = c(wat1 = "n", wat2 = "n", det1 = "n", det2 = "n",
                              cong1 = "n", cong2 = "n", bio1 = "n", bio2 = "n", 
                              pri1 = "n", pri2 = "n", pri3 = "n"), R = 100, haltons = NA,
     panel = TRUE, bi = "mixl.bi", method = "bfgs")

### 2.e Latent class model 

finaldatacleanxlm <- mlogit.data(finaldata, choice = "choice", shape = "long", 
                                 alt.var = "alt", idx = c("personid", "id"))

# Estimate a LC-MNL model with 3 classes

library(devtools)
install_bitbucket("mauricio1986/gmnl")
library(gmnl)

lc <- gmnl(choice ~ wat1 + wat2 + det1 + det2 + 
             cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | 0 | 0 | 0 | 1, 
           data = finaldatacleanxlm,
           model = 'lc', 
           Q = 3,
           method = "bfgs")


#### SAME but for dummy coded data ---- 

## Create new columns for the indexes 

finaldatadummy$cs.personid <- paste(finaldatadummy$cs, finaldatadummy$personid, sep = "_")
finaldatadummy$chid <- 1:nrow(finaldatadummy)

# Make the choice logical (TRUE/FALSE)

finaldatadummy <- finaldatadummy %>% 
  mutate(choice = as.logical(choice))

levels(finaldatadummy$water)
levels(finaldatadummy$water) <- c("Acceptable water","Excellent water","Insufficient water")

levels(finaldatadummy$detritus)
levels(finaldatadummy$detritus) <- c("Both removed","Both left","Garbage removed")

levels(finaldatadummy$congestion)
levels(finaldatadummy$congestion) <- c("Very crowded","Crowded","Not crowded")

levels(finaldatadummy$biodiversity)
levels(finaldatadummy$biodiversity) <- c("Moderate biodiversity","High biodiversity","No biodiversity")

# Create the data to be used in mlogit

finaldatadummyclean <- dfidx(finaldatadummy, choice = "choice",
                             idx = list("cs.personid", "alt"), idnames = c("cs", "alt"))  

head(finaldatadummyclean, 5) # check the indexes created 
str(finaldatadummyclean)

#### 2. Data analysis ---- 

### 2.a CLM model ---- 

conditional_logit_model_dummy <- clogit(choice ~ water + detritus + congestion + biodiversity
                                  + price + strata(cs), data = finaldatadummy)

conditional_logit_model_dummy # gives the outputs of the model 

conditional_logit_model_dummy$loglik # a log-likelihood at zero and at convergence

### 2.b MNL model ---- 

multinomial_logit_model_dummy <- mlogit(choice ~ water + detritus + congestion + biodiversity
                                        + price, finaldatadummyclean)  # 0 or -1 removes the intercept so just remove it 
                      
## Give summary of the model outputs 

summary(multinomial_logit_model_dummy)

# Save the output of the model in table 

stargazer(multinomial_logit_model_2, type="text", out="multi.htm")


### 2.c Mixed-effect model ---- 

mixed.lmer <- lmer(choice ~ water + detritus + congestion + biodiversity
                   + price + (1|age) + (1|gender) + (1|study_level), data = finaldatadummy) # no tendency to vary the intercept 

summary(mixed.lmer) # gives summary of the model 

## Look at plot to check assumptions 

plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

## Save the outputs of the model as a table 

stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

### 2.d XLM model ---- (comes out with an error)

## Create mlogit data for XLM model (depricated function)

finaldatadummycleanxlm <- mlogit.data(finaldatadummy, choice = "choice", shape = "long", 
                                 alt.var = "alt", idx = c("personid", "id"))


finaldatadummycleanxlm2 <- mlogit.data(finaldatadummy, choice="choice", shape = "long", 
                                  alt.var = "alt", idx = c("personid", "cs"))

head(finaldatacleanxlm2, 5) # check the indexes 

mixed_logit_model_dummy_1 <- mlogit(choice ~ water + detritus + congestion + biodiversity 
                              + price, 
                              data = finaldatadummyclean,
                              rpar = c(water = "n", detritus = "n",congestion = "n",
                                       price = "n"),
                              halton = NA, 
                              R = 100, 
                              panel = TRUE)

gmnl(formula = choice ~ 0 + water + detritus + congestion + biodiversity + price, 
     data = finaldatadummycleanxlm2,
     model = "mixl", ranp = c(water = "n", detritus = "n", congestion = "n", biodiversity = "n", price = "n"), 
     R = 100, 
     haltons = NA,
     panel = TRUE, 
     bi = "mixl.bi",
     method = "bfgs")





