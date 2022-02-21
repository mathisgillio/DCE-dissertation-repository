################################################################################
##                         EES 4th year Disseration                           ##
##                          Analysis for the DCE                              ##
##                          Date: 31-January-2021                             ##
##                            Written by:                                     ##
##              Mathis Gillio (s1843841@ed.ac.uk)                             ##
################################################################################

## 1. Load libraries ---- 

library(tidyverse)
library(dplyr)
library(mlogit)
library(survival) # used for the clogit function 
library(lme4) 
library(stargazer)
library(lmtest)

## 2. Load the data ---- 

finaldata <- read.csv("Data/finaldata.csv")

## Create new columns for the indexes 

finaldata$cs.personid <- paste(finaldata$cs, finaldata$personid, sep = "_")
finaldata$id <- 1:nrow(finaldata)

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
                                      cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | 0,
                                    finaldataclean) 


multinomial_logit_model_2 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                      cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3,
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

# Save the output of the model in table 

stargazer(multinomial_logit_model_2, type="text", out="multi.htm")


### 2.c Mixed-effect model ---- 

mixed.lmer <- lmer(choice ~ wat1 + wat2 + det1 + det2 + 
                     cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 + 
                     (1|personid), data = finaldata)

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


### 2.d XLM model ---- 

## Create mlogit data for XLM model (depricated function)

finaldatacleanxlm <- mlogit.data(finaldata, choice = "choice", shape = "long", 
                                 alt.var = "alt", idx = c("personid", "id"))

head(finaldatacleanxlm, 5) # check the indexes 

mixed_logit_model_1 <- mlogit(choice ~ wat1 + wat2 + det1 + det2 + 
                                cong1 + cong2 + bio1 + bio2 + pri1 + pri2 + pri3 | 0, 
                              finaldatacleanxlm,
                              rpar = c(wat1 = "n", wat2 = "n", det1 = "n", det2 = "n",
                                       cong1 = "n", cong2 = "n", bio1 = "n", bio2 = "n", 
                                       pri1 = "n", pri2 = "n", pri3 = "n"),
                              correlation = TRUE,
                              halton = NA, 
                              R = 100, 
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


