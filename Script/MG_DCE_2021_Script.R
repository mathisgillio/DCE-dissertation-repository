################################################################################
##                         EES 4th year Disseration                           ##
##            Generate and efficient design and analysis for a DCE            ##
##              Using the methodology from Pérez-Troncoso (2020)              ##
##                      Date: 10-December-2021                                ##
##                            Written by:                                     ##
##              Mathis Gillio (s1843841@ed.ac.uk)                             ##
################################################################################

### 1. Install and load packages ----

#install.packages("idefix")
#install.packages("support.CEs")
#install.packages("survival")
#install.packages("mlogit")
#install.packages("reshape2")
install.packages("stats")

library(idefix) # package used to create an efficient design
library(dplyr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(mlogit)
library(survival) # used for the clogit function 
library(lme4) 
library(stargazer)
library(lmtest)
library(gmnl)
library(ggplot2)
library(maps)
library(AICcmodavg)
library(stats)

load('Data/d.RData') # load the design for more efficient script 

### 2. Set numer of attributes and levels ----

set.seed(123)
levels <- c(3,3,3,3,4) # create a vector with each element as an attribute 
coding <-c("E","E","E","E","E") # the type of coding that we are going to use in each 
                                # attribut (effects coding in our case) 

### 3. Display the profiles ----

Profiles (lvls = levels, coding = coding) # see the different alternatives from all 
                                          # attributes and levels combination

## 4. Generate design with no priors ----

(3+3+3+3+4)-5 # Calculate number of priors needed
priors <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # specifying vector with prior coefficients

# simulation procedure where 1000 random draws are obtained from a normal distribution 
# with mean equal to the priors specified

s <- diag(length(priors))
sim <- MASS::mvrnorm(n = 1000, mu = priors, Sigma = s)


# 5. Create output with d-efficient design: ----

d <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 12, par.draws = sim,
           best = TRUE) 

# n.alt gives you the number of alternative per choice set 
# n.sets gives you the number of choice sets 

save.image(file = 'Data/d.RData') # save the image so don't have to reload later

## IF loading d.RData: start the code here ---- 

# Choose the design with the lowest D-error from the list of design created from the CEA

design <- d$design
design # final design with lowest D-error 

#write.table(design, 'design1.txt', col.names=NA) # save the design 

### 6. Decode the design set ---- 

lvls <- list(c("Insuffisante", "Tolérable", "Excellente"),
             c("Déchets enlevés", "Déchets et algues enlevés", "Déchets et algues laissés sur la plage"),
             c("Très congestionée", "Moyennent congestionée", "Peu congestioné"), 
             c("Biodiversité élévée", "Biodiversité moyenne", "Pas de biodiversité"),
             c("0€", "10€", "25€", "40€")) # specify names of attirbute levels to be decoded

Dd <- Decode(des = d$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd # visualize the decoded choice set


### 7. Data cleaning ----

datablock1 <- read.csv("Data/dce1.csv") # load the responses from Google Survey 
datablock2 <- read.csv("Data/dce2.csv")

str(datablock1) # look at the structure of the data loaded 

## 7.a Reshape the data for block 1 ---- 

datablock1 <- datablock1  %>% 
  select(-(1:2)) %>% # remove non-needed data (date stamp)
  select(-(13:16)) %>% # remove socio-economic data and follow-up questions 
  filter(!row_number() %in% c(2, 10, 29)) %>% # remove rows with uncomplete data 
  mutate(personid = row_number()) %>% # create personid column
  relocate(personid) %>% # move the new column to the front
  rename("1" = Choix.1 
         ,"2" = Choix.2
         ,"3" = Choix.3
         ,"4" = Choix.4
         ,"5" = Choix.5
         ,"6" = Choix.6
         , "gender" = Quel.est.votre.genre...
         , "age" = Quel.est.votre.âge...
         , "study_level" = Quel.est.votre.niveau.d.études..
         , "children" = Avez.vous.des.enfants.de.moins.de.15.ans...
         , "toursist_dependent" = Votre.principale.source.de.revenus.dépend.elle.de.la.présence.de.touristes.pendant.l.été.....
         , "club_member" = Faites.vous.partie.d.un.club.ou.association.qui.organisent.des.activités.liées.à.la.préservation.de.l.environnement.....) 
# rename columns back to English 
  
## Change columns titles to character values 

datablock1$'1' <- as.character(datablock1$'1')
datablock1$'2' <- as.character(datablock1$'2')
datablock1$'3' <- as.character(datablock1$'3')
datablock1$'4' <- as.character(datablock1$'4')
datablock1$'5' <- as.character(datablock1$'5')
datablock1$'6' <- as.character(datablock1$'6')

## Change the choice people made between beaches to a more readable output 

datablock1$'1'[datablock1$'1' == "Plage 1"] <- "A"
datablock1$'1'[datablock1$'1' == "Plage 2"] <- "B"

datablock1$'2'[datablock1$'2' == "Plage 1"] <- "A"
datablock1$'2'[datablock1$'2' == "Plage 2"] <- "B"

datablock1$'3'[datablock1$'3' == "Plage 1"] <- "A"
datablock1$'3'[datablock1$'3' == "Plage 2"] <- "B"

datablock1$'4'[datablock1$'4' == "Plage 1"] <- "A"
datablock1$'4'[datablock1$'4' == "Plage 2"] <- "B"

datablock1$'5'[datablock1$'5' == "Plage 1"] <- "A"
datablock1$'5'[datablock1$'5' == "Plage 2"] <- "B"

datablock1$'6'[datablock1$'6' == "Plage 1"] <- "A"
datablock1$'6'[datablock1$'6' == "Plage 2"] <- "B"

## Put the date in long format

datablock1 <- pivot_longer(datablock1, cols = 2:7, names_to = "variable", 
                           values_to = "value")

# Add a row per alternative: 

datablock1 <-rbind(datablock1, datablock1) # bind the data set twice 
datablock1 <- datablock1[order(datablock1$personid, datablock1$variable),]

# Create an alternative id

x <- nrow(datablock1)/2
alt <- rep(1:2, x) # define the alternative (beach 1 or beach 2)
datablock1 <- cbind(datablock1, alt) # add to the data set 

# Create a choice set column 

cs <- rep(1:x, each = 2)
cs <- sort(cs)
datablock1 <- cbind(datablock1,cs)

## Creating the choice variable that will take value 1 if the alternative in its row 
#  is selected or 0 otherwise

datablock1 <- mutate(datablock1, 
                     choice = ifelse(value == "A" & alt == "1" | value== "B" & alt=="2",
                                     1, 0))

## 7.b Reshape the data for block 2 ---- 

datablock2 <- datablock2  %>% 
  select(-(1:2)) %>% # remove non-needed data
  select(-(13:16)) %>% 
  mutate(personid = row_number()) %>% # create personid column
  select(-personid) %>% # move the new column to the front
  mutate(personid = 27:59) %>% 
  relocate(personid) %>% 
  rename("1"=Choix.1
         ,"2"=Choix.2
         ,"3"=Choix.3
         ,"4"=Choix.4
         ,"5"=Choix.5
         ,"6"=Choix.6
         , "gender" = Quel.est.votre.genre..
         , "age" = Quel.est.votre.âge..
         , "study_level" = Quel.est.votre.niveau.d.études..
         , "children" = Avez.vous.des.enfants.de.moins.de.15.ans..
         , "toursist_dependent" = Votre.principale.source.de.revenus.dépend.elle.de.la.présence.de.touristes.pendant.l.été..
         , "club_member" = Faites.vous.partie.d.un.club.ou.association.qui.organisent.des.activités.liées.à.la.préservation.de.l.environnement..) 
# rename columns back to English 

## Change columns titles to character values 

datablock2$'1' <- as.character(datablock2$'1')
datablock2$'2' <- as.character(datablock2$'2')
datablock2$'3' <- as.character(datablock2$'3')
datablock2$'4' <- as.character(datablock2$'4')
datablock2$'5' <- as.character(datablock2$'5')
datablock2$'6' <- as.character(datablock2$'6')

## Change the choice people made between beaches to a more readable output

datablock2$'1'[datablock2$'1' == "Plage 1"] <- "A"
datablock2$'1'[datablock2$'1' == "Plage 2"] <- "B"

datablock2$'2'[datablock2$'2' == "Plage 1"] <- "A"
datablock2$'2'[datablock2$'2' == "Plage 2"] <- "B"

datablock2$'3'[datablock2$'3' == "Plage 1"] <- "A"
datablock2$'3'[datablock2$'3' == "Plage 2"] <- "B"

datablock2$'4'[datablock2$'4' == "Plage 1"] <- "A"
datablock2$'4'[datablock2$'4' == "Plage 2"] <- "B"

datablock2$'5'[datablock2$'5' == "Plage 1"] <- "A"
datablock2$'5'[datablock2$'5' == "Plage 2"] <- "B"

datablock2$'6'[datablock2$'6' == "Plage 1"] <- "A"
datablock2$'6'[datablock2$'6' == "Plage 2"] <- "B"

## Put the date in long format: 

datablock2 <- pivot_longer(datablock2, cols = 2:7, names_to = "variable", 
                           values_to = "value")

# Add a row per alternative: 

datablock2 <-rbind(datablock2, datablock2)
datablock2 <- datablock2[order(datablock2$personid, datablock2$variable),]

# Create an alternative id: 

x <- nrow(datablock2)/2
alt <- rep(1:2, x)
datablock2 <- cbind(datablock2, alt)

# Create a choice set column: 

cs <- rep(157:354, each = 2)
cs <- sort(cs)
datablock2 <- cbind(datablock2,cs)

## Creating the choice variable that will take value 1 if the alternative in its row 
#  is selected or 0 otherwise

datablock2 <- mutate(datablock2, 
                     choice = ifelse(value == "A" & alt == "1" | value== "B" & alt=="2",
                                     1, 0))

### 7.c Combine the two blocks ---- 

## Split the design matrix into the two blocks 

truedesign <- read.table("Data/design.txt") # load the design matrix 
truedesignmatrix <- as.matrix(truedesign) # transform to a matrix to change the titles 

str(truedesignmatrix)
str(truedesign)

# Redefine the variables to make the output more readable 

colnames(truedesignmatrix) <- c("wat1", "wat2", "det1", "det2", "cong1", "cong2", 
                                "bio1", "bio2", "pri1", "pri2", "pri3")

rownames(truedesignmatrix) <- c("set1.alt1", "set1.alt2", "set2.alt1", "set2.alt2", 
                                "set3.alt1", "set3.alt2", "set4.alt1", "set4.alt2", 
                                "set5.alt1", "set5.alt2", "set6.alt1", "set6.alt2", 
                                "set7.alt1", "set7.alt2", "set8.alt1", "set8.alt2", 
                                "set9.alt1", "set9.alt2", "set10.alt1", "set10.alt2", 
                                "set11.alt1", "set11.alt2", "set12.alt1", "set12.alt2")

as.data.frame(truedesignmatrix) # transform into a data frame to split it 

## Split the design into two blocks 

design1 <- truedesignmatrix[1:12, ]      
design2 <- truedesignmatrix[13:24, ]

# Transform back to matrix for analysis 
as.matrix(design1)
as.matrix(design2)

# Adapt the ‘design’ to the number of responses (26 for first block and 33 for second)

design1 <- design1[rep(seq_len(nrow(design1)), 26), ]
design2 <-design2[rep(seq_len(nrow(design2)), 33), ]

# Merge responses and design

final1 <- cbind(datablock1, design1)
final2 <- cbind(datablock2, design2)

final <- rbind(final1, final2)

write.csv(final,'Data/finaldata.csv') # save final data for analysis 

### 7.d Further transform the data for analysis ---- 

## Load the data 

finaldatadummy <- read.csv("Data/finaldata-dummy.csv")

## Make price as numeric to estiamte WTP 

finaldatadummy$price <- as.numeric(as.character(finaldatadummy$price))

finaldatadummy <- finaldatadummy %>% 
  # Create relevant ID variables from X
  separate(col = `X`, sep = "\\.", into = c("cs.new", "alt.new"), extra = "drop") %>%
  # NB choice situation ID plus alternative ID must define unique data points
  # without reference to person ID for dfidx to work, so combine to new index:
  mutate(cs.new.person = paste0(cs.new, "_", personid) ) %>%
  # Further data prep
  mutate(
    # Make the choice logical (TRUE/FALSE)
    choice = as.logical(choice),
    # Set baseline factor levels
    water = factor(water, levels = c("Acceptable water","Excellent water","Insufficient water")),
    detritus = factor(detritus, levels = c("Both removed","Both left","Garbage removed")),
    congestion = factor(congestion, levels = c("Very crowded","Crowded","Not crowded")),
    biodiversity = factor(biodiversity, levels = c("Moderate biodiversity","High biodiversity","No biodiversity"))
  )

### 8. Data analysis ---- 

## 8.a Conditional logit model ---- 

# NB Remove intercept since alternatives were unnamed
conditional_logit_model_dummy <- clogit(choice ~ 0 + water + detritus + congestion + biodiversity
                                        + price + strata(cs), data = finaldatadummy)

conditional_logit_model_dummy # gives the outputs of the model 

conditional_logit_model_dummy$loglik # a log-likelihood at zero and at convergence


## 8.b Multinomial logit model ---- 

# Create the data to be used in mlogit

mlogit_data <- mlogit.data(finaldatadummy, choice = "choice", shape = "long",
                 alt.var = "alt.new", chid.var = "cs.new.person", id.var = "personid")

mnl_model <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, data = mlogit_data)
mnl_model_null <- mlogit(choice ~ 1, data = mlogit_data)
# Give summary of the model outputs 

summary(mnl_model)
summary(mnl_model_null)

1 - (-161.48/-243.54) # calcualtion of McFaden r-squared manually using null 
                      # model log-likelihood: 0.34

# 𝜌2 can be interpreted like: values from 0.2-0.4 indicate (in McFadden's words) excellent model fit

# Save the output of the model in table 

stargazer(mnl_model, type = "text", title="Multinomial model regression Results",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "",
          align=TRUE, dep.var.labels=c("Coefficients"),
          single.row=TRUE,
          covariate.labels=c("Excellent water quality","Insufficient water quality",
                             "Garbage and algea left on the beach","Algea left on the beach",
                             "Moderate congestion","Little congestion",
                             "High biodiversity", "Low biodiversity", "Price"),
          add.lines = list(c("McFaden R-squared", "0.34")),
          out="mnl.html")

### 8.c Generalized Linear model ---- 

glm <- glm(choice ~ 0 + water + detritus + congestion + biodiversity
         + price, family = binomial, data = finaldatadummy)

glm
summary(glm)
plot(glm)

# Look at the difference between the Null deviance (variability explained by a null model
# and the Residual deviance: the amount of variability that remains after you’ve explained 
# some away by your explanatory variable. 

981.5 - 774.08

# The bigger the reduction in deviance, the better a job your model is doing at 
# explaining a relationship.

pchisq(774.08, 698, lower.tail = FALSE) # p-value: the residual deviance is significant: 
                                        # evidence against the model? 

# The functions that can be used to extract results from the fit include: 

#residuals() or resid(), for the deviance residuals
#fitted() or fitted.values(), for the fitted values (estimated probabilities)
#predict(), for the linear predictor (estimated logits)
#coef() or coefficients(), for the coefficients 
#deviance(), for the deviance

# Save the outputs of the model as a table 

stargazer(glm, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


### 8.d Mixed-effects logit model ----

# Create mlogit data for XLM model

m1 <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, 
                               rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n", 
                                        "detritusBoth left" = "n", "detritusGarbage removed" = "n", 
                                        "congestionCrowded" = "n", "congestionNot crowded" = "n", 
                                        "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n", 
                                        "price" = "n"),
                               panel = TRUE, R = 100, mlogit_data)

m1a <- update(m1, rpar = c("waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1b <- update(m1, rpar = c("waterExcellent water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1c <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1d <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1e <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1f <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1g <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityNo biodiversity" = "n",
                           "price" = "n"))
m1h <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n",
                           "price" = "n"))
m1i <- update(m1, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", "biodiversityNo biodiversity" = "n"))

AIC(m1, m1a, m1b, m1c, m1d, m1e, m1f, m1g, m1h, m1i) %>% arrange(AIC)

m2 <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, 
             rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n", 
                      "detritusBoth left" = "n", "detritusGarbage removed" = "n", 
                      "congestionCrowded" = "n", "congestionNot crowded" = "n", 
                      "biodiversityHigh biodiversity" = "n", 
                      "price" = "n"),
             panel = TRUE, R = 100, mlogit_data)

m2a <- update(m2, rpar = c("waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n",
                           "price" = "n"))
m2b <- update(m2, rpar = c("waterExcellent water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m2c <- update(m2, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m2d <- update(m2, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m2e <- update(m2, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m2f <- update(m2, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m2g <- update(m2, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "price" = "n"))
m2h <- update(m2, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionCrowded" = "n", "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n"))

AIC(m2, m2a, m2b, m2c, m2d, m2e, m2f, m2g, m2h) %>% arrange(AIC)

m3 <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, 
             rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                      "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                      "congestionNot crowded" = "n",
                      "biodiversityHigh biodiversity" = "n", 
                      "price" = "n"),
             panel = TRUE, R = 100, mlogit_data)

m3a <- update(m3, rpar = c("waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n",
                           "price" = "n"))
summary(m3a)
m3b <- update(m3, rpar = c("waterExcellent water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m3c <- update(m3, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusGarbage removed" = "n",
                            "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m3d <- update(m3, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n",
                            "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m3e <- update(m3, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "biodiversityHigh biodiversity" = "n", 
                           "price" = "n"))
m3f <- update(m3, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionNot crowded" = "n",
                           "price" = "n"))

m3g <- update(m3, rpar = c("waterExcellent water" = "n", "waterInsufficient water" = "n",
                           "detritusBoth left" = "n", "detritusGarbage removed" = "n",
                           "congestionNot crowded" = "n",
                           "biodiversityHigh biodiversity" = "n"))

AIC(m3, m3a, m3b, m3c, m3d, m3e, m3f, m3g) %>% arrange(AIC)

summary(m3)

mixed_logit_model_final <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, 
             rpar = c( "waterInsufficient water" = "n", 
                      "price" = "n"),
             panel = TRUE, R = 100, mlogit_data)

AIC(mixed_logit_model_final)

summary(mixed_logit_model_final)

### Plot model 

mixed_logit_model_final <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, 
                              rpar = c("waterInsufficient water" = "n", 
                                       "price" = "n"),
                              panel = TRUE, R = 100, mlogit_data)

summary(mixed_logit_model_final)

model_null <- mlogit(choice ~ 1, data = mlogit_data)

summary(model_null)


stargazer(mixed_logit_model_final, type = "text", title="Mixed model regression Results",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          digit.separator = "", 
          align=TRUE, dep.var.labels=c("Coefficients"), 
          single.row=TRUE,
          covariate.labels=c("Excellent water quality","Insufficient water quality",
                             "Garbage and algea left on the beach","Algea left on the beach",
                             "Moderate congestion","Little congestion",
                             "High biodiversity", "Low biodiversity", "Price", "sd.Insufficient Water"),
          add.lines = list(c("McFaden R-squared", "0.37")),
          out="mnl.html")

1 - (-152.5/-243.54) # calcualtion of McFaden r-squared manually using null 
# model log-likelihood: 0.37
### 8.e Latent class model ---- 

library(devtools)
install_bitbucket("mauricio1986/gmnl")
library(gmnl)

lc <- gmnl(choice ~ 0 + water + detritus + congestion + biodiversity + price | 0 | 0 | 0 | 1, 
           data = mlogit_data,
           model = 'lc', 
           Q = 3,
           method = "bfgs")

summary(lc)
AIC(lc)
BIC(lc)


lc2 <- gmnl(choice ~ 0 + water + detritus + congestion + biodiversity + price| 0 | 0 | 0 | 1, 
            data = mlogit_data, 
            model = 'lc',
            Q = 2, 
            method = 'bfgs')

summary(lc2)
AIC(lc2)
BIC(lc2)


# Share of individuals in class 2: 37% class 1 (63%)
exp(coef(lc2)["(class)2"]) / (exp(0) + exp(coef(lc2)["(class)2"]))

# WTP 
-coef(lc2)["class.1.waterExcellent water"] / coef(lc2)["class.1.price"] #28.3

# Save output in table: 

summary(lc3)
AIC(lc3)
BIC(lc3)

E### 9. Socio-economic and follow up questions ----  

## 9.a Socio-economic ---- 

summary(finaldatadummy$gender)
408+300

# percentage of females: 58
408*100/708
# percentage of males: 42
300*100/708

summary(finaldatadummy$age)

# percentage of 18-29: 35
252*100/708

# percentage of 30-49: 36
252*100/708

# percentage of 50-64: 17 
120*100/708

# percentage of 65+: 12
84*100/708

summary(finaldatadummy$study_level)

# percentage of apprenticeship: 12
84*100/708

# percentage of highschool: 15
108*100/708

# percentage of bachelor: 37
(36+228)*100/708

# percentage of Master: 36
252*100/708

summary(finaldatadummy$children)

# percentage of people with children under 15: 20
144*100/708

# percentage of people without children under 15: 80
564*100/708

summary(finaldatadummy$toursist_dependent)

# percentage of people who depend on tourists as their main source of revenue: 8
60*100/708

# percentage of people who depend on tourists as their main source of revenue: 92
648*100/708

summary(finaldatadummy$club_member)

# percentage of people who are part of a conservation group: 15
108*100/708

# percentage of people who depend on tourists as their main source of revenue: 85
600*100/708

## 9.b Follow-up questions ---- 

datafollowup1 <- read.csv("Data/dce1.csv") # load the responses from Google Survey 
datafollowup2 <- read.csv("Data/dce2.csv")

str(datafollowup1)
str(datafollowup2)

datafollowup1 <- datafollowup1 %>% 
  select(-(1:14)) %>% 
  rename("environmnetal_awarness" = Avant.cette.étude..étiez.vous.conscient.des.dangers.environnementaux.auxquels.font.face.les.écosystèmes.littoraux....
         , "natural_parc_awarness" = Connaissez.vous.l.existence.de.sites.naturels.protégés.sur.le.Cap.d.Antibes.et.îles.de.Lérins...
         , "beach_access" = Accepteriez.vous.de.voir.votre.accès.aux.plages.limité.pendant.l.été.pour.limiter.la.pression.sur.l.environnement..l.accès.pourrait.par.exemple.prendre.la.forme.d.un.enregistrement.à.l.avance.....
         , "green_label" = Pensez.vous.que.le.développement.d.un.label.vert.pourrait.mieux.vous.aider.à.faire.un.choix.quant.à.quelle.plage.choisir...)

datafollowup2 <- datafollowup2 %>% 
  select(-(1:14)) %>% 
  rename("environmnetal_awarness" = Avant.cette.étude..étiez.vous.conscient.des.dangers.environnementaux.auxquels.font.face.les.écosystèmes.littoraux....
         , "natural_parc_awarness" = Connaissez.vous.l.existence.de.sites.naturels.protégés.sur.le.Cap.d.Antibes.et.îles.de.Lérins..
         , "beach_access" = Accepteriez.vous.de.voir.votre.accès.aux.plages.limité.pendant.l.été.pour.limiter.la.pression.sur.l.environnement..l.accès.pourrait.par.exemple.prendre.la.forme.d.un.enregistrement.à.l.avance...
         , "green_label" = Pensez.vous.que.le.développement.d.un.label.vert.pourrait.mieux.vous.aider.à.faire.un.choix.quant.à.quelle.plage.choisir..)

finalfollowupdata <- rbind(datafollowup1, datafollowup2)

summary(finalfollowupdata$environmnetal_awarness)

# percentage of people that were aware of conservation issues: 90
56*100/62

# percentage of people who depend on tourists as their main source of revenue: 10
6*100/62

summary(finalfollowupdata$natural_parc_awarness)

# percentage of people that were aware of the existence of Natura 2000 parcs: 81
50*100/62

# percentage of people who depend on tourists as their main source of revenue: 19
12*100/62

summary(finalfollowupdata$beach_access)

# percentage of people that would accept seeing their access to beach limited: 60
37*100/62

# percentage of people who would not accept seeing their access to beach limited: 40
25*100/62

summary(finalfollowupdata$green_label)

# percentage of people that would benefit from green beach label: 81
50*100/62

# percentage of people who would not benefit from green beach label: 19
12*100/62

### 10. Map of Antibes and location surveys ---- 

library("tidyverse")           # For cleaning/wrangling data                    
library("dplyr")               # For cleaning/wrangling data                 
library("ggplot2")             # For plotting data                  
library("sf")                  # For plotting map data              

theme_set(theme_bw()) # setting default map background colors to black & white

theme_diss <- function(){            # creating a new theme function
  # define font, font sizes, text angle and alignment
  theme(plot.title    = element_text(size = 20, 
                                     face = "bold"),
        plot.subtitle = element_text(size = 16, 
                                     face = "plain"),
        axis.title    = element_text(size = 15,
                                     face = "bold"),
        axis.text.x   = element_text(size = 12,
                                     angle = 45,
                                     vjust = 1,
                                     hjust = 1, 
                                     face = "bold"), 
        axis.text.y   = element_text(size = 12, 
                                     face = "bold"),
        legend.position = "none",                                # remove legend
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # create plot margins
        panel.grid = element_blank())
}

france <- map_data("world") %>% # creating dataframe for coordinates of all countries
  rename("Countries" = "region")  %>%  # renaming "region" column to "Countries"
  filter(Countries == "France") %>% 
  filter(is.na(subregion))

antibes <- data.frame(
  long = c(7.10831),
  lat = c(43.58579),
  stringsAsFactors = FALSE)  

(francemap <- ggplot() + 
  geom_polygon(data = france, aes(x=long, y = lat, group = group), fill = "grey95", color = "black") + 
  coord_fixed(1.3) +
  geom_point(data = antibes, aes(x = long, y = lat), color = "coral", size = 9) +
  annotate("text", x = 7.5, y = 42.8, label = "Antibes", size = 12) + 
  theme_diss() + 
    theme(axis.text.x      =   element_blank(),
          axis.text.y      =   element_blank(),
          axis.ticks       =   element_blank(),
          axis.title.x     =   element_blank(),
          axis.title.y     =   element_blank(),
          #panel.border     =   element_blank(),
          panel.grid.major =   element_blank(),
          panel.grid.minor =   element_blank()))

ggsave(francemap, filename = "Pictures/france_map.png", width = 10, height = 3)

## Calculation of percentage of bathing water quality status of Antibes' beaches

## In 2019: 

# Insufficient: 2 (18%)
# Sufficent: 3 (27%)
# Good: 6 (54%)


# In 2021: 

# Insufficient: 3 (13%)
# Sufficient: 7 (32%)
# Good: 3 (13%)
# Excellent: 9 (41%)



