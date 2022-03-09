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

## Create new columns for the indexes 

finaldatadummy$cs.personid <- paste(finaldatadummy$cs, finaldatadummy$personid, sep = "_")
finaldatadummy$chid <- 1:nrow(finaldatadummy)

## Make the choice logical (TRUE/FALSE)

finaldatadummy <- finaldatadummy %>% 
  mutate(choice = as.logical(choice))

## Change reference levels 

levels(finaldatadummy$water)
levels(finaldatadummy$water) <- c("Acceptable water","Excellent water","Insufficient water")

levels(finaldatadummy$detritus)
levels(finaldatadummy$detritus) <- c("Both removed","Both left","Garbage removed")

levels(finaldatadummy$congestion)
levels(finaldatadummy$congestion) <- c("Very crowded","Crowded","Not crowded")

levels(finaldatadummy$biodiversity)
levels(finaldatadummy$biodiversity) <- c("Moderate biodiversity","High biodiversity","No biodiversity")

### 8. Data analysis ---- 

## 8.a Conditional logit model ---- 

conditional_logit_model_dummy <- clogit(choice ~ water + detritus + congestion + biodiversity
                                        + price + strata(cs), data = finaldatadummy)

conditional_logit_model_dummy # gives the outputs of the model 

conditional_logit_model_dummy$loglik # a log-likelihood at zero and at convergence



## 8.b Multinomial logit model ---- 

# Create the data to be used in mlogit

finaldatadummyclean <- dfidx(finaldatadummy, choice = "choice",
                             idx = list("cs.personid", "alt"), idnames = c("cs", "alt")) 

multinomial_logit_model_dummy <- mlogit(choice ~ water + detritus + congestion + biodiversity
                                        + price, finaldatadummyclean)  # 0 or -1 removes the intercept so just remove it 


finaldatadummyclean2 <- mlogit.data(finaldatadummy, choice = "choice", shape = "long", 
                                    alt.var = "alt", id = "personid")

multinomial_logit_model_dummy2 <- mlogit(choice ~ water + detritus + congestion +
                                           biodiversity + price, finaldatadummyclean2)

# Give summary of the model outputs 

summary(multinomial_logit_model_dummy)
summary(multinomial_logit_model_dummy2)

# Save the output of the model in table 

stargazer(multinomial_logit_model_2, type="text", out="multi.htm")



### 8.c Mixed-effect model ---- 

lm <- lm(choice ~ water + detritus + congestion + biodiversity
         + price, data = finaldatadummy)

summary(lm)
plot(lm)

mixed.lmer <- lmer(choice ~ water + detritus + congestion + biodiversity
                   + price + (1|personid), data = finaldatadummy) # no tendency to vary the intercept 

summary(mixed.lmer) # gives summary of the model 

# Look at plot to check assumptions 

plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# Save the outputs of the model as a table 

stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")



### 8.d Mixed-effects logit model ----

# Create mlogit data for XLM model

finaldatadummycleanxlm <- mlogit.data(finaldatadummy, choice="choice", shape = "long", 
                                       alt.var = "alt", idx = c("personid", "cs"))


mixed_logit_model_dummy <- mlogit(choice ~ water + detritus + congestion + biodiversity 
                                    + price, 
                                    data = finaldatadummycleanxlm,
                                    rpar = c(water = "n", detritus = "n",congestion = "n",
                                             price = "n"),
                                    halton = NA, 
                                    R = 100, 
                                    panel = TRUE)

