### Title: DCE script 
### Purpose: This repository was created to generate an efficient choice set
### Name: Mathis Gillio

# This script was written using the methodology from Pérez-Troncoso (2020)

### 1. LOAD packages and objects ----

#install.packages("idefix")
#install.packages("support.CEs")
library(idefix) # package used to create an efficient design
library(support.CEs)
load('d.RData')
library(dplyr)
library(tidyr)
#install.packages("reshape2")
library(reshape2)

# The package support.CEs (Aizaki 2012) provides functions for generating orthogonal 
# main-effect arrays, but does not support optimal designs for discrete choice models

### 2. Set numer of attributes and levels ----

set.seed(123)
levels <- c(3,3,3,3,4) # create a vector with each element as an attribute 
coding <-c("E","E","E","E","E") # the type of coding that we are going to use in each attribute
                                # using effects coding in our case 

# Attributes can be effects coded "E", dummy coded "D" or treated as a continuous 
# variable "C". Here, all attributes will be effects coded.

### 3. Display the profiles ----

Profiles (lvls = levels, coding = coding) # see the different alternatives from all attributes
                                          # and levels combination
(3^4)*4


### Generate a D-efficent design (use Fedorov modified algorithm in idefix package)

# By reducing D-error we are getting close to the principles of good DCE design: 
# orthogonality, level balance, minimal overlap, and utility balance

## 4. Generate design with no priors ----

# Calculate number of priors needed: 
(3+3+3+3+4)-5
priors <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # specifying vector with prior coefficients

# simulation procedure where 1000 random draws are obtained from a normal distribution 
# with mean equal to the priors specified

s <- diag(length(priors))
sim <- MASS::mvrnorm(n = 1000, mu = priors, Sigma = s)

# Since the computation time of generating a DB efficient design depends on the number 
# of draws, we advise to generate designs with a sample such that the computation 
# remains feasible.

# Create a list for the coefficients: 
#sim <- list(sim[, 1:12])

# 5. Create output with d-efficient design: ----

d <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 12, par.draws = sim,
           best = TRUE) 

# n.alt gives you the number of alternative per choice set 
# n.sets gives you the number of choice sets 

save.image(file='d.RData') # save the image so don't have to reload later
dir()

# Choose the design with the lowest D-error from the list of design created from the CEA

design <- d$design
design

### 6. ---- Decode the design set ---- 

lvls <- list(c("Insuffisante", "Tolérable", "Excellente"),
             c("Déchets enlevés", "Déchets et algues enlevés", "Déchets et algues laissés sur la plage"),
             c("Très congestionée", "Moyennent congestionée", "Peu congestioné"), 
             c("Biodiversité élévée", "Biodiversité moyenne", "Pas de biodiversité"),
             c("0€", "10€", "25€", "40€"))

Dd <- Decode(des = d$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd # visualize the decoded choice set

## Transform into a table to split the design matrix in two 

#write.table(design, 'design.txt', col.names=NA)

truedesign <- read.table("design.txt")
truedesignmatrix <- as.matrix(truedesign)

colnames(truedesignmatrix) <- c("Var11", "Var12", "Var21", "Var22", "Var31", "Var32", 
                                "Var41", "Var42", "Var51", "Var52", "Var53")
rownames(truedesignmatrix) <- c("set1.alt1", "set1.alt2", "set2.alt1", "set2.alt2", 
                                "set3.alt1", "set3.alt2", "set4.alt1", "set4.alt2", 
                                "set5.alt1", "set5.alt2", "set6.alt1", "set6.alt2", 
                                "set7.alt1", "set7.alt2", "set8.alt1", "set8.alt2", 
                                "set9.alt1", "set9.alt2", "set10.alt1", "set10.alt2", 
                                "set11.alt1", "set11.alt2", "set12.alt1", "set12.alt2")



### 7. Data Anlysis ----

datablock1 <- read.csv("Data/dce1.csv")
datablock2 <- read.csv("Data/dce2.csv")

str(datablock1)

## 7.a Reshape the data for block 1 ---- 

datablock1 <- datablock1  %>% 
  select(-(1:2)) %>% # remove non-needed data
  select(-(7:16)) %>% 
  filter(!row_number() %in% c(2, 10, 29)) %>% 
  mutate(personid = row_number()) %>% # create personid column
  relocate(personid) %>% # move the new column to the front
  rename("1"=Choix.1
         ,"2"=Choix.2
         ,"3"=Choix.3
         ,"4"=Choix.4
         ,"5"=Choix.5
         ,"6"=Choix.6)
  
# personid length = 26

datablock1$'1' <- as.character(datablock1$'1')
datablock1$'2' <- as.character(datablock1$'2')
datablock1$'3' <- as.character(datablock1$'3')
datablock1$'4' <- as.character(datablock1$'4')
datablock1$'5' <- as.character(datablock1$'5')
datablock1$'6' <- as.character(datablock1$'6')

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

## Put the date in long format: 

datablock1 <- pivot_longer(datablock1, cols = 2:7, names_to = "variable", values_to = "value")


# Now we need a row per alternative: 

datablock1 <-rbind(datablock1, datablock1)
datablock1 <- datablock1[order(datablock1$personid, datablock1$variable),]

# Create an alternative id

x <- nrow(datablock1)/2
alt <- rep(1:2, x)
datablock1 <- cbind(datablock1, alt)


cs <- rep(1:x, each = 2)
cs <- sort(cs)
datablock1 <- cbind(datablock1,cs)

## Creating the choice variable that will take value 1 if the alternative in its row 
#  is selected or 0 otherwise

datablock1 <- mutate(datablock1, choice = ifelse(value == "A" & alt == "1" | value== "B" & alt=="2", 1, 0))


## 7.a Reshape the data for block 2---- 

datablock2 <- datablock2  %>% 
  select(-(1:2)) %>% # remove non-needed data
  select(-(7:16)) %>% 
  mutate(personid = row_number()) %>% # create personid column
  relocate(personid) %>% # move the new column to the front
  rename("1"=Choix.1
         ,"2"=Choix.2
         ,"3"=Choix.3
         ,"4"=Choix.4
         ,"5"=Choix.5
         ,"6"=Choix.6)

# personid lenght = 33

datablock2$'1' <- as.character(datablock2$'1')
datablock2$'2' <- as.character(datablock2$'2')
datablock2$'3' <- as.character(datablock2$'3')
datablock2$'4' <- as.character(datablock2$'4')
datablock2$'5' <- as.character(datablock2$'5')
datablock2$'6' <- as.character(datablock2$'6')

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

datablock2 <- pivot_longer(datablock2, cols = 2:7, names_to = "variable", values_to = "value")


# Now we need a row per alternative: 

datablock2 <-rbind(datablock2, datablock2)
datablock2 <- datablock2[order(datablock2$personid, datablock2$variable),]

# Create an alternative id

x <- nrow(datablock2)/2
alt <- rep(1:2, x)
datablock2 <- cbind(datablock2, alt)


cs <- rep(1:x, each = 2)
cs <- sort(cs)
datablock2 <- cbind(datablock2,cs)

## Creating the choice variable that will take value 1 if the alternative in its row 
#  is selected or 0 otherwise

datablock2 <- mutate(datablock2, choice = ifelse(value == "A" & alt == "1" | value== "B" & alt=="2", 1, 0))


# Split the design matrix into the two blocks 

as.data.frame(truedesignmatrix)

design1 <- truedesignmatrix[1:12, ]      
design2 <- truedesignmatrix[13:24, ]

as.matrix(design1)
as.matrix(design2)

# adapt the ‘design’ to the number of responses

design1 <- design1[rep(seq_len(nrow(design1)), 26), ]
design2 <-design2[rep(seq_len(nrow(design2)), 33), ]

# merge responses and design

final1 <- cbind(datablock1, design1)
final2 <- cbind(datablock2, design2)

final <- rbind(final1, final2)

write.csv(final,'finaldata.csv')



