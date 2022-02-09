### Title: DCE script 
### Purpose: This repository was created to generate an efficient choice set
### Name: Mathis Gillio

# This script was written using the methodology from Pérez-Troncoso (2020)

### 1. LOAD packages and objects ----

#install.packages("idefix")
#install.packages("support.CEs")
library(idefix) # package used to create an efficient design
library(support.CEs)
load('Script/d.RData')

# The package support.CEs (Aizaki 2012) provides functions for generating orthogonal 
# main-effect arrays, but does not support optimal designs for discrete choice models

### 2. Set numer of attributes and levels ----

set.seed(123)
levels <- c(3,3,3,3,4) # create a vector with each element as an attribute 
coding <-c("E","E","E","E","E") # the type of coding that we are going to use in each attribute
                                # using effects coding in our case 

# Attributes can be effects coded "E", dummy coded "D" or treated as a continuous 
# variable "C". In this case all attributes will be effects coded.

### 3. Display the profiles ----

Profiles (lvls = levels, coding = coding) # see the different alternatives from all attributes
                                          # and levels combination
(3^4)*4


### Generate a D-efficent design (use Fedorov modified algorithm in idefix package)

#By reducing D-error we are getting close to the principles of good DCE design: 
# orthogonality, level balance, minimal overlap, and utility balance

## 4. Generate design with no priors ----

# Calculate number of priors needed: 
(3+3+3+3+4)-5
priors <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # specifying vector with prior coefficients

# simulation procedure where 500 random draws are obtained from a normal distribution 
# with mean equal to the priors specified

s <- diag(length(priors))
sim <- MASS::mvrnorm(n = 1000, mu = priors, Sigma = s)

# Since the computation time of generating a DB efficient design depends on the number 
# of draws, we advise to generate designs with a sample such that the computation 
# remains feasible.

# Create a list for the coefficients: 
#sim <- list(sim[, 1:12])

# 5. Create output with d-efficient design: ----

d <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 14, par.draws = sim,
         best = TRUE) 

d16 <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 16, par.draws = sim,
           best = TRUE) 

d12 <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 12, par.draws = sim,
           best = TRUE) 


save.image(file='d.RData')
dir()

# n.alt gives you the number of alternative per choice set 
# n.sets gives you the number of choice sets 

design <- d$design # create best design object 
design # show the best design 

design16 <- d16$design
design16

design12 <- d12$design
design12

### 6. ---- Decode the design set ---- 

lvls <- list(c("Insuffisante", "Tolérable", "Excellente"),
             c("Déchets enlevés", "Déchets et algues enlevés", "Déchets et algues laissés sur la plage"),
             c("Très congestionée", "Moyennent congestionée", "Peu congestioné"), 
             c("Biodiversité élévée", "Biodiversité moyenne", "Pas de biodiversité"),
             c("0€", "10€", "25€", "40€"))

Dd <- Decode(des = d$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd16 <- Decode(des = d16$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd12 <- Decode(des = d12$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd # visualize the decoded choice set

# As previously mentioned, besides statistical efficiency other criteria such as
# attribute level balance can be of importance too.

Dd16

Dd12


### 'DB' efficient design: takes a really long time (using the Modfed function) ---- 

cs <- Profiles(lvls = c(3,3,3,3,5), coding = coding)

D <- Modfed(cand.set = cs, n.sets = 12, n.alts = 2, par.draws = sim)

### L^ma design ---- 

attribute.names <- list(Waterquality = c("Poor", "Sufficient", "Good", "Excellent"),
                      Posidonia = c("Removed during summer", "removed all year long", "Not removed"), 
                      Biodiversity = c("High biodiversity", "Average biodiveristy", "Low biodiversity"),
                      Litter = c("Highly", "Moderatly", "Not littered"),
                      Cost = c("0", "5", "10","25", "40"))

LMA.design <- Lma.design(attribute.names = attribute.names, 
           nalternatives = 2, nblocks = 1, row.renames = TRUE, 
           seed = NULL)


questionnaire(choice.experiment.design = LMA.design) 


### 7. Data Anlysis ----

## 7.a Reshape the data ---- 


              
