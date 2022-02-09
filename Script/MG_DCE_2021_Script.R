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

d12 <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 12, par.draws = sim,
           best = TRUE) 

# n.alt gives you the number of alternative per choice set 
# n.sets gives you the number of choice sets 

save.image(file='d.RData') # save the image so don't have to reload later
dir()

# Choose the design with the lowest D-error from the list of design created from the CEA

design12 <- d12$design
design12

### 6. ---- Decode the design set ---- 

lvls <- list(c("Insuffisante", "Tolérable", "Excellente"),
             c("Déchets enlevés", "Déchets et algues enlevés", "Déchets et algues laissés sur la plage"),
             c("Très congestionée", "Moyennent congestionée", "Peu congestioné"), 
             c("Biodiversité élévée", "Biodiversité moyenne", "Pas de biodiversité"),
             c("0€", "10€", "25€", "40€"))

Dd12 <- Decode(des = d12$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd12 # visualize the decoded choice set

# As previously mentioned, besides statistical efficiency other criteria such as
# attribute level balance can be of importance too.


### 7. Data Anlysis ----

## 7.a Reshape the data ---- 


              
