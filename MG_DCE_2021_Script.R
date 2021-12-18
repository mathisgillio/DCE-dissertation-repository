### Title: DCE script 
### Purpose: This repository was created to generate an efficient choice set
### Name: Mathis Gillio

# This script was written using the methodology from Treats et al. (2020)

### 1. LOAD packages and objects ----

#install.packages("idefix")
#install.packages("support.CEs")
library(idefix) # package used to create an efficient design
load('d.RData')
library(support.CEs)

### 2. Set numer of attributes and levels ----

set.seed(123)
levels <- c(3,3,3,3,4) # create a vector with each element as an attribute 
coding <-c("E","E","E","E","E") # the type of coding that we are going to use in each attribute
                                # using effects coding in our case 


### 3. Display the profiles ----

Profiles (lvls = levels, coding = coding) # see the different alternatives from all attributes
                                          # and levels combination
(3^4)*5


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
sim <- MASS::mvrnorm(n = 500, mu = priors, Sigma = s)

# Create a list for the coefficients: 
#sim <- list(sim[, 1:12])

# 5. Create output with d-efficient design: ----

d <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 16, par.draws = sim,
         best = TRUE) 


save.image(file='d.RData')
dir()

# n.alt gives you the number of alternative per choice set 
# n.sets gives you the number of choice sets 

design <- d$design # create best design object 
design # show the best design 

### 6. ---- Decode the design set ---- 

lvls <- list(c("Insuffisante", "Tolérable", "Bonne", "Excellente"),
             c("Déchets enlevés", "Déchets et herbes marines mortes enlevés", "Déchets et herbes laissés sur la plage"),
             c("Très congestionée", "Moyennent congestionée", "Peu congestioné"), 
             c("Biodiversité élévée", "Biodiversité moyenne", "Pas de biodiversité"),
             c("0€", "10€", "25€", "40€"))

Dd <- Decode(des = d$design, lvl.names = lvls, n.alts = 2, coding = coding)

Dd # visualize the decoded choice set


### 'DB' efficient design: takes a really long time ---- 

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
              
