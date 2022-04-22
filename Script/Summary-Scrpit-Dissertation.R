
## Summary of script used to design, process and analyse the DCE 

### Load relevant packages ---- 

library(idefix) # package used to create an efficient design
library(dplyr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(mlogit)

### A. Design the DCE ---- 

## A.1. Set numer of attributes and levels ----

set.seed(123)
levels <- c(3,3,3,3,4) # create a vector with each element as an attribute 
coding <-c("E","E","E","E","E") # the type of coding that we are going to use in each 
# attribut (effects coding in our case) 

## A.2 Generate design with no priors ----

(3+3+3+3+4)-5 # Calculate number of priors needed
priors <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # specifying vector with prior coefficients

# simulation procedure where 1000 random draws are obtained from a normal distribution 
# with mean equal to the priors specified

s <- diag(length(priors))
sim <- MASS::mvrnorm(n = 1000, mu = priors, Sigma = s)


## A.3 Create output with d-efficient design: ----

d <- CEA(lvls = levels, coding = coding, n.alts = 2, n.sets = 12, par.draws = sim,
         best = TRUE) 

# n.alt gives you the number of alternative per choice set 
# n.sets gives you the number of choice sets 

# Choose the design with the lowest D-error from the list of design created from the CEA

design <- d$design
design # final design with lowest D-error 

## A.4 Decode the design set for better visualization ---- 

lvls <- list(c("Insuffisante", "Tolérable", "Excellente"),
             c("Déchets enlevés", "Déchets et algues enlevés", "Déchets et algues laissés sur la plage"),
             c("Très congestionée", "Moyennent congestionée", "Peu congestioné"), 
             c("Biodiversité élévée", "Biodiversité moyenne", "Pas de biodiversité"),
             c("0€", "10€", "25€", "40€")) # specify names of attirbute levels to be decoded

Dd <- Decode(des = d$design, lvl.names = lvls, n.alts = 2, coding = coding)

### B. Merge design and responses 

## B.1 Load the responses (in our case 2 blocks) ----

datablock1 <- read.csv("Data/dce1.csv") # load the responses from Google Survey 
datablock2 <- read.csv("Data/dce2.csv")


## B.2 Reshape the data (reapeat the same procedure for block 2) ---- 

datablock1 <- datablock1  %>% 
  select(-(1:2)) %>% # remove non-needed data (date stamp)
  mutate(personid = row_number()) %>% # create personid column
  relocate(personid) %>% # move the new column to the front
  rename("1" = Choix.1 
         ,"2" = Choix.2
         ,"3" = Choix.3
         ,"4" = Choix.4
         ,"5" = Choix.5
         ,"6" = Choix.6) 

## Change the choice people made between beaches to a more readable output 
## (repeat for all choice sets) 

datablock1$'1'[datablock1$'1' == "Plage 1"] <- "A"
datablock1$'1'[datablock1$'1' == "Plage 2"] <- "B"

## Put the date in long format

datablock1 <- pivot_longer(datablock1, cols = 2:7, names_to = "variable", 
                           values_to = "value")

## Add a row per alternative and create an alternative id

datablock1 <-rbind(datablock1, datablock1) # bind the data set twice 
datablock1 <- datablock1[order(datablock1$personid, datablock1$variable),]

x <- nrow(datablock1)/2
alt <- rep(1:2, x) # define the alternative (beach 1 or beach 2)
datablock1 <- cbind(datablock1, alt) # add to the data set 

## Create a choice set column 

cs <- rep(1:x, each = 2)
cs <- sort(cs)
datablock1 <- cbind(datablock1,cs)

## Creating the choice variable that will take value 1 if the alternative in its row 
##  is selected or 0 otherwise

datablock1 <- mutate(datablock1, 
                     choice = ifelse(value == "A" & alt == "1" | value== "B" & alt=="2",
                                     1, 0))

### B.3 Combine the two blocks ---- 

## Split the design matrix into the two blocks 

truedesignmatrix <- read.table("Data/design.txt") # load the design matrix 

## Split the design into two blocks 

design1 <- truedesignmatrix[1:12, ]      
design2 <- truedesignmatrix[13:24, ]

## Transform from table to matrix for analysis 
as.matrix(design1)
as.matrix(design2)

## Adapt the ‘design’ to the number of responses (in this study: 26 for first block and 33 for second)

design1 <- design1[rep(seq_len(nrow(design1)), 26), ]
design2 <-design2[rep(seq_len(nrow(design2)), 33), ]

## Merge responses and design

final1 <- cbind(datablock1, design1)
final2 <- cbind(datablock2, design2)

finaldata <- rbind(final1, final2)

### C. Transform the data for mlogit analysis ---- 

## Make price as numeric to estiamte WTP 

finaldata$price <- as.numeric(as.character(finaldatadummy$price)) 

## C.1. Create relevant ID to use in mlogit, make choice logical and set basline levels

finaldata <- finaldata %>% 
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

## C.2. Create mlogit data 

mlogit_data <- mlogit.data(finaldata, choice = "choice", shape = "long",
                           alt.var = "alt.new", chid.var = "cs.new.person", id.var = "personid")

## C.3. Perform stepwise regression analysis and build the final model 

mixed_logit_model_final <- mlogit(choice ~ 0 + water + detritus + congestion + biodiversity + price, 
                                  rpar = c( "waterInsufficient water" = "n", 
                                            "price" = "n"),
                                  panel = TRUE, R = 100, mlogit_data)

summary(mixed_logit_model_final) # visualize final output 

