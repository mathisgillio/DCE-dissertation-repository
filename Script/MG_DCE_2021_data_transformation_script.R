## Data transformation script 
## Mathis Gillio 
## 09/03/2022


## 0. Load the librairies needed ---- 


library(idefix) # package used to create an efficient design
library(dplyr)
library(tidyr)
library(reshape2)
        
## 1. Import the data needed ---- 

datablock1 <- read.csv("Data/dce1.csv") # load the responses from Google Survey 
datablock2 <- read.csv("Data/dce2.csv")

## Clean data block 1

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

truedesigndummy <- read.table("Data/design-dummy-coded.txt") # load the design matrix 
truedesigndummymatrix <- as.matrix(truedesigndummy) # transform to a matrix to change the titles 

str(truedesigndummymatrix)
str(truedesigndummy)

# Redefine the variables to make the output more readable 

colnames(truedesigndummymatrix) <- c("water","detritus","congestion","biodiversity", "price")

rownames(truedesigndummymatrix) <- c("set1.alt1", "set1.alt2", "set2.alt1", "set2.alt2", 
                                "set3.alt1", "set3.alt2", "set4.alt1", "set4.alt2", 
                                "set5.alt1", "set5.alt2", "set6.alt1", "set6.alt2", 
                                "set7.alt1", "set7.alt2", "set8.alt1", "set8.alt2", 
                                "set9.alt1", "set9.alt2", "set10.alt1", "set10.alt2", 
                                "set11.alt1", "set11.alt2", "set12.alt1", "set12.alt2")

as.data.frame(truedesigndummymatrix) # transform into a data frame to split it 

## Split the design into two blocks 

design11 <- truedesigndummymatrix[1:12, ]      
design22 <- truedesigndummymatrix[13:24, ]

# Transform back to matrix for analysis 
as.matrix(design11)
as.matrix(design22)

# Adapt the ‘design’ to the number of responses (26 for first block and 33 for second)

design11 <- design11[rep(seq_len(nrow(design11)), 26), ]
design22 <-design22[rep(seq_len(nrow(design22)), 33), ]

# Merge responses and design

final11 <- cbind(datablock1, design11)
final22 <- cbind(datablock2, design22)

finaldummy <- rbind(final11, final22)

finaldummy$price <- as.numeric(as.character(finaldummy$price))

str(finaldummy)

write.csv(finaldummy,'Data/finaldata-dummy.csv') # save final data for analysis 





