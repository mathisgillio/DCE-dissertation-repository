## Follow-up questions script 
## Mathis Gillio 
## Dissertation work Marsh 2022

## Load the packages ---- 

library(dplyr)
library(tidyverse)
library(ggplot2) 
library(plyr)

library(RColorBrewer)

install.packages("tidytext")
install.packages("R.utils")
library(tidytext)
library(R.utils)
library(viridis)
library (RColorBrewer) 

display.brewer.all (colorblindFriendly = T)
brewer.pal (n = 3, name = "YlGnBu")

## Load the data ---- 

data1 <- read.csv("Data/dce1.csv") # load the responses from Google Survey 
data2 <- read.csv("Data/dce2.csv")

## Modify data ---- 

str(data1)

data1 <- data1  %>% 
  select(-(1:8)) %>% # remove non-needed data (date stamp)
  filter(!row_number() %in% c(2, 10, 29)) %>% # remove rows with uncomplete data 
  rename("gender" = Quel.est.votre.genre...
         , "age" = Quel.est.votre.âge...
         , "study_level" = Quel.est.votre.niveau.d.études..
         , "children" = Avez.vous.des.enfants.de.moins.de.15.ans...
         , "toursist_dependent" = Votre.principale.source.de.revenus.dépend.elle.de.la.présence.de.touristes.pendant.l.été.....
         , "club_member" = Faites.vous.partie.d.un.club.ou.association.qui.organisent.des.activités.liées.à.la.préservation.de.l.environnement.....
         , "awarness" = Avant.cette.étude..étiez.vous.conscient.des.dangers.environnementaux.auxquels.font.face.les.écosystèmes.littoraux....
         , "natura2000" = Connaissez.vous.l.existence.de.sites.naturels.protégés.sur.le.Cap.d.Antibes.et.îles.de.Lérins...
         , "access" = Accepteriez.vous.de.voir.votre.accès.aux.plages.limité.pendant.l.été.pour.limiter.la.pression.sur.l.environnement..l.accès.pourrait.par.exemple.prendre.la.forme.d.un.enregistrement.à.l.avance.....
         , "label" = Pensez.vous.que.le.développement.d.un.label.vert.pourrait.mieux.vous.aider.à.faire.un.choix.quant.à.quelle.plage.choisir...) 
# rename columns back to English 

data1$age <- as.character(data1$age)
data1$age[data1$age == "66 + ans"] <- "65 + ans"
data1$age <- as.factor(data1$age)

str(data2)

data2 <- data2  %>% 
  select(-(1:8)) %>% # remove non-needed data
  rename("gender" = Quel.est.votre.genre..
         , "age" = Quel.est.votre.âge..
         , "study_level" = Quel.est.votre.niveau.d.études..
         , "children" = Avez.vous.des.enfants.de.moins.de.15.ans..
         , "toursist_dependent" = Votre.principale.source.de.revenus.dépend.elle.de.la.présence.de.touristes.pendant.l.été..
         , "club_member" = Faites.vous.partie.d.un.club.ou.association.qui.organisent.des.activités.liées.à.la.préservation.de.l.environnement..
         , "awarness" = Avant.cette.étude..étiez.vous.conscient.des.dangers.environnementaux.auxquels.font.face.les.écosystèmes.littoraux....
         , "natura2000" = Connaissez.vous.l.existence.de.sites.naturels.protégés.sur.le.Cap.d.Antibes.et.îles.de.Lérins..
         , "access" = Accepteriez.vous.de.voir.votre.accès.aux.plages.limité.pendant.l.été.pour.limiter.la.pression.sur.l.environnement..l.accès.pourrait.par.exemple.prendre.la.forme.d.un.enregistrement.à.l.avance...
         , "label" = Pensez.vous.que.le.développement.d.un.label.vert.pourrait.mieux.vous.aider.à.faire.un.choix.quant.à.quelle.plage.choisir..) 
# rename columns back to English 

data2$study_level <- as.character(data2$studylevel)
data2$study_level[data2$study_level == "Bachelor"] <- "License"
data2$study_level <- as.factor(data2$studylevel)

finaldata <- rbind(data1, data2)

str(finaldata)

finaldata$age <- as.character(finaldata$age)
finaldata$age[finaldata$age == "18 - 29 ans"] <- "18-29"
finaldata$age[finaldata$age == "30 - 49 ans"] <- "30-49"
finaldata$age[finaldata$age == "50 - 64 ans"] <- "50-64"
finaldata$age[finaldata$age == "65 + ans"] <- "65 +"
finaldata$age <- as.factor(finaldata$age)

finaldata$study_level <- as.character(finaldata$study_level)
finaldata$study_level[finaldata$study_level == "Apprentissage"] <- "Apprenticeship"
finaldata$study_level[finaldata$study_level == "Baccalauréat"] <- "Baccalaureate"
finaldata$study_level[finaldata$study_level == "License"] <- "Bachelor"
finaldata$study_level[finaldata$study_level == "Master ou plus"] <- "Master or more"
finaldata$study_level <- as.factor(finaldata$study_level)

finaldata$awarness <- as.character(finaldata$awarness)
finaldata$awarness[finaldata$awarness == "Non"] <- "Not aware"
finaldata$awarness[finaldata$awarness == "Oui"] <- "Aware"
finaldata$awarness <- as.factor(finaldata$awarness)

finaldata <- finaldata %>% 
  mutate(awarness = factor(awarness, levels = c("Not aware","Aware")))

finaldata$natura2000 <- as.character(finaldata$natura2000)
finaldata$natura2000[finaldata$natura2000 == "Non"] <- "Not aware"
finaldata$natura2000[finaldata$natura2000 == "Oui"] <- "Aware"
finaldata$natura2000 <- as.factor(finaldata$natura2000)

finaldata <- finaldata %>% 
  mutate(natura2000 = factor(natura2000, levels = c("Not aware","Aware")))

## Plot some figures ---- 

### Awarness with age

finaldata_awarness_age <- ddply(finaldata,.(age), 
      function(x) with(x,
                       data.frame(100*round(table(awarness)/length(awarness),2))))

(bar_plot_awarness_age <- ggplot(finaldata_awarness_age, aes(x = age, group = awarness, y = Freq, fill = awarness)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", aes(fill = awarness)) +
    scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
    theme_bw() +
    ylab("Percentage of awarness\n") +                             
    xlab("Age")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

## Awarness and study level 

finaldata_awarness_studylevel <- ddply(finaldata,.(study_level), 
                           function(x) with(x,
                                            data.frame(100*round(table(awarness)/length(awarness),2))))

(bar_plot_awarness_studylevel <- ggplot(finaldata_awarness_studylevel, aes(x = study_level, group = awarness, y = Freq, fill = awarness)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", aes(fill = awarness)) +
    scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
    theme_bw() +
    ylab("Percentage of awarness\n") +                             
    xlab("Study level")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

### Natura 2000 with age 

finaldata_natura_age <- ddply(finaldata,.(age), 
                                function(x) with(x,
                                                 data.frame(100*round(table(natura2000)/length(natura2000),2))))

(bar_plot_natura_age <- ggplot(finaldata_natura_age, aes(x = age, group = natura2000, y = Freq, fill = natura2000)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", aes(fill = natura2000)) +
    scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
    theme_bw() +
    ylab("Percentage of awarness of Natura 2000 sites\n") +                             
    xlab("Age")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

### Natura 2000 with study level

finaldata_natura_studylevel <- ddply(finaldata,.(study_level), 
                                       function(x) with(x,
                                                        data.frame(100*round(table(natura2000)/length(natura2000),2))))


(bar_plot_natura_studylevel <- ggplot(finaldata_natura_studylevel, aes(x = study_level, group = natura2000, y = Freq, fill = natura2000)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", aes(fill = natura2000)) +
    scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
    theme_bw() +
    ylab("Percentage of awarness of Natura 2000 sites\n") +                             
    xlab("Study level")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)


