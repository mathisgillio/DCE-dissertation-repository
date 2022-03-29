## Follow-up questions script 
## Mathis Gillio 
## Dissertation work Marsh 2022

## Load the packages ---- 

library(dplyr)
library(tidyverse)
library(ggplot2) 
library(plyr)

install.packages("tidytext")
install.packages("R.utils")
library(tidytext)
library(R.utils)
library(viridis)
library (RColorBrewer) 


devtools::install_github("kassambara/ggpubr")


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
  dplyr::rename("gender" = Quel.est.votre.genre...
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
  dplyr::rename("gender" = Quel.est.votre.genre..
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

save_plot <- function(plot_name, # first put the plot object name
                      file_name = "plot",  #give it a title 
                      width = 13, # set the width, heigh and dpi
                      height = 8, 
                      dpi = 150) {
  
  ggsave(
    paste0(file_name, ".png"), plot_name, width = width,  # save as png
    height = height, dpi = dpi) 
  
  ggsave(
    paste0(file_name, ".pdf"), plot_name, width = width, # save as pdf
    height = height, dpi = dpi
  )
}

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
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    theme(axis.text.x = element_text(size = 22, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 22),
          axis.title = element_text(size = 25, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          legend.key.size = unit(1, 'cm'), #change legend key size
          legend.text = element_text(size=20),
          panel.grid.major.y = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

save_plot(bar_plot_awarness_age, file_name = "Figures/bar-plot-awaraness-age", width = 13, 
          height = 8, dpi = 150)

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
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    theme(axis.text.x = element_text(size = 22, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 22),
          axis.title = element_text(size = 25, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          legend.key.size = unit(1, 'cm'), #change legend key size
          legend.text = element_text(size=20),
          panel.grid.major.y = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

save_plot(bar_plot_awarness_studylevel, file_name = "Figures/bar-plot-awaraness-studylevel", width = 13, 
          height = 8, dpi = 150)

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
    scale_y_continuous(breaks = seq(0, 80, by = 10)) +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          panel.grid.major.y = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

save_plot(bar_plot_natura_age, file_name = "Figures/bar-plot-nature-age", width = 13, 
          height = 8, dpi = 150)

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
    scale_y_continuous(breaks = seq(0, 80, by = 10)) +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          panel.grid.major.y = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

save_plot(bar_plot_natura_studylevel, file_name = "Figures/bar-plot-nature-studylevel", width = 13, 
          height = 8, dpi = 150)

## Access group 

finaldata_access_group <- ddply(finaldata,.(club_member), 
                              function(x) with(x,
                                               data.frame(100*round(table(access)/length(access),2))))


(bar_access_group <- ggplot(finaldata_access_group, aes(x = club_member, group = access, y = Freq, fill = access)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", aes(fill = access)) +
    scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
    theme_bw() +
    ylab("Percentage of access\n") +                             
    xlab("Club member")  +
    scale_y_continuous(breaks = seq(0, 60, by = 10)) +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.title = element_blank(),
          panel.grid.major.y = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

save_plot(bar_access_group, file_name = "Figures/bar-plot-access-group", width = 13, 
          height = 8, dpi = 150)


### Make some pie charts 

data_access <- data.frame("category" = c('No', 'Yes'),
                   "amount" = c(40, 60))

(pie_access <- ggplot(data_access, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1, aes(fill = category)) +
  scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(plot.title = element_text(size = 35, face = "bold", hjust = 0.5, vjust = -3)) +
  labs(title = "Would accept restricted access to beaches") + 
  geom_text(aes(label = paste0(amount, " %")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL))

data_natura <- data.frame("category" = c('No', 'Yes'),
                          "amount" = c(19, 81))

(pie_natura <- ggplot(data_natura, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1, aes(fill = category)) +
  scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(plot.title = element_text(size = 35, face = "bold", hjust = 0.5, vjust = -3)) +
  labs(title = "Aware of the existence of Nature 2000 sites ") + 
  geom_text(aes(label = paste0(amount, " %")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL))

data_aware <- data.frame("category" = c('No', 'Yes'),
                          "amount" = c(10, 90))

(pie_aware <- ggplot(data_aware, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1, aes(fill = category)) +
  scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(plot.title = element_text(size = 35, face = "bold", hjust = 0.5, vjust = -3)) +
  labs(title = "Aware of coastal conservation issues") + 
  geom_text(aes(label = paste0(amount, " %")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL))

data_label <- data.frame("category" = c('No', 'Yes'),
                         "amount" = c(19, 81))

(pie_label <- ggplot(data_label, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1, aes(fill = category)) +
  scale_fill_manual(values = c("#EDF8B1", "#7FCDBB")) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(plot.title = element_text(size = 35, face = "bold", hjust = 0.5, vjust = -3)) +
  labs(title = "Benefit from green beach label") + 
  geom_text(aes(label = paste0(amount, " %")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL))

arranged_graphs <- ggarrange(pie_aware, pie_natura, pie_access, pie_label,
                                    labels = c("A", "B", "C", "D"),
                                    ncol = 2, nrow = 2,
                             common.legend = TRUE, legend = "bottom")


