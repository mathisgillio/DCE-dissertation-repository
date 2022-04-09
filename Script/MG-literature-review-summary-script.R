## Summary table for literature review: attribute selection

# Load libraries: 

library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library (RColorBrewer) 

# Load the data 

litdata <- read_csv("Data/literature-review-summary-table.csv")

display.brewer.all (colorblindFriendly = T)
# Modify the data 

litdata <- litdata  %>% 
  dplyr::rename("attribute" = 'Potential attributes'
                , "times" = 'Number of time found in the literature review')

litdata$times <- as.numeric(as.character(litdata$times))

str(litdata)

theme_diss <- function(){            # creating a new theme function
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "plain"),
        axis.title = element_text(size = 25,
                                  face = "bold"),
        axis.text.x = element_text(size = 20,
                                   vjust = 1,
                                   hjust = 1, face = "bold"), 
        axis.text.y = element_text(size = 22, face = "bold"),  # define font,
        # font sizes,
        # text angle and
        # alignment
        legend.position = "none",  # remove legend
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # create plot
        # margins
        panel.grid = element_blank())
}  


(lit_barplot <- ggplot(litdata,
                                 aes(x = reorder(attribute, times),
                                     y = times)) +
    geom_bar(position = position_dodge(), # create a barplot
             stat = "identity",
             colour = "black",
             aes(fill = times)) +  # fill the bars according to
    # percentage change value
    scale_fill_distiller(palette = "GnBu", direction = 1) +   # apply a colour
    # gradient
    labs(
         x = "\nPotential attributes", 
         y = "\nNumber of time the attribute was found in the literature review\n\n") + # add title,
    # subtitle and
    # axis labels
    geom_text(aes(label = times), hjust = -0.5, size = 7) +
    coord_flip(clip = "off") +
    theme_diss()) # apply the theme that we created earlier 

save_plot <- function(plot_name, # first put the plot object name
                      file_name = "plot",  #give it a title 
                      width = 20, # set the width, heigh and dpi
                      height = 14, 
                      dpi = 150) {
  
  ggsave(
    paste0(file_name, ".png"), plot_name, width = width,  # save as png
    height = height, dpi = dpi) 
  
  ggsave(
    paste0(file_name, ".pdf"), plot_name, width = width, # save as pdf
    height = height, dpi = dpi
  )
}

save_plot(lit_barplot, file_name = "Figures/lit-review-plot")
