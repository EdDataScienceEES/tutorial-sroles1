# Clear environment
rm(list=ls()) 

# Load libraries
library(GGally)

# Read in agricultural data
agri_data<- read.csv("data/us_agri.csv")

# Plot parallel coordinates plot
(parcoord_plot <- ggparcoord(agri_data,
           columns = 1:4, groupColumn = 6, # Variable columns on x axis and which to group lines by
           scale = "center", # Standardise and center variables
           title = "Relative Price Per Unit Variable by Farm", # Title
           alphaLines = 0.6)) # Opacity of lines

# Load libraries
library(plotly)
library(tidyverse)

# Plot  a plotly parallel coordinate plot
plotly_parcoord <- us_agri %>%
  plot_ly(type = 'parcoords', line = list(color = ~size_plotly, # Distinguish sizes into three colours
                                          colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
                                dimensions = list( # Define the scale ranges of each variable  
                        list(range = c(0,1.5),
                             label = '          Size: Large \r\n       
                    Medium \r\n
                 Small', values = ~size_plotly), # A make-shift legend         
                        list(range = c(0,4),
                             label = 'capital', values = ~p.capital),
                        list(range = c(0,4),
                             constraintrange = c(0,4),
                             label = 'land', values = ~p.land),
                        list(range = c(0,4),
                             label = 'labor', values = ~p.labor),
                        list(range = c(0,4),
                             label = 'crop', values = ~p.crop))
          )

plotly_parcoord # Call plot

