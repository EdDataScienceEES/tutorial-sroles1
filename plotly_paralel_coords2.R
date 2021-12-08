



iris<-iris

# Load libraries
rm(list=ls()) # clear environment

library(GGally)

# Read in agricultural data
agri_data<- read.csv("data/us_agri.csv")

# Plot

(parcoord_plot <- ggparcoord(agri_data,
           columns = 1:4, groupColumn = 5,
           scale = "center",
           title = "Relative Price Per Unit Variable by Farm",
           alphaLines = 0.6))

library(plotly)
class(us_agri$size)
library(tidyverse)
fig <- us_agri %>%
  plot_ly(type = 'parcoords', line = list(color = ~size,
                                          colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
                                dimensions = list(
                        list(range = c(0,4),
                             label = 'capital', values = ~p.capital),
                        list(range = c(0,4),
                             constraintrange = c(0,4),
                             label = 'land', values = ~p.land),
                        list(range = c(0,4),
                             label = 'labor', values = ~p.labor),
                        list(range = c(0,4),
                             label = 'crop', values = ~p.crop)
                      )
          )

fig
