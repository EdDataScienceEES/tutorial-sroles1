rm(list=ls()) # clear environment

# libraries
library(readxl) 
library(tidyverse)
library(reshape2)

theft_data <- read_excel("data/2015_State_Top10Report_wTotalThefts.xlsx") # read in car thefts data

# view unique values in each column 
ulst <- lapply(theft_data, unique)
ulst


# We can see that the make_and_model column is providing us with two bits of infomation which
# could be displayed clearer. So, we seperate the columns.

# Create new data frame with make and model extracted and seperated
make_model_sep <- colsplit(theft_data$make_and_model," ",c("make","model"))
# Bind data frames back together
theft_data <- cbind(make_model_sep, theft_data)
# Remove original make_and_model column
theft_data <- select(theft_data, -make_and_model)


# Plot car thefts facetted by make

# Order from highest thefts to lowest, between models and in overall facet plot
theft_data$model <- reorder(theft_data$model, theft_data$thefts)
theft_data$make <- reorder(theft_data$make, -theft_data$thefts)
# plot 

(theft_facet <- ggplot(theft_data, aes(x = thefts, y = model, colour = make )) + 
    geom_point() +
    ggtitle("2015 US Car Thefts") + 
    scale_x_continuous(trans = "log10", name = "Number of Thefts") +
    scale_y_discrete(name = "Model") +
    facet_grid(make ~ ., scales = "free", space = "free") +
    theme_light() + theme(text=element_text(size=7, angle=12),
                          strip.text.y = element_text(angle = 0, size = 8),
                          legend.position="none", title = element_text(angle = 0, size =10), 
                          plot.title = element_text(hjust = 0.5))
) 


# Load library 
library(plotly)

# Plot thefts against year, coloured by make
(make_plot <- theft_data %>% 
    ggplot(aes(label = model, label_2 = rank, label_3 = state)) +
    geom_point(aes(x = thefts, y = year, colour = make)) +
    scale_x_continuous(trans = "log10") +
    scale_y_reverse()
) 
# Convert to ggplotly
ggplotly(make_plot)

# Plot thefts against year, coloured by state
(state_plot <- theft_data %>% 
    ggplot(aes(label = model, label_2 = rank, label_3 = state)) +
    geom_point(aes(x = thefts, y = year, colour = state)) +
    scale_x_continuous(trans = "log10") +
     scale_y_reverse()
)

# Convert to ggplotly
ggplotly(state_plot)

library(htmlwidgets)

saveWidget(state_plot, "graph.html")


htmltools::save_html(
  html = htmltools::as.tags(
    x = plotly::toWebGL(state_plot),
    standalone = TRUE),
  file = "test.html")
