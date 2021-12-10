# Visualising Multi-dimensional Data Tutorial Starter Scipt ---- 

# Mosaic plots: Multiple catagorical variables ----
# clear environment
rm(list=ls()) 

# libraries
library(tidyverse)
library(readxl)

# load exit poll data
exit_poll <- read_excel("data/2020_ap_exit_polls_combined_2.xlsx")

# see all the columns and their unique levels 
ulist <- lapply(exit_poll, unique) 
ulist

# Filter data set for only wage-bracket demographics
filt <- exit_poll %>% filter(Demographic == "Under $25,000"  |
                               Demographic == "$25,000 - $49,999" |
                               Demographic == "$50,000 - $74,999" |
                               Demographic == "$100,000+")

# Find out which states have most electoral power
state_value <- unique(exit_poll[c( "Electoral_Votes_Available",
                                   "State_Abbr")])
state_value <- state_value[order(state_value$Electoral_Votes_Available,
                                 decreasing = TRUE), ]

state_value # print states in order of the most electoral power

# Filter data set for top four states
filt <- filt %>% filter(State_Abbr == "CA" |
                          State_Abbr == "TX" |
                          State_Abbr == "FL" |
                          State_Abbr == "NY")

# library 
library(splitstackshape)

# Convert rows to count data for the percentage who voted for biden and
# the percentage who voted for trump. 

# Create two new data frames in which to expand the rows and delete 
# column where data was extracted from.
expanded_biden <- expandRows(filt, "Biden_%")
expanded_trump <- expandRows(filt, "Trump_%")

# Expand rows to show the proportion of the total population each
# demographic makes up.
expanded_biden <- expandRows(expanded_biden, "proportion")
expanded_trump <- expandRows(expanded_trump, "proportion")

# Add new column name to distinguish Trump voters from Biden voters
expanded_trump$voted_for <- "Trump"
expanded_biden$voted_for <- "Biden"

# Remove names of column which has been deleted from the other data set
# in order to make them match, ready to combine
expanded_biden <- expanded_biden %>% select(-"Trump_%")
expanded_trump <- expanded_trump %>% select(-"Biden_%")

# Combine the two data sets 
combined <- rbind(expanded_biden,expanded_trump)

glimpse(combined) # See how the structure and size of our data has
# changed

# Library 
library(ggmosaic)

# Reorder demographics into ascending order 
combined$Demographic <- factor(combined$Demographic,
                               levels=c("Under $25,000", "$25,000 - $49,999", "$50,000 - $74,999",
                                        "$75,000 - $99,999", "$100,000+"))

# Plot mosaic figure 
(mosaic_plot <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic),
                offset = 0.05) + 
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1, vjust = .5)) + 
    labs(y="Income Demographic", x="Voted for: State",
         title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3",
                                 "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
)

# Plot mosaic figure
(mosaic_plot_alpha <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic,
                    alpha = winner_amongst_group,),
                offset = 0.05) + # Alpha argument included 
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Income Demographic", x="Voted for: State",
         title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3",
                                 "Biden" = "deepskyblue3"))+
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(), 
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
)

# Repetition of the previous section but to allow for facetting
# according to region.

# Library
library(gridExtra)

# Filter data set for only wage-bracket demographics
region_data <- exit_poll %>% filter(Demographic == "Under $25,000"  |
                                      Demographic == "$25,000 - $49,999" |
                                      Demographic == "$50,000 - $74,999" |
                                      Demographic == "$75,000 - $99,999" | 
                                      Demographic == "$100,000+")


# Filter deep south states
deep_south <- region_data %>% filter(State == "south-carolina" |
                                       State == "mississippi" |
                                       State == "florida" |
                                       State == "texas" |
                                       State == "alabama" )

# Filter north east states 
north_east <- region_data %>% filter(State == "connecticut" |
                                       State == "new-hampshire" |
                                       State == "new-jersey" |
                                       State == "new-york" |
                                       State == "pennsylvania")


# Add region column 
north_east$region <- "North East"
deep_south$region <- "Deep South"

# Combine the two data sets 
region_data <- rbind(north_east, deep_south)


# Convert rows to count data for the percentage who voted for biden and
# the percentage who voted for trump. 

# Create two new data frames in which to expand the rows, also deletes
# column where data was extracted from.
expanded_biden_region<- expandRows(region_data, "Biden_%")
expanded_trump_region <- expandRows(region_data, "Trump_%")

# Expand rows to show the proportion of the total population each
# demographic makes up
expanded_biden_region <- expandRows(expanded_biden_region, "proportion")
expanded_trump_region <- expandRows(expanded_trump_region, "proportion")

# Add new column name to distinguish Trump voters from Biden voters
expanded_trump_region$voted_for <- "Trump"
expanded_biden_region$voted_for <- "Biden"

# Remove names of column which has been deleted from the other data set
# in order to make them match, ready to combine
expanded_biden_region <- expanded_biden_region %>% select(-"Trump_%")
expanded_trump_region <- expanded_trump_region %>% select(-"Biden_%")

# Combine the two data sets 
combined_region <- rbind(expanded_biden_region,expanded_trump_region)

deep_south_data <- subset(combined_region, region == "Deep South")
north_east_data <- subset(combined_region, region == "North East")

# North East plot 
(mosaic_plot_ne <-north_east_data %>%
    arrange(Demographic) %>%    
    mutate(Demographic=factor(Demographic, levels=c("Under $25,000",
                                                    "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999",
                                                    "$100,000+"))) %>%
    ggplot() +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic,
                    alpha = winner_amongst_group,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                     vjust = .5)) + 
    labs(x="CT        NH       NJ       NY       PA"      ,
         title = "North East") +
    scale_fill_manual(values = c("Trump" = "firebrick3",
                                 "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank())
)

# Deep South plot
(mosaic_plot_ds <- deep_south_data %>%
    arrange(Demographic) %>%    
    mutate(Demographic=factor(Demographic, levels=c("Under $25,000",
                                                    "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999",
                                                    "$100,000+"))) %>% 
    ggplot() +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic,
                    alpha = winner_amongst_group,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                     vjust = .5)) + 
    labs(y="Votes for:Income Demographic",
         x = "AL        FL         MS         SC         TX",
         title = "Deep South") +
    scale_fill_manual(values = c("Trump" = "firebrick3",
                                 "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank(),
                       axis.text.x=element_blank(), 
                       axis.ticks.x=element_blank(), 
                       legend.position = "none")
  )


# Arrange both plots together to give the appearance of a facet 
grid.arrange(mosaic_plot_ds, mosaic_plot_ne, ncol=2, bottom = "States")

# Parallel coordinate plots: Multiple continuous variable ----

# Clear environment
rm(list=ls()) 

# Load libraries
library(GGally)

# Read in agricultural data
agri_data<- read.csv("data/us_agri.csv")

# Investigate data frame structure and class of each variable
str(agri_data)

# Plot parallel coordinates plot
(parcoord_plot <- ggparcoord(agri_data,
                             columns = 1:4, groupColumn = 5, # Variable columns on x axis
                             # and which to group lines by
                             scale = "center", # Standardise and center variables
                             title = "Relative Price Per Unit Variable by Farm", # Title
                             alphaLines = 0.6) + # Opacity of lines 
    theme_bw())

# Load libraries
library(plotly)
library(tidyverse)

# Add column giving 'size' numerical values for parcoords plot
agri_data$size_plotly <- ifelse(agri_data$size == "Large", 1,
                                ifelse(agri_data$size == "Small", 0, 0.5))

head(agri_data) # See the new column we have added 

# Plot  a plotly parallel coordinate plot
plotly_parcoord <- agri_data %>%
  plot_ly(type = 'parcoords', line = list(color = ~size_plotly,
                                          # Distinguish sizes into three colours
                                          colorscale = list(c(0,'blue'),
                                                            c(0.5,'green'),c(1,'red'))),
          dimensions = list( # Define the scale
            # ranges of each variable  
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

# Faceted plots: A Mixture of catagorical and continuous ----

# clear environment
rm(list=ls())

# libraries
library(readxl) 
library(tidyverse)
library(reshape2)

# read in car thefts data
theft_data <- read_excel("data/2015_State_Top10Report_wTotalThefts.xlsx")

# view unique values in each column 
ulst <- lapply(theft_data, unique)
ulst

# Create new data frame with make and model extracted and seperated
make_model_sep <- colsplit(theft_data$make_and_model," ",c("make",
                                                           "model"))
# Bind data frames back together
theft_data <- cbind(make_model_sep, theft_data)
# Remove original make_and_model column
theft_data <- select(theft_data, -make_and_model)

# Plot car thefts facetted by make

# Order from highest thefts to lowest, between models and in overall
# facet plot
theft_data$model <- reorder(theft_data$model, theft_data$thefts)
theft_data$make <- reorder(theft_data$make, -theft_data$thefts)

# plot 
(theft_facet <- ggplot(theft_data, aes(x = thefts, y = model,
                                       colour = make )) + 
    geom_point() +
    ggtitle("2015 US Car Thefts") + 
    scale_x_continuous(trans = "log10", name = "Number of Thefts") +
    scale_y_discrete(name = "Model") +
    facet_grid(make ~ ., scales = "free", space = "free") +
    theme_light() + theme(text=element_text(size=7, angle=12),
                          strip.text.y = element_text(angle = 0,
                                                      size = 8),
                          legend.position="right",
                          title = element_text(angle = 0, size =10), 
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
