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
                          Demographic == "$75,000 - $99,999" | 
                          Demographic == "$100,000+")

# Find out which states have most electoral power
state_value <- unique(exit_poll[c( "Electoral_Votes_Available","State_Abbr")])
state_value <- state_value[order(state_value$Electoral_Votes_Available, decreasing = TRUE), ]

# Filter data for top four states
filt <- filt %>% filter(State_Abbr == "CA" |
                             State_Abbr == "TX" |
                            State_Abbr == "FL" |
                           State_Abbr == "NY")


#install.packages("splitstackshape")

# library 
library(splitstackshape)

# Convert rows to count data for the percentage who voted for biden and the
# percentage who voted for trump. 

# Create two new data frames in which to expand the rows, also deletes column 
# where data was extracted from
expanded_biden <- expandRows(filt, "Biden_%")
expanded_trump <- expandRows(filt, "Trump_%")

# Expand rows to show the proportion of the total population each demographic 
# makes up
expanded_biden <- expandRows(expanded_biden, "proportion")
expanded_trump <- expandRows(expanded_trump, "proportion")

# Add new column name to distinguish Trump voters from Biden voters
expanded_trump$voted_for <- "Trump"
expanded_biden$voted_for <- "Biden"

# Remove names of column which has been deleted from the other data set in order
# to make them match, ready to combine
expanded_biden <- expanded_biden %>% select(-"Trump_%")
expanded_trump <- expanded_trump %>% select(-"Biden_%")

# Combine the two data sets 
combined <- rbind(expanded_biden,expanded_trump)







# Library 
library(ggmosaic)

# Reorder demographics into ascending order
combined$Demographic <- factor(combined$Demographic, levels=c("Under $25,000", "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "$100,000+"))

# Plot mosaic figure showing proportion of demographic to total population in 
# each state, and how they voted. 
(mosaic_plot <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic), offset = 0.05) + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Votes for:Income Demographic", x="Voted for: State", title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
  )


#combined$voted_for <- factor(combined$voted_for, levels=c("", "","","","", "","","" ))
 
# Plot mosaic figure showing proportion of demographic to total population in 
# each state, and how they voted. 
(mosaic_plot_alpha <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, alpha = winner_amongst_group,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Votes for:Income Demographic", x="Voted for: State", title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3"))+
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
)


# Repetition of the previous section but to allow for facetting according to region ---- 


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


# Convert rows to count data for the percentage who voted for biden and the
# percentage who voted for trump. 

# Create two new data frames in which to expand the rows, also deletes column 
# where data was extracted from
expanded_biden_region<- expandRows(region_data, "Biden_%")
expanded_trump_region <- expandRows(region_data, "Trump_%")

# Expand rows to show the proportion of the total population each demographic 
# makes up
expanded_biden_region <- expandRows(expanded_biden_region, "proportion")
expanded_trump_region <- expandRows(expanded_trump_region, "proportion")

# Add new column name to distinguish Trump voters from Biden voters
expanded_trump_region$voted_for <- "Trump"
expanded_biden_region$voted_for <- "Biden"

# Remove names of column which has been deleted from the other data set in order
# to make them match, ready to combine
expanded_biden_region <- expanded_biden_region %>% select(-"Trump_%")
expanded_trump_region <- expanded_trump_region %>% select(-"Biden_%")

# Combine the two data sets 
combined_region <- rbind(expanded_biden_region,expanded_trump_region)

deep_south_data <- subset(combined_region, region == "Deep South")
north_east_data <- subset(combined_region, region == "North East")

# North East plot 
(mosaic_plot_ne <-north_east_data %>%
    arrange(Demographic) %>%    
    mutate(Demographic=factor(Demographic, levels=c("Under $25,000", "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "$100,000+"))) %>%
    ggplot() +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic, alpha = winner_amongst_group,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(x="CT        NH       NJ       NY       PA"      , title = "North East") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank(),
                    #   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank())
)

# Deep South plot
(mosaic_plot_ds <- deep_south_data %>%
    arrange(Demographic) %>%    
    mutate(Demographic=factor(Demographic, levels=c("Under $25,000", "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "$100,000+"))) %>% 
    ggplot() +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic, alpha = winner_amongst_group,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Votes for:Income Demographic", x="AL        FL         MS         SC         TX"      , title = "Deep South") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank(),
                       #   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),
                       legend.position = "none")
)

# Arrange both plots together to give the appearance of a facet 
grid.arrange(mosaic_plot_ds, mosaic_plot_ne, ncol=2, bottom = "States /n/ fig cap")





# test ----

(mosaic_plot <- ggplot(data = combined_region) +
   geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                   fill = voted_for, colour = Demographic), offset = 0.05) +
   labs(y="Votes for:Income Demographic", x="Voted for: State", title = "Exit Poll") +
   scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3")) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
         panel.border = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.title = element_text(hjust = 0.5),
                      axis.line = element_blank())+
   facet_wrap("region")
)
