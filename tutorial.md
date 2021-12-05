# Visualising Highly Dimentional Data
## 

High dimentional data refers to data frames which contain many variables and or levels. They are difficult to plot as only so much infomation can exist on a single figure. Here we will walk through several approaches to displaying multiple varibles whilst remaining clear and manageable.


Facetting creates mutiple plots based on another variable 

Import the car_thefts data set

Catagorical variables with many levels are hard to plot. This is because they fundimentally take up a lot of space. One way to tackle this is to create manageable groups to reduce the number of levels. 
*make, model, 


When collecting data in a format such as survey it can become highly dimentional. There can be many catagorical columns with many repeating rows (count data). One way to visualise this is through the use of mosaic plots. 

Import exit_poll data set 

```
# clear environment
rm(list=ls()) 

# libraries
library(tidyverse)
library(readxl)

# load exit poll data
exit_poll <- read_excel("data/2020_ap_exit_polls_combined_1.xlsx")

# see all the columns and their unique levels 
ulist <- lapply(exit_poll, unique) 
ulist
```
This data set contains 28 demographics and 50 states Mosaic plots work well for displaying multiple catagorical variables but can't handle this many levels and remain clear. In this example we will choose our demogrpahic to focus on to be wage-brackets and will choose the four states with the most electoral power. 

```
# Filter data set for only wage-bracket demographics
filt <- exit_poll %>% filter(Demographic == "Under $25,000"  |
                          Demographic == "$25,000 - $49,999" |
                          Demographic == "$50,000 - $74,999" |
                          Demographic == "$100,000+")

# Find out which states have most electoral power
state_value <- unique(exit_poll[c( "Electoral_Votes_Available","State_Abbr")])
state_value <- state_value[order(state_value$Electoral_Votes_Available, decreasing = TRUE), ]

# Filter data set for top four states
filt <- filt %>% filter(State_Abbr == "CA" |
                             State_Abbr == "TX" |
                            State_Abbr == "FL" |
                           State_Abbr == "NY")
```
Our data frame originates from an exit poll survey taken nationwide. However, some 'wrangling' has already occured and what we are presented with is a summary data frame. Mosaic plots are best for displaying the raw data. If we had collected with data ourselves, using a mosaic plot would summarise our data us without the need for extra rangling. We will take a backwards step to return the data to a form representative of the raw form to allow the mosaic plot to deal with the count data it needs. 

```
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
expanded2$voted_for <- "Trump"
expanded$voted_for <- "Biden"

# Remove names of column which has been deleted from the other data set in order
# to make them match, ready to combine
expanded_biden <- expanded_biden %>% select(-"Trump_%")
expanded_trump <- expanded_trump %>% select(-"Biden_%")

# Combine the two data sets 
combined <- rbind(expanded_biden,expanded_trump)
```

Now we are going to plot our data. This plot will show us infomation on demographic and how this varies by state, and what proportion voted for who. 
```
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
    labs(y="Income Demographic", x="Voted for: State", title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3")) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
  )
```

![image](https://user-images.githubusercontent.com/91271151/144756569-1479db89-9eab-4a71-88fa-0900aa2c5a9c.png)

However, there is limitations with gg_mosaic. Mosaic plots idealy are able to show more catagorical variables than this for example by the use of transparency. However when using this in gg_mosaic it causes complications. Try showing who was the overall winner in each group using the alpha argument in the geom_mosaic aesthetic. 

```
# Plot mosaic figure showing proportion of demographic to total population in 
# each state, and how they voted. 
(mosaic_plot_alpha <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic, alpha = Winner,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Income Demographic", x="Voted for: State", title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3"))+
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor = element_blank(), 
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
)
```
![image](https://user-images.githubusercontent.com/91271151/144756533-49af96f8-e0f5-4635-8a9f-07d6ef38b6d8.png)

We now have a plot which displays four catagorical variables. However, a complication with gg_mosaic is that it is very hard to alter axis labels. This is due to the fact that although we know that variables are discrete, in gg_mosaic they are continuous as this is how the proportionate sizes of boxes based on count data, is in a mosaic plot are generated. This means it is not possible to customise axis, in an intuitive way. This has made our axis a little complex and comprimised the clarity of out plot, as is seen on the x-axis where there are redundant labels. In the next plot we will show one approach to tackling this problem.

Now, if we wanted we could take this plot a step further and display a fifth categorical variable- region. We cannot use facet_wrap() because in this case we want our x-axis to be diferent according to which states are found in a region. If we had equivilent data for the 2016 exit poll we could facet wrap by year. However, we will instead create two seperate plots based on region and combine them using grid.arrange(). We will repeat the previous code and create then add in another varible (region) to display.
```

# Repetition of the previous section but to allow for the creation of two comparable plots according to region 


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
expanded_biden_facet <- expandRows(region_data, "Biden_%")
expanded_trump_facet <- expandRows(region_data, "Trump_%")

# Expand rows to show the proportion of the total population each demographic 
# makes up
expanded_biden_facet <- expandRows(expanded_biden_facet, "proportion")
expanded_trump_facet <- expandRows(expanded_trump_facet, "proportion")

# Add new column name to distinguish Trump voters from Biden voters
expanded_trump_facet$voted_for <- "Trump"
expanded_biden_facet$voted_for <- "Biden"

# Remove names of column which has been deleted from the other data set in order
# to make them match, ready to combine
expanded_biden_facet <- expanded_biden_facet %>% select(-"Trump_%")
expanded_trump_facet <- expanded_trump_facet %>% select(-"Biden_%")

# Combine the two data sets 
combined_facet <- rbind(expanded_biden_facet,expanded_trump_facet)

deep_south_data <- subset(combined_facet, region == "Deep South")
north_east_data <- subset(combined_facet, region == "North East")

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
grid.arrange(mosaic_plot_ds, mosaic_plot_ne, ncol=2, bottom = "States")
```

![image](https://user-images.githubusercontent.com/91271151/144762600-ecdd201d-9008-4475-a16a-a1fb462d7dca.png)



