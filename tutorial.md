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
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3"))
  )
```
![image](https://user-images.githubusercontent.com/91271151/144751862-1abb97ce-edd8-46fb-a1af-ecc9e49732ba.png)
However, there is limitations with gg_mosaic. Mosaic plots idealy are able to show more catagorical variables than this for example by the use of transparency. However when using this in gg_mosaic it causes complications. Try showing who was the overall winner in each group using the alpha argument in the geom_mosaic aesthetic. 

```
(mosaic_plot_alpha <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic, alpha = Winner,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Income Demographic", x="Voted for: State", title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3"))
)
```
![image](https://user-images.githubusercontent.com/91271151/144751787-dc9c6cc9-1479-49b3-ac19-d71e05f89423.png)

We now have a plot which displays four catagorical variables clearly. However, a complication with gg_mosaic is that it is very hard to alter axis labels. This has made our axis a little complex and comprimised the clarity of out plot. 

Now, if we wanted we could take this plot a step further and display a fifth categorical variable using facet_wrap(). When we combine the mosaic plot with facet technique we are able to display 5 categorical variables clearly. We will repeat the previous code and add in another varible to display- region. We will group states into regions as an example. In a more complete data set this grouping would be already done and more completely. 

```
# Repetition of the previous section but to allow for facetting according to region


# Filter data set for only wage-bracket demographics
facet_data <- exit_poll %>% filter(Demographic == "Under $25,000"  |
                               Demographic == "$25,000 - $49,999" |
                               Demographic == "$50,000 - $74,999" |
                               Demographic == "$75,000 - $99,999" | 
                               Demographic == "$100,000+")


# Filter deep south states
deep_south <- facet_data %>% filter(State == "south-carolina" |
                                     State == "mississippi" |
                                     State == "florida" |
                                     State == "texas" |
                                     State == "alabama" )

# Filter north east states 
north_east <- facet_data %>% filter(State == "connecticut" |
                                     State == "new-hampshire" |
                                     State == "new-jersey" |
                                     State == "new-york" |
                                     State == "pennsylvania")


# Add region column 
north_east$region <- "North East"
deep_south$region <- "Deep South"

# Combine the two data sets 
facet_data <- rbind(north_east, deep_south)




# Convert rows to count data for the percentage who voted for biden and the
# percentage who voted for trump. 

# Create two new data frames in which to expand the rows, also deletes column 
# where data was extracted from
expanded_biden_facet <- expandRows(facet_data, "Biden_%")
expanded_trump_facet <- expandRows(facet_data, "Trump_%")

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

(mosaic_plot_alpha <- ggplot(data = combined_facet) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, colour = Demographic, alpha = Winner,), offset = 0.05) +
    scale_alpha_manual(values =c(.5,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="Income Demographic", x="Voted for: State", title = "Exit Poll") +
    scale_fill_manual(values = c("Trump" = "firebrick3", "Biden" = "deepskyblue3")) +
    facet_wrap(~region, nrow = 1)
)
```

![image](https://user-images.githubusercontent.com/91271151/144754885-d0d3c39b-545b-4f61-867a-3620304400dc.png)
