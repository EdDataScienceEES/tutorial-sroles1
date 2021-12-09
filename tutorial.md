# Visualising Highly Dimentional Data
## 

High dimentional data refers to data frames which contain many variables and or levels. They are difficult to plot as only so much infomation can exist on a single figure. Here we will walk through several approaches to displaying multiple varibles whilst remaining clear and manageable.

## Learning Objectives:


First, download the repository locally through this link:

# Parallel Coordinate Plots

Parallel coordinates for multidimensional exploratory data analysis.

```
# Clear environment
rm(list=ls()) 

# Load libraries
library(GGally)

# Read in agricultural data
agri_data<- read.csv("data/us_agri.csv")

# Investigate data frame structure
str(agri_data)
```
We can see that we have 4 continuous variables and a catagorical variable which groups the rows into size of farm. Now we are going to plot parallel coordinates plot to display all of the varibles.
```
# Plot parallel coordinates plot
(parcoord_plot <- ggparcoord(agri_data,
           columns = 1:4, groupColumn = 5, # Variable columns on x axis and which to group lines by
           scale = "center", # Standardise and center variables
           title = "Relative Price Per Unit Variable by Farm", # Title
           alphaLines = 0.6)) # Opacity of lines
```

![image](https://user-images.githubusercontent.com/91271151/145380394-525b9c40-0414-43ce-bb67-9099686174c3.png)


This plot is not useful for extracting specific values but is for viewing overall trends in the data. A question which may be posed by looking at this graph is why do small farms own land which is valued higher than larger farms? 
However, this style of plot can become confusing and unclear when more rows of data are added. With plotly we can plot the same style of graph but allow for viewer interaction to make the trends and clear isolate parameters of interest. 

Plotly parellel coordinate plots are not perfect however. They struggle with customisation of labels, legends and catagorical data. We have implemented two 'work-arounds' in this code to overcome some of these problems. The first requires us to assign each level of the 'size' catagory a numerical value.
```
# Load libraries
library(plotly)
library(tidyverse)

# Add column giving 'size' numerical values for parcoords plot
agri_data$size_plotly <- ifelse(agri_data$size == "Large", 1,
                              ifelse(agri_data$size == "Small", 0, 0.5))

```

We will now plot the parallel coordinate graph. The numerical size column is a work-around to assign each level a colour. It also allows us to add a make-shift legend and seperate the levels. We add size as an extra variable and use our assinged numerical values to distinguish them. We will use the label of this column to distingush each level.
```
# Plot  a plotly parallel coordinate plot
plotly_parcoord <- agri_data %>%
  plot_ly(type = 'parcoords', line = list(color = ~size_plotly, # Distinguish sizes into three colours
                                          colorscale = list(c(0,'blue'),c(0.5,'green'),c(1,'red'))),
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
```


### Mosaic Plots

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

We now have a plot which displays four catagorical variables. However, a complication with gg_mosaic is that it is very hard to alter axis labels. This is due to the fact that although we know that variables are discrete, in gg_mosaic they are continuous as this is how the proportionate sizes of boxes based on the count data are generated. This means it is not possible to customise axis, in an intuitive way. This has made our axis a little complex and comprimised the clarity of out plot, as is seen on the x-axis where there are redundant labels. In the next plot we will show one approach to tackling this problem.

Now, if we wanted we could take this plot a step further and display a fifth categorical variable- region. We cannot use facet_wrap() because in this case we want our x-axis to be diferent according to which states are found in a region. If we had equivilent data for the 2016 exit poll we could facet wrap by year. However, we will instead create two seperate plots based on region and combine them using grid.arrange(). We will repeat the previous code and create then add in another varible (region) to display. We remove several aspects of the graphs in order to combine them cleanly. The x-axis labels have been improved, see if you can spot the way we got around the dificulites with changing axis in gg_mosaic.
```
# Repetition of the previous section but to allow for facetting according to region 


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
grid.arrange(mosaic_plot_ds, mosaic_plot_ne, ncol=2, bottom = "States")
```

![image](https://user-images.githubusercontent.com/91271151/144762600-ecdd201d-9008-4475-a16a-a1fb462d7dca.png)

We now have a plot which clearly displays 5 catagorical variables. Ideally this would contain a figure caption, perhaps giving the full names of each state instead of just the abbreviations. We could do this when knitting into a PDF for example using fig.cap = "". 

# Facet
Facetting creates mutiple plots based on a catagorical variable. 

First clear the R environment. Then load in the libraries, the car thefts data frame and explore it's columns and their levels. 
```
# clear environment
rm(list=ls())

# libraries
library(readxl) 
library(tidyverse)
library(reshape2)

theft_data <- read_excel("data/2015_State_Top10Report_wTotalThefts.xlsx") # read in car thefts data

# view unique values in each column 
ulst <- lapply(theft_data, unique)
ulst
```

We can see that the make_and_model column is providing us with two bits of infomation which could be displayed clearer. So, we seperate the columns. 
```
# Create new data frame with make and model extracted and seperated
make_model_sep <- colsplit(theft_data$make_and_model," ",c("make","model"))
# Bind data frames back together
theft_data <- cbind(make_model_sep, theft_data)
# Remove original make_and_model column
theft_data <- select(theft_data, -make_and_model)
```
Now we are going to plot our data. Our plot will display infomation about the make, model and thefts.
```
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
```

![image](https://user-images.githubusercontent.com/91271151/145218595-178543c6-770e-40a2-a361-6a398663fcef.png)

Now, using facet_wrap() we have managed to display an extra variable. However, our data frame has three more variables we might be intersting in displaying. We could do this in multiple plots but this might become confusing for our audience. We are going to use the package `plotly` to help us display extra variables and improve our ability to explore data. 

### Plotly

Plotly is a package which acts as an interface to the plotly javascript graphing library. It wraps javascript for multiple coding languages including R, Python and Matlab. It produces amazing interactive and animated graphics of browser/ html based charts and visualisations with little code. Although plotly is relively simple to use, it is made even simplier by being able to improve existing ggplot code using ggplotly().

Interactive visuals can have a particular use in displaying high density or high dimentional data. Interactive visualisations are beocming increasingly popular. This is especially within popular web based news media. 

Static vs. interactive: 
       - Static useful for reports useful for displaying what you the creator has highlighted 
       - User can update an interactive graphic e.g. drill down to specific data points using hover info or focusing on subsets of data by selecting or deselecting groups 
       - Simple interaction improve ability of data exploration

However, we must remember that interactivity alone does not make a good graphic. We must always refer to the best practices of data visualisation and design principles.

```
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
```

In these plots, all 6 of the variables are available to the viewer to find infomation about by hovering over data points. Data can be explored by selecting and deselecting data points. All items in the legend can be selected and deselected by double clickling. 




