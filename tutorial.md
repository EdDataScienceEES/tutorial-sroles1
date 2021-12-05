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
This data set contains 28 demographics and 50 states Mosaic plots work well for displaying multiple catagorical variables but can't handle this many levels and remain clear. In this example we will choose our demogrpahic to focus on to be wage-brackets

```
filt <- exit_poll %>% filter(Demographic == "Under $25,000"  |
                          Demographic == "$25,000 - $49,999" |
                          Demographic == "$50,000 - $74,999" |
                          Demographic == "$100,000+")
```


