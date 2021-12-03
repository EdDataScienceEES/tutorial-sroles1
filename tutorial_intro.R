rm(list=ls()) # clear environment
library(readxl)
library(tidyverse)
thefts <- read_excel("data/2015_State_Top10Report_wTotalThefts.xlsx") # read in thefts data

# view unique values in each column 
ulst <- lapply(thefts, unique)
ulst
thefts$rank <- as.factor(thefts$rank)
class(thefts$rank)
# We can see that the make_and_model column is providing us with two bits of infomation which
# could be displayed clearer. So, we seperate the columns. 

# create a make column
thefts$make <- ifelse(grepl("Toy", thefts$make_and_model, ignore.case = T), "Toyota", 
               ifelse(grepl("Chev", thefts$make_and_model, ignore.case = T), "Chevrolet",
               ifelse(grepl("Ford", thefts$make_and_model, ignore.case = T), "Ford",
               ifelse(grepl("GMC", thefts$make_and_model, ignore.case = T), "GMC",
               ifelse(grepl("Honda", thefts$make_and_model, ignore.case = T), "Honda",
               ifelse(grepl("Jeep", thefts$make_and_model, ignore.case = T), "Jeep",
               ifelse(grepl("Nissan", thefts$make_and_model, ignore.case = T), "Nissan",
               ifelse(grepl("Acura", thefts$make_and_model, ignore.case = T), "Acura",
               ifelse(grepl("Pontiac ", thefts$make_and_model, ignore.case = T), "Pontiac ",
               ifelse(grepl("Hyundai ", thefts$make_and_model, ignore.case = T), "Hyundai ",
               ifelse(grepl("Subaru ", thefts$make_and_model, ignore.case = T), "Subaru ",
               ifelse(grepl("Chrysler", thefts$make_and_model, ignore.case = T), "Chrysler  ",
               ifelse(grepl("Dodge", thefts$make_and_model, ignore.case = T), "Dodge", "Other")))))))))))))


# create model column 
library(stringr)

thefts <- thefts %>% 
  rename( model = make_and_model)

thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Toyota", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Chevrolet", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Ford", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "GMC", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Honda", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Jeep", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Nissan", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Acura", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Pontiac", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Hyundai", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Subaru", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Chrysler", "")
thefts <- thefts %>%
  mutate_at("make_and_model", str_replace, "Dodge", "")

thefts_plot5<-ggplot(thefts, aes(x = thefts, y = model, colour = rank)) + 
    geom_point(size = 0.5) + 
    #   scale_x_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10")) +
    theme(strip.text.y = element_text(angle = 0, size = 10),
          strip.text.x = element_text(angle = 90, size = 6))


thefts_plot6 <-thefts_plot5 + facet_grid(make ~ ., scales = "free") +
  theme(strip.text.x = element_text(angle = 90, size = 6))


library(plotly)
install.packages("gapminder")
library(gapminder)
ggplotly(thefts_plot4)


# filter only to rank 1 (the most stolen)
rank_1 <- thefts %>% filter(rank == 1)

# group years 
thefts$year_groups<- ifelse(thefts$year<2020& thefts$year>=2010, "2011-16",
                        ifelse(thefts$year<2010&thefts$year>=2000, "2000-10",
                             #  ifelse(thefts$year<2005&thefts$year>=2000, "20s",
                                # ifelse(thefts$year<2000&thefts$year>=1995, "d",
                                             ifelse(thefts$year<2000&thefts$year>=1989, "1989-99", "f")))

                                                                 
thefts_plot5<-ggplot(thefts, aes(x = thefts, y = model, colour = rank)) + 
  geom_point(size = 0.5) + 
  #   scale_x_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  theme(strip.text.y = element_text(angle = 0, size = 10),
        strip.text.x = element_text(angle = 90, size = 6))


thefts_plot6 <-thefts_plot5 + facet_grid(make ~ ., scales = "free") +
  theme(strip.text.x = element_text(angle = 90, size = 6))



