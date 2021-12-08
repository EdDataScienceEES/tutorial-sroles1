rm(list=ls()) # clear environment
library(readxl)
library(tidyverse)
thefts <- read_excel("data/2015_State_Top10Report_wTotalThefts.xlsx") # read in thefts data

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

thefts <- thefts %>% 
  rename( model = make_and_model)

library(plotly)







library(RColorBrewer)
plotly_fig <- plot_ly(data = thefts, x = ~thefts, y = ~year, type = 'scatter',
                      color = ~state,
                      colors = brewer.pal(length(names(table(state))),
                                                          "Paired")) %>% 
  layout(plotly_fig, xaxis = list(type = "log"),
         yaxis = list(autorange = "reversed"))


plotly_fig

