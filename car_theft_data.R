#intro ---- 
rm(list=ls())

library(tidyverse)
library(ggplot2)
thefts <- X2015_State_Top10Report_wTotalThefts

#creating make column -----
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
      ifelse(grepl("Chrysler  ", thefts$make_and_model, ignore.case = T), "Chrysler  ",
      ifelse(grepl("Dodge", thefts$make_and_model, ignore.case = T), "Dodge", "Other")))))))))))))
                  



# create model column 
thefts <- thefts %>% 
  rename( model = make_and_model)
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















# install.packages("devtools") ----
# devtools::install_github("haleyjeppson/ggmosaic")
# library(ggmosaic)

(theft_example <- ggplot(data = thefts) +
  geom_mosaic(aes(x=product(, ``, ), fill = `Make/Model`, alpha = `State`)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="Do you recline?", x="Eliminate reclining?:Is it rude to recline?", title = "Mosaic Plot (3 variables)"))

(ggplot(data = thefts) +
  geom_mosaic(aes(x = product(state, `make_and_model`), fill=state)) + 
  labs(title='state | make/model'))

