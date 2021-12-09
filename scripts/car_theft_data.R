#intro ---- 
rm(list=ls())

data(`C:/Users/s1869354/OneDrive - University of Edinburgh/Year 3/Data Science/2015_State_Top10Report_wTotalThefts`)
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
      ifelse(grepl("Chrysler", thefts$make_and_model, ignore.case = T), "Chrysler  ",
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


library(plotly)
install.packages("gapminder")
library(gapminder)

(thefts_plot<- thefts$model <- reorder(thefts$model, thefts$thefts)
thefts$make <- reorder(thefts$make, -thefts$thefts)
ggplot(thefts, aes(x = thefts, y = model)) + 
  geom_point() + 
  scale_x_continuous(trans = "log10") +
  facet_grid(make ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))
)

(thefts_plot<-  ggplot(thefts, aes(x = thefts, y = model)) + 
    geom_point() + 
    scale_x_continuous(trans = "log10") +
    theme(strip.text.y = element_text(angle = 0))
)

ggplotly(thefts_plot)

(thefts_plot1<-  ggplot(thefts, aes(thefts, model)) + 
    geom_point() + 
    scale_x_continuous(trans = "log10")


ggplotly(thefts_plot1)
  
  +
    facet_grid(make ~ ., scales = "free", space = "free") +
    theme(strip.text.y = element_text(angle = 0))
)



(thefts_p<- 
  ggplot(data = thefts, aes(x = thefts, y = model)) + 
  geom_point() + 
  scale_x_continuous(trans = "log10"))+
  facet_grid(make ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))

ggplotly(thefts_p)

(thefts_plot2<-ggplot(thefts, aes(x = thefts, y = model)) + 
    geom_point(size = 1) + 
    scale_x_continuous(trans = "log10") +
    facet_grid(make ~ ., scales = "free", space = "free") +
    theme(strip.text.y = element_text(angle = 0))
)
fig <- ggplotly(thefts_plot2)
fig <- fig %>% layout(autosize = F, width = 500, height = 500, margin = m)

#install.packages("devtools") ----
 #devtools::install_github("haleyjeppson/ggmosaic")
 #library(ggmosaic)

(thefts_plot<-ggplot(thefts, aes(x = rank, y = model, colour = state)) + 
   geom_point() + 
   #   scale_x_continuous(trans = "log10") +
   facet_grid(make ~ ., scales = "free", space = "free") +
   theme(strip.text.y = element_text(angle = 0))
)

library(plotly)
install.packages("gapminder")
library(gapminder)
ggplotly(thefts_plot)


