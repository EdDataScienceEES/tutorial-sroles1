rm(list=ls()) # clear environment

# libraries
library(readxl) 
library(tidyverse)
library(stringr)

thef_data <- read_excel("data/2015_State_Top10Report_wTotalThefts.xlsx") # read in car thefts data

# view unique values in each column 
ulst <- lapply(thefts_data, unique)
ulst

#thef_data$rank <- as.factor(thef_data$rank) ----
#class(thef_data$rank)

# We can see that the make_and_model column is providing us with two bits of infomation which
# could be displayed clearer. So, we seperate the columns. 

# create a make column
thef_data$make <- ifelse(grepl("Toy", thef_data$make_and_model, ignore.case = T), "Toyota", 
               ifelse(grepl("Chev", thef_data$make_and_model, ignore.case = T), "Chevrolet",
               ifelse(grepl("Ford", thef_data$make_and_model, ignore.case = T), "Ford",
               ifelse(grepl("GMC", thef_data$make_and_model, ignore.case = T), "GMC",
               ifelse(grepl("Honda", thef_data$make_and_model, ignore.case = T), "Honda",
               ifelse(grepl("Jeep", thef_data$make_and_model, ignore.case = T), "Jeep",
               ifelse(grepl("Nissan", thef_data$make_and_model, ignore.case = T), "Nissan",
               ifelse(grepl("Acura", thef_data$make_and_model, ignore.case = T), "Acura",
               ifelse(grepl("Pontiac ", thef_data$make_and_model, ignore.case = T), "Pontiac ",
               ifelse(grepl("Hyundai ", thef_data$make_and_model, ignore.case = T), "Hyundai ",
               ifelse(grepl("Subaru ", thef_data$make_and_model, ignore.case = T), "Subaru ",
               ifelse(grepl("Chrysler", thef_data$make_and_model, ignore.case = T), "Chrysler  ",
               ifelse(grepl("Dodge", thef_data$make_and_model, ignore.case = T), "Dodge", "Other")))))))))))))


# create model column 

thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Toyota", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Chevrolet", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Ford", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "GMC", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Honda", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Jeep", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Nissan", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Acura", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Pontiac", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Hyundai", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Subaru", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Chrysler", "")
thef_data <- thef_data %>%
  mutate_at("make_and_model", str_replace, "Dodge", "")

thef_data <- thef_data %>% 
  rename( model = make_and_model) 




library(plotly) ----


# filter only to rank 1 (the most stolen) ----
rank_1 <- thef_data %>% filter(rank == 1)

# group years 
thef_data$year_groups<- ifelse(thef_data$year<2020& thef_data$year>=2010, "2011-16",
                        ifelse(thef_data$year<2010&thef_data$year>=2000, "2000-10",
                             #  ifelse(thef_data$year<2005&thef_data$year>=2000, "20s",
                                # ifelse(thef_data$year<2000&thef_data$year>=1995, "d",
                                             ifelse(thef_data$year<2000&thef_data$year>=1989, "1989-99", "f")))

                                                                 
(thefts_plot5<-ggplot(thef_data, aes(x = thefts, y = model, colour = rank)) + 
  geom_point(size = 0.5) + 
  scale_x_continuous(trans = "log10") +
  theme(strip.text.y = element_text(angle = 0, size = 10),
        strip.text.x = element_text(angle = 90, size = 6))
)


(thefts_plot6 <-thefts_plot5 + facet_grid(make ~ ., scales = "free") +
  theme(strip.text.x = element_text(angle = 90, size = 1)) +
    theme(strip.text.y = element_text(angle = 0))
)



thef_data$model <- reorder(thef_data$model, thef_data$thefts)
thef_data$make <- reorder(thef_data$make, -thef_data$thefts)
  
(theft_facet <- ggplot(thef_data, aes(x = thefts, y = model, colour = make )) + 
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
library(plotly)

(make_plot <- thef_data %>% 
    ggplot(aes(label = model, label_2 = rank, label_3 = state)) +
  geom_point(aes(x = thefts, y = year, colour = make)) +
  scale_x_continuous(trans = "log10") +
    scale_y_reverse()
)
ggplotly(make_plot)

(state_plot <- thef_data %>% 
    ggplot(aes(label = model, label_2 = rank, label_3 = state)) +
    geom_point(aes(x = thefts, y = year, colour = state)) +
    scale_x_continuous(trans = "log10") +
    scale_y_reverse()
)
ggplotly(state_plot)





library(htmlwidgets)
p <- plot_ly(x = rnorm(100))
saveWidget(p, "p1.html", selfcontained = F, libdir = "lib")
saveWidget(p, "p2.html", selfcontained = F, libdir = "lib")

browseVignettes("widgetframe")
