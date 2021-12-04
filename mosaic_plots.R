rm(list=ls()) # clear environment
ulst2 <- lapply(exit_poll, unique)
ulst2

# load exit poll data
library(tidyverse)
library(readxl)
exit_poll <- read_excel("data/2020_ap_exit_polls_combined_1.xlsx")

# majority votes column
exit_poll$majority_votes<- ifelse(exit_poll$`Biden_%` >
                                    exit_poll$`Trump_%`, "Biden", "Trump")



#filt <- exit_poll %>% filter(Demographic == "Rural" |
 #                              Demographic == "Small Town" |
  #                             Demographic == "Suburban" |
   #                            Demographic == "Urban")

filt <- exit_poll %>% filter(Demographic == "Under $25,000"  |
                          Demographic == "$25,000 - $49,999" |
                          Demographic == "$50,000 - $74,999" |
                          Demographic == "$100,000+")

filt <- filt %>% filter(State_Abbr == "CA" |
                             State_Abbr == "TX" |
                            State_Abbr == "FL" |
                           State_Abbr == "NY")

#install.packages("splitstackshape")
library(splitstackshape)

expanded <- expandRows(filt, "Biden_%")
expanded2 <- expandRows(filt, "Trump_%")

expanded <- expandRows(expanded, "proportion")
expanded2 <- expandRows(expanded2, "proportion")

expanded2$voted_for <- "Trump"
expanded$voted_for <- "Biden"

combined <- rbind(expanded,expanded2)

expanded <- expanded %>% select(-"Trump_%")
expanded2 <- expanded2 %>% select(-"Biden_%")






library(ggmosaic)

#(mosaic2 <-  ggplot(data = filt) +
 # geom_mosaic(aes(x = product(Demographic), fill = State_Abbr)) + 
  #theme_mosaic()
  #)



(mosaic3 <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, alpha = State_Abbr, colour = Demographic), offset = 0.05) + 
    scale_alpha_manual(values =c(.3,.5,.7,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="State", x="Income Demographic:Who they Voted For", title = "Exit Poll")
  )

combined$Demographic <- factor(combined$Demographic, levels=c("Under $25,000", "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "$100,000+"))
#combined$voted_for <- factor(combined$voted_for, levels=c("", "","","","", "","","" ))
 


(mosaic3 <- ggplot(data = combined) +
    geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                    fill = voted_for, weight = Electoral_Votes_Available
                   # alpha = State_Abbr,
                ), offset = 0.03) + 
   # scale_alpha_manual(values =c(.3,.5,.7,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="State", x="Income Demographic:Who they Voted For", title = "Exit Poll") +
    facet_wrap(~Winner, nrow = 1)
)
  + 
    scale_x_continuous(labels= SoilSciGuylabs)

SoilSciGuylabs <- c("CA", "", "FL", "", "NY", "", "TX", "")


facet_wrap(~species, nrow = 1)
