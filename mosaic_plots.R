rm(list=ls()) # clear environment
ulst2 <- lapply(X2020_ap_exit_polls_combined, unique)
ulst2

# load exit poll data
exit_poll <- read_excel("data/2020_ap_exit_polls_combined.xlsx")

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


library(ggmosaic)

(mosaic2 <-  ggplot(data = filt) +
  geom_mosaic(aes(x = product(Demographic), fill = State_Abbr)) + 
  theme_mosaic()
  )

(mosaic3 <- ggplot(data = filt) +
  geom_mosaic(aes(x=product( Demographic, State_Abbr ),
                  fill = State_Abbr, alpha = majority_votes, colour = Demographic), offset = 0.05) + 
  scale_alpha_manual(values =c(.3,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="State", x="Income Demographic:Who they Voted For", title = "Exit Poll")
)

(mosaic4 <- ggplot(data = filt) +
    geom_mosaic(aes(x=product(majority_votes , Demographic),
                    fill = State_Abbr, Demographic alpha = Demographic), offset = 0.05) + 
    scale_alpha_manual(values =c(.2,.5,.7,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="State", x="Income Demographic:Who they Voted For", title = "Exit Poll") +
    facet_grid(. ~ State_Abbr)
)

(mosaic3 <- ggplot(data = exit_poll) +
    geom_mosaic(aes(x=product( Demographic, majority_votes ),
                    fill = State_Abbr, alpha = majority_votes, colour = Demographic), offset = 0.05) + 
    scale_alpha_manual(values =c(.3,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    labs(y="State", x="Income Demographic:Who they Voted For", title = "Exit Poll")
)


