#install.packages("productivity")

library(productivity)
test.data<-usagri %>% select(q.land, p.capital, p.land, p.labor, p.crop)
quantile(usagri$q.land, probs=c(0,0.25,0.5,0.75,1))
test.data$size <- ifelse(test.data$q.land >989375.900, 1,
                         ifelse(test.data$q.land <228244.846, 0, 0.5))
levels(test.data$size)
test.data<-test.data %>% select(-q.land)

us_agri <- test.data

write.csv(us_agri ,"C:\\Users\\s1869354\\Documents\\tutorial-sroles1\\data\\us_agri.csv", row.names = FALSE)



us_agri <- test.data[-sample(1:nrow(test.data), 380), ]

rownames(us_agri) <- seq(length=nrow(Us_agri))

