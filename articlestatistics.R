library(tidyverse)
setwd("~/github/covid-research-volumeR")
rs <- read_excel("data/All_Articles_Excel_Reduced_Cleaned.xlsx") 

research <- rs %>% select(`Date Added`, Author, Title, Year, `Journal/Publisher`, Language, Keywords) %>% 
  rename(date = `Date Added`, journal = `Journal/Publisher`) %>%
  mutate(status = if_else(str_detect(journal,"Rxiv"),'Preprint','Reviewed'))

research$date <- str_replace(research$date, " EDT ", " ")
research$date <- as.Date(research$date, format = "%a %b %d %T %Y")

datecount <- research %>% group_by(date) %>% 
  summarize(n()) %>% 
  rename(count = 'n()') %>% 
  filter(date > '2020-03-30')

publications <- research %>% group_by(journal) %>% 
  summarize(n()) %>% rename(count = 'n()') %>% 
  filter(count > 5) %>% arrange(journal) 

preprints <- research %>% group_by(date, status) %>% 
  summarize(n()) %>% rename(count = 'n()') %>% 
  filter(count > 5) %>% 
  filter(date > '2020-03-30')

g <- ggplot(datecount, aes(x=date, y=count)) + geom_point() + geom_smooth()
g

p <- ggplot(preprints, aes(x=date, y=count, color=status)) + 
      geom_point() + geom_path() + geom_smooth() +
      xlab("Database Entry Date") + ylab("Number of Publications Per Day") +
      ggtitle("COVID Research Publication Rate (Worldwide)")
p
