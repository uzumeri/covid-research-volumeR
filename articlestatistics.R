library(tidyverse)
setwd("~/github/covid-research-volumeR")
rs <- read_excel("data/All_Articles_Excel_Reduced.xlsx") 

research <- rs %>% select(`Date Added`, Author, Title, Year, `Journal/Publisher`, Language, Keywords) %>% 
  rename(date = `Date Added`, journal = `Journal/Publisher`) %>%
  mutate(preprint = if_else(str_detect(journal,"Rxiv"),'Preprint','Reviewed'))

datecount <- research %>% group_by(date) %>% 
  summarize(n()) %>% 
  rename(count = 'n()') %>% 
  filter(date > '2020-03-30')

publications <- research %>% group_by(journal) %>% 
  summarize(n()) %>% rename(count = 'n()') %>% 
  filter(count > 5) %>% arrange(journal) 

preprints <- research %>% group_by(date, preprint) %>% 
  summarize(n()) %>% rename(count = 'n()') %>% 
  filter(count > 5) %>% 
  filter(date > '2020-03-30')

g <- ggplot(datecount, aes(x=date, y=count)) + geom_point() + geom_smooth()
g

p <- ggplot(preprints, aes(x=date, y=count, color=preprint)) + geom_point() + geom_path()
p
