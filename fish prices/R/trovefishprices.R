library(tidyverse)
library(googlesheets4)
library(stringr)

setwd("C:/Users/21729759/gitdt/myrepo/fish prices")

data <- read_sheet("https://docs.google.com/spreadsheets/d/1Bzw870G2b9sR5UEvzbn4yQPFyTweRK49HDehQO4d_FQ/edit#gid=1693170744")
googlesheets4::gs4_deauth()
googlesheets4::gs4_auth()
head(data)

dt <- as.tibble(data) %>% 
  separate_wider_delim('price/lb', delim = "s", names = c("shilling", "pence")) %>%  
  filter(!is.na(year)) %>% 
  mutate(pence = ceiling(as.numeric(pence))) %>% 
  mutate(shilling = as.numeric(shilling)) %>% 
  mutate(full_price_shilling = (12*shilling) + (pence)) %>% 
  mutate(str_replace_all(Species,'schnapper','snapper')) %>% 
  mutate(Species=as.factor(Species)) 
  mutate(target_sp = pecies=as.factor(Species)) 

demersal_species <- subset(dt,Species=="dhufish"|Species =="snapper")

ggplot(data = demersal_species, aes(x = year, y = full_price_shilling, colour=Species)) + 
  geom_point() 
 



glimpse(dt)

ggplot(data = dt, aes(x = year, y = full_price_shilling)) + 
  geom_point() 
 
  
abline(lm(full_price_shilling ~ year, data = dt))


ggplot(data = dt, aes(x = year, y = full_price_shilling, colour=Species)) + 
  geom_point(data=subset(dt,Species=="dhufish"|Species =="snapper")) 
  
 
 



#####run GLM, include model with supply and demand?

lm1 = lm(formula = full_price_shilling~year, data = target_species)
summary(lm1)




