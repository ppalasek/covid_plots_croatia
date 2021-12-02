library(jsonlite)
library(ggplot2)
library(zoo) 
library(tidyverse)
library(scales)

diff_cases_df = read.csv('data/latest/diff_df.csv')
diff_deaths_df = read.csv('data/latest/diff_deaths_df.csv')
population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')


diff_cases_df <- diff_cases_df %>% 
  mutate(Panonska.Hrvatska = rowSums(diff_cases_df[, c(1, 18, 12, 2, 11, 19, 6, 15)]))  %>% 
  mutate(Jadranska.Hrvatska = rowSums(diff_cases_df[, c(13, 9, 20, 14, 16, 5, 3)]))  %>% 
  mutate(Sjeverna.Hrvatska = rowSums(diff_cases_df[, c(10, 17, 7, 8, 21)]))
  
diff_cases_df <- diff_cases_df %>% 
  mutate(Panonska.Hrvatska.7d = rollapply(diff_cases_df$Panonska.Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Jadranska.Hrvatska.7d = rollapply(diff_cases_df$Jadranska.Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Sjeverna.Hrvatska.7d = rollapply(diff_cases_df$Sjeverna.Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Grad.Zagreb.7d = rollapply(diff_cases_df$Grad.Zagreb, 7, sum, fill=NA, align="right")) %>%
  mutate(Hrvatska.7d = rollapply(diff_cases_df$Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Panonska.Hrvatska.14d = rollapply(diff_cases_df$Panonska.Hrvatska, 14, sum, fill=NA, align="right")) %>%
  mutate(Jadranska.Hrvatska.14d = rollapply(diff_cases_df$Jadranska.Hrvatska, 14, sum, fill=NA, align="right")) %>%
  mutate(Sjeverna.Hrvatska.14d = rollapply(diff_cases_df$Sjeverna.Hrvatska, 14, sum, fill=NA, align="right")) %>%
  mutate(Grad.Zagreb.14d = rollapply(diff_cases_df$Grad.Zagreb, 14, sum, fill=NA, align="right")) %>%
  mutate(Hrvatska.14d = rollapply(diff_cases_df$Hrvatska, 14, sum, fill=NA, align="right"))

diff_cases_df <- diff_cases_df %>% 
  mutate(Panonska.Hrvatska.diff = (Panonska.Hrvatska.7d / (Panonska.Hrvatska.14d - Panonska.Hrvatska.7d) - 1) * 100) %>%
  mutate(Jadranska.Hrvatska.diff = (Jadranska.Hrvatska.7d / (Jadranska.Hrvatska.14d - Jadranska.Hrvatska.7d) - 1) * 100) %>%
  mutate(Sjeverna.Hrvatska.diff = (Sjeverna.Hrvatska.7d / (Sjeverna.Hrvatska.14d - Sjeverna.Hrvatska.7d) - 1) * 100) %>%
  mutate(Grad.Zagreb.diff = (Grad.Zagreb.7d / (Grad.Zagreb.14d - Grad.Zagreb.7d) - 1) * 100) %>%
  mutate(Hrvatska.diff = (Hrvatska.7d / (Hrvatska.14d - Hrvatska.7d) - 1) * 100)


diff_cases_df$Datum <- as.Date(diff_cases_df$Datum)



diff_deaths_df <- diff_deaths_df %>% 
  mutate(Panonska.Hrvatska = rowSums(diff_deaths_df[, c(1, 18, 12, 2, 11, 19, 6, 15)]))  %>% 
  mutate(Jadranska.Hrvatska = rowSums(diff_deaths_df[, c(13, 9, 20, 14, 16, 5, 3)]))  %>% 
  mutate(Sjeverna.Hrvatska = rowSums(diff_deaths_df[, c(10, 17, 7, 8, 21)]))

diff_deaths_df <- diff_deaths_df %>% 
  mutate(Panonska.Hrvatska.7d = rollapply(diff_deaths_df$Panonska.Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Jadranska.Hrvatska.7d = rollapply(diff_deaths_df$Jadranska.Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Sjeverna.Hrvatska.7d = rollapply(diff_deaths_df$Sjeverna.Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Grad.Zagreb.7d = rollapply(diff_deaths_df$Grad.Zagreb, 7, sum, fill=NA, align="right")) %>%
  mutate(Hrvatska.7d = rollapply(diff_deaths_df$Hrvatska, 7, sum, fill=NA, align="right")) %>%
  mutate(Panonska.Hrvatska.14d = rollapply(diff_deaths_df$Panonska.Hrvatska, 14, sum, fill=NA, align="right")) %>%
  mutate(Jadranska.Hrvatska.14d = rollapply(diff_deaths_df$Jadranska.Hrvatska, 14, sum, fill=NA, align="right")) %>%
  mutate(Sjeverna.Hrvatska.14d = rollapply(diff_deaths_df$Sjeverna.Hrvatska, 14, sum, fill=NA, align="right")) %>%
  mutate(Grad.Zagreb.14d = rollapply(diff_deaths_df$Grad.Zagreb, 14, sum, fill=NA, align="right")) %>%
  mutate(Hrvatska.14d = rollapply(diff_deaths_df$Hrvatska, 14, sum, fill=NA, align="right"))
  
diff_deaths_df <- diff_deaths_df %>% 
  mutate(Panonska.Hrvatska.diff = (Panonska.Hrvatska.7d / (Panonska.Hrvatska.14d - Panonska.Hrvatska.7d) - 1) * 100) %>%
  mutate(Jadranska.Hrvatska.diff = (Jadranska.Hrvatska.7d / (Jadranska.Hrvatska.14d - Jadranska.Hrvatska.7d) - 1) * 100) %>%
  mutate(Sjeverna.Hrvatska.diff = (Sjeverna.Hrvatska.7d / (Sjeverna.Hrvatska.14d - Sjeverna.Hrvatska.7d) - 1) * 100) %>%
  mutate(Grad.Zagreb.diff = (Grad.Zagreb.7d / (Grad.Zagreb.14d - Grad.Zagreb.7d) - 1) * 100) %>%
  mutate(Hrvatska.diff = (Hrvatska.7d / (Hrvatska.14d - Hrvatska.7d) - 1) * 100)



# get last row
population_per_county <- population_by_age[nrow(population_by_age), -c(1)]

population_per_county

diff_deaths_df <- diff_deaths_df %>% 
  mutate(Panonska.Hrvatska.pop = sum(population_per_county[c(1, 18, 12, 2, 11, 19, 6, 15)]))  %>% 
  mutate(Jadranska.Hrvatska.pop = sum(population_per_county[c(13, 9, 20, 14, 16, 5, 3)]))  %>% 
  mutate(Sjeverna.Hrvatska.pop = sum(population_per_county[c(10, 17, 7, 8, 21)])) %>%
  mutate(Grad.Zagreb.pop = population_per_county$Grad.Zagreb)


diff_cases_df <- diff_cases_df %>% 
  mutate(Panonska.Hrvatska.pop = sum(population_per_county[c(1, 18, 12, 2, 11, 19, 6, 15)]))  %>% 
  mutate(Jadranska.Hrvatska.pop = sum(population_per_county[c(13, 9, 20, 14, 16, 5, 3)]))  %>% 
  mutate(Sjeverna.Hrvatska.pop = sum(population_per_county[c(10, 17, 7, 8, 21)])) %>%
  mutate(Grad.Zagreb.pop = population_per_county$Grad.Zagreb)



write.csv(diff_cases_df, 'data/latest/region_diff_cases_df.csv', row.names=FALSE, quote=FALSE) 
write.csv(diff_deaths_df, 'data/latest/region_diff_deaths_df.csv', row.names=FALSE, quote=FALSE) 

