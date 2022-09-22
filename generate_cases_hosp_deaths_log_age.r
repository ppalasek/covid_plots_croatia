library(tidyverse)
library(jsonlite)
library(lubridate)
library(zoo) 
library(scales)
library(plyr)

library(ggplot2)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

# get data from owid
data <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')  %>%
  filter(location %in% "Croatia")


data$date <- as.Date(data$date, format="%Y-%m-%d")

# read latest data we fetched from koronavirus.hr
diff_df <- read_csv('data/latest/diff_df.csv')
avg7_df <- as.data.frame(rollapply(diff_df[, 1:ncol(diff_df) - 1], 7, mean, fill=NA, align="right"))

diff_df$date <- diff_df$Datum
avg7_df$Hrvatska_avg7 <- avg7_df$Hrvatska
avg7_df$date <- diff_df$date

# get last_date_
load('data/latest/last_date_.Rda')


# hospitalisation data
hospitalisation_data <- read.csv(file ='data/latest/last_hzjz_data.csv')

print(hospitalisation_data)

hospitalisation_data$date <- as.Date(hospitalisation_data$datum, format="%d/%m/%Y")


print(max(hospitalisation_data$date))

data_sorted <- data[order(data$date),]
hospitalisation_data_sorted <- hospitalisation_data[order(hospitalisation_data$date),]

merged_data <- merge(data_sorted, hospitalisation_data_sorted, by="date", all=TRUE) %>%
  select(date, new_cases, new_deaths, people_fully_vaccinated, population, 
         pozitivne.osobe, new_hospitalised=hospitalizirani.u.zadnja.24.sata,
         new_on_respirator=na.respiratoru.u.zadnja.24.sata,
         new_deaths_hospital=preminuli.u.bolnici,
         new_tests=učinjeni.testovi) %>%
  mutate(new_cases_7da = zoo::rollmean(new_cases, k = 7, fill = NA, align = "right")) %>%
  mutate(new_deaths_7da = zoo::rollmean(new_deaths, k = 7, fill = NA, align = "right"))  %>%
  mutate(new_on_respirator_7da = zoo::rollmean(new_on_respirator, k = 7, fill = NA, align = "right"))  %>%
  mutate(new_hospitalised_7da = zoo::rollmean(new_hospitalised, k = 7, fill = NA, align = "right")) %>%
  mutate(death_ratio = lead(new_deaths_7da, n=15, default=NA) / new_cases_7da) %>%
  mutate(hospitalisation_ratio = lead(new_hospitalised_7da, n=7, default=NA) / new_cases_7da) %>%
  
  mutate(pozitivne.osobe_7da = zoo::rollmean(pozitivne.osobe, k = 7, fill = NA, align = "right")) %>%
  mutate(new_tests_7da = zoo::rollmean(new_tests, k = 7, fill = NA, align = "right")) %>%

  mutate(positive_percentage = pozitivne.osobe / new_tests) %>%
  mutate(positive_percentage_7da = pozitivne.osobe_7da / new_tests_7da)


only_perc <- merged_data %>% select(positive_percentage_7da, date) %>% drop_na()

last_date_perc <- format(as.Date(only_perc[nrow(only_perc), 'date']), '%d.%m.%Y.')


mylog10_trans <- function (base = 10) 
{
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^x
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(1e-100, Inf))
}



##########################################

p <- list()
a <- list()
i <-22

# get age data
json_data <- fromJSON('data/latest/last_data_po_osobama.json')

# age grouping step
step <- 5


population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')


json_data <- json_data %>%  mutate(dob = ifelse(dob < 1901, NA, dob))

json_data$Datum <- as.Date(json_data$Datum, format="%Y-%m-%d")

min_date = min(json_data$Datum)
max_date = max(json_data$Datum)


json_data <- json_data[order(json_data$Datum),]



json_data$dob <- as.Date(paste0(json_data$dob, '-01-01'), format="%Y-%m-%d")

json_data$age <- round(time_length(difftime(json_data$Datum, json_data$dob), "years"))






json_data$age_group <- findInterval(json_data$age, seq(0, 85, by=step))

labels <- paste0(seq(0, 85, by=step), '-', seq(step - 1, 85, by=step), sep='')

labels[length(labels)] = '85+'


# counties <- unique(json_data[c("Zupanija")])



current_county_data <- json_data

json_d <- current_county_data %>% group_by(Datum, age_group) %>% tally()

age_reshaped <- pivot_wider(json_d, names_from = age_group, values_from = n) %>%
  mutate_at(vars(-("Datum")), ~replace(., is.na(.), 0))

missing_cols <- setdiff(as.character(c(1:length(labels))), names(age_reshaped))

age_reshaped[missing_cols] <- 0

age_reshaped <- age_reshaped[c('Datum', as.character(1:length(labels)))]

# add missing dates
missing_rows <- as.Date(setdiff(seq.Date(min_date, max_date, by="day"), age_reshaped$Datum))
missing_data <- data.frame(missing_rows)
colnames(missing_data)[1] <- "Datum"

age_reshaped <- rbind.fill(age_reshaped, missing_data)
age_reshaped[is.na(age_reshaped)] <- 0

# sort by date  
age_reshaped <- age_reshaped[order(age_reshaped$Datum),]

# calc 7 day sums
age_reshaped_sum7 <- as.data.frame(rollapply(age_reshaped[, names(age_reshaped) != "Datum"], 7, sum, fill=0, align="right"))

older_sum <- age_reshaped_sum7

older_sum$older60 <- age_reshaped_sum7[, c(13)] + age_reshaped_sum7[, c(14)]  + age_reshaped_sum7[, c(15)]   + age_reshaped_sum7[, c(16)] + age_reshaped_sum7[, c(17)] + age_reshaped_sum7[, c(18)]
older_sum$older70 <- age_reshaped_sum7[, c(15)] + age_reshaped_sum7[, c(16)] + age_reshaped_sum7[, c(17)] + age_reshaped_sum7[, c(18)]
older_sum$older80 <- age_reshaped_sum7[, c(17)] + age_reshaped_sum7[, c(18)]




current_population <-c(population_by_age[1:nrow(population_by_age) - 1, c(i + 1)])

current_population <- c(current_population, 
                        current_population[13] + current_population[14] + current_population[15] + current_population[16] + current_population[17] + current_population[18],
                        current_population[15] + current_population[16] + current_population[17] + current_population[18],
                        current_population[17] + current_population[18])


age_reshaped_sum7 <- older_sum

# age_reshaped_sum7[, 1:ncol(age_reshaped_sum7)] <- sweep(age_reshaped_sum7[, 1:ncol(age_reshaped_sum7)], 2, current_population / 100000, `/`)

age_reshaped_sum7[, 1:ncol(age_reshaped_sum7)] <- sweep(age_reshaped_sum7[, 1:ncol(age_reshaped_sum7)], 2, current_population / 100000, `/`)



# 
# change column names to age groups
colnames(age_reshaped_sum7)[1:length(labels)] <- labels

colnames(age_reshaped_sum7)[(length(labels) + 1)] <- 'older60'
colnames(age_reshaped_sum7)[(length(labels) + 2)] <- 'older70'
colnames(age_reshaped_sum7)[(length(labels) + 3)] <- 'older80'

age_reshaped_sum7$Datum <- age_reshaped$Datum

data_to_plot <- head(age_reshaped_sum7, n=nrow(age_reshaped_sum7) - 1)







##########################################








g3 <- ggplot() + 
  geom_point(data=diff_df, aes_string(x='date', y="Hrvatska*100000/3871833"), colour="blue", alpha=0.2) + 

  geom_point(data=merged_data, aes_string(x='date', y="new_hospitalised*1000000/3871833"), colour="red", alpha=0.2) +
  geom_point(data=merged_data, aes_string(x='date', y="new_on_respirator"), colour="darkolivegreen4", alpha=0.2) +
  
  geom_point(data=merged_data, aes_string(x='date', y="new_deaths"), colour="black", alpha=0.2) +
  geom_line(data=merged_data, aes_string(x='date', y="new_deaths_7da"), colour="black", size=1.5, alpha=0.7) +
  geom_line(data=merged_data, aes_string(x='date', y="new_hospitalised_7da*1000000/3871833"), colour='red', size=1.5, alpha=0.7) +
  geom_line(data=merged_data, aes_string(x='date', y="new_on_respirator_7da"), colour='darkolivegreen4', size=1.5, alpha=0.7) +
  
  geom_line(data=avg7_df, aes_string(x='date', y="Hrvatska_avg7*100000/3871833"), colour='blue', size=1.5, alpha=0.7) +
  # #geom_line(data=data_to_plot, aes(Datum, y=`65-69`), colour='pink', size=1.5, alpha=0.7) +
  # geom_line(data=data_to_plot, aes(Datum, y=`70-74`), colour='yellow', size=1.5, alpha=0.7) +
  # geom_line(data=data_to_plot, aes(Datum, y=`75-79`), colour='orange', size=1.5, alpha=0.7) +
  # geom_line(data=data_to_plot, aes(Datum, y=`80-84`), colour='brown', size=1.5, alpha=0.7) +
  geom_line(data=data_to_plot, aes(Datum, y=`older60`), colour='pink', size=1.5, alpha=0.7) +
  geom_line(data=data_to_plot, aes(Datum, y=`older70`), colour='orange', size=1.5, alpha=0.7) +
  geom_line(data=data_to_plot, aes(Datum, y=`older80`), colour='yellow', size=1.5, alpha=0.7) +
                         
  ylab('Broj novih slučajeva na 100k stanovnika (plavo)\nBroj novih hospitalizacija na 1M stanovnika (crveno)\nBroj novih osoba na respiratoru (zeleno)\nBroj umrlih (crno)') +
  xlab('Datum') + 
  ggtitle('Kretanje broja COVID-19 slučajeva na 100 tisuća stanovika (plavo - svi, 60+ roza, 70+ narančasta, 80+ žuta),\nhospitaliziranih na 1 milijun stanovnika (crveno), osoba na respiratoru (zeleno) i umrlih (crno) u Hrvatskoj (sedmodnevni prosjek, logaritamska skala)') +
  scale_x_date(labels = date_format("%b %Y."), date_breaks = "1 month") +
  scale_y_continuous(trans='pseudo_log', breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)) +
  theme_minimal() +
  theme(text = element_text(size=18)) +
  labs(caption = paste('Izvori podataka: koronavirus.hr (slučajevi), ourworldindata.com (umrli), hzjz.hr (broj na respiratoru, hospitalizacije). Korištena populacija HR: 3871833. Generirano:', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h.'), 'Autor: Petar Palašek, ppalasek.github.io')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g3

ggsave(paste('img/', last_date_, '_cases_hospitalisations_deaths_log_age.png', sep = ''),
       plot = g3, dpi=300, width=1600*4, height=700*4, units="px",
       bg = "white")


print(max(merged_data$date))
