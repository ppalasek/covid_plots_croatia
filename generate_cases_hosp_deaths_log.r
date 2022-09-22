library(tidyverse)
library(jsonlite)
library(lubridate)
library(zoo) 
library(scales)


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

hospitalisation_data$date <- as.Date(hospitalisation_data$datum, format="%d/%m/%Y")

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

# 
# g <- ggplot() +
#   geom_point(data=merged_data, aes_string(x='date', y="positive_percentage"), colour='red', size=1) +
#   geom_line(data=merged_data, aes_string(x='date', y="positive_percentage_7da"),  size=1) +
#   scale_x_date(labels = date_format("%b %Y."), date_breaks = "1 month") +
#   scale_y_continuous(labels = scales::percent) +
#   ggtitle(paste('Kretanje udjela pozitivnih testova u Hrvatskoj do', last_date_perc, '(crveno - dnevni udio, crno - sedmodnevni prosjek)')) +
#   
#   ylab('Udio pozitivnih testova') +
#   xlab('Datum') + 
#   labs(caption = paste('Izvor podataka: hzjz.hr (broj pozitivnih i učinjenih testova). Generirano:', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h.'), 'Autor: Petar Palašek, ppalasek.github.io')) +
#   theme_minimal() +
#   theme(text = element_text(size=18)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#   
#   
# g
# 
# ggsave(paste('img/', last_date_, '_percentage_positive_tests.png', sep = ''),
#        plot = g, dpi=300, width=1600*4, height=700*4, units="px",
#        bg = "white")

mylog10_trans <- function (base = 10) 
{
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^x
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


g3 <- ggplot() + 
  geom_point(data=diff_df, aes_string(x='date', y="Hrvatska*100000/3871833"), colour="blue", alpha=0.2) + 

  geom_point(data=merged_data, aes_string(x='date', y="new_hospitalised*1000000/3871833"), colour="red", alpha=0.2) +
  geom_point(data=merged_data, aes_string(x='date', y="new_on_respirator"), colour="darkolivegreen4", alpha=0.2) +
  
  geom_point(data=merged_data, aes_string(x='date', y="new_deaths"), colour="black", alpha=0.2) +
  geom_line(data=merged_data, aes_string(x='date', y="new_deaths_7da"), colour="black", size=1.5, alpha=0.7) +
  geom_line(data=merged_data, aes_string(x='date', y="new_hospitalised_7da*1000000/3871833"), colour='red', size=1.5, alpha=0.7) +
  geom_line(data=merged_data, aes_string(x='date', y="new_on_respirator_7da"), colour='darkolivegreen4', size=1.5, alpha=0.7) +
  geom_line(data=avg7_df, aes_string(x='date', y="Hrvatska_avg7*100000/3871833"), colour='blue', size=1.5, alpha=0.7) +
  
  ylab('Broj novih slučajeva na 100k stanovnika (plavo)\nBroj novih hospitalizacija na 1M stanovnika (crveno)\nBroj novih osoba na respiratoru (zeleno)\nBroj umrlih (crno)') +
  xlab('Datum') + 
  ggtitle('Kretanje broja COVID-19 slučajeva na 100 tisuća stanovika (plavo), hospitaliziranih na 1 milijun stanovnika (crveno), osoba na respiratoru (zeleno) i\numrlih (crno) u Hrvatskoj (sedmodnevni prosjek, logaritamska skala)') +
  scale_x_date(labels = date_format("%b %Y."), date_breaks = "1 month") +
  scale_y_continuous(trans='pseudo_log', breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)) +
  theme_minimal() +
  theme(text = element_text(size=18)) +
  labs(caption = paste('Izvori podataka: koronavirus.hr (slučajevi), ourworldindata.com (umrli), hzjz.hr (broj na respiratoru, hospitalizacije). Korištena populacija HR: 3871833. Generirano:', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h.'), 'Autor: Petar Palašek, ppalasek.github.io')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g3

ggsave(paste('img/', last_date_, '_cases_hospitalisations_deaths_log.png', sep = ''),
       plot = g3, dpi=300, width=1600*4, height=700*4, units="px",
       bg = "white")


write.csv(merged_data, 'data/latest/mmerged_data_df.csv', row.names=FALSE, quote=FALSE) 
