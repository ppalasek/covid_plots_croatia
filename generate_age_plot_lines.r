library(jsonlite)
library(ggplot2)
library(zoo) 

library(colorspace)
library(gridExtra)

library(plotly)
library(htmlwidgets)

library(sf)
sf_use_s2(FALSE)

library(lubridate)
library(plyr)

library(dplyr)

library(tidyr)

library(reshape2)


library("scales")                                   
Sys.setlocale("LC_TIME", "hr_HR.UTF-8")


# get last_date_
load('data/latest/last_date_.Rda')


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


f <- list(size = 13, color = "black")

p <- list()
a <- list()
i <-22



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


current_population <-c(population_by_age[1:nrow(population_by_age) - 1, c(i + 1)])

age_reshaped_sum7[, 1:ncol(age_reshaped_sum7)] <- sweep(age_reshaped_sum7[, 1:ncol(age_reshaped_sum7)], 2, current_population / 100000, `/`)
# 
# change column names to age groups
colnames(age_reshaped_sum7)[1:length(labels)] <- labels

age_reshaped_sum7$Datum <- age_reshaped$Datum

data_to_plot <- head(age_reshaped_sum7, n=nrow(age_reshaped_sum7) - 1)

last_date <- strftime(data_to_plot$Datum[(nrow(data_to_plot))], "%d.%m.%Y.")



d <- melt(data_to_plot, id.vars="Datum")

colnames(d)[2:3] <- c('Dobna_skupina', 'Broj_zadnjih_7_data')

my_breaks <-c(0, 10, 50, 100, 200, 400, 800)
my_labels <-c('0', '10', '50', '100', '200', '400', '800+')

# p <- ggplot(d, aes_string('Datum', colnames(d)[2], fill='Broj_zadnjih_7_data')) + 
#   geom_tile() +
#   ylab("Dobna skupina") +
#   scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika',
#                        limits = c(0, 800), labels=my_labels, breaks=my_breaks) +
#   theme_minimal()

d

# 18-24
# 25-49
# 50-59
# 60-69
# 70-79
# 80+

library(scales)


colour_hex_codes <- hue_pal()(6)

p <- ggplot() + 
  geom_line(data=data_to_plot, aes(Datum, y=`20-24`, colour='20-24', size='20-24')) +
  
  geom_line(data=data_to_plot, aes(Datum, y=`30-34`, colour='30-34', size='30-34')) +
  geom_line(data=data_to_plot, aes(Datum, y=`35-39`, colour='35-39', size='35-39')) +
  geom_line(data=data_to_plot, aes(Datum, y=`40-44`, colour='40-44', size='40-44')) +
  geom_line(data=data_to_plot, aes(Datum, y=`45-49`, colour='45-49', size='45-49')) +
  geom_line(data=data_to_plot, aes(Datum, y=`50-54`, colour='50-54', size='50-54')) +
  geom_line(data=data_to_plot, aes(Datum, y=`55-59`, colour='55-59', size='55-59')) +
  geom_line(data=data_to_plot, aes(Datum, y=`60-64`, colour='60-64', size='60-64')) +
  geom_line(data=data_to_plot, aes(Datum, y=`65-69`, colour='65-69', size='65-69')) +
  
  
  
  geom_line(data=data_to_plot, aes(Datum, y=`70-74`, colour='70-74', size='70-74')) +
  geom_line(data=data_to_plot, aes(Datum, y=`75-79`, colour='75-79', size='75-79')) + 
  geom_line(data=data_to_plot, aes(Datum, y=`80-84`, colour='80-84', size='80-84')) +
  geom_line(data=data_to_plot, aes(Datum, y=`85+`, colour='85+', size='85+')) +
  
  geom_line(data=data_to_plot, aes(Datum, y=`25-29`, colour='25-29', size='25-29')) +
  
  geom_line(data=data_to_plot, aes(Datum, y=`0-4`, colour='0-4', size='0-4')) +
  geom_line(data=data_to_plot, aes(Datum, y=`5-9`, colour='5-9', size='5-9')) +
  
  
  geom_line(data=data_to_plot, aes(Datum, y=`15-19`, colour='15-19', size='15-19')) + 
  geom_line(data=data_to_plot, aes(Datum, y=`10-14`, colour='10-14', size='10-14')) +
  
  #scale_x_date(labels = date_format("%d.%m."), date_breaks = "1 week") +
  scale_x_date(labels = date_format("%b %Y."), date_breaks = "1 month") +
  scale_color_manual(name='Dobna skupina', values = c(
    '0-4' = 'black',
    '5-9' = 'black',
    '10-14' = 'black',
    
    '15-19' = colour_hex_codes[1],
    '20-24' = colour_hex_codes[1],
    
    
    '25-29' = colour_hex_codes[2],
    '30-34' = colour_hex_codes[2],
    '35-39' = colour_hex_codes[2],
    '40-44' = colour_hex_codes[2],
    '45-49' = colour_hex_codes[2],
    
    '50-54' = colour_hex_codes[3],
    '55-59' = colour_hex_codes[3],
    '60-64' = colour_hex_codes[4],
    '65-69' = colour_hex_codes[4],
    '70-74' = colour_hex_codes[5],
    '75-79' = colour_hex_codes[5],
    '80-84' = colour_hex_codes[6],
    '85+' = colour_hex_codes[6]))  +
  scale_size_manual(name='Dobna skupina', values = c(
    '0-4' = 0.3,
    '5-9' = 0.7,
    '10-14' = 1.2,
    
    '15-19' = 1.2,
    '20-24' = 0.5,
    
    '25-29' = 1.2,
    '30-34' = 0.9,
    '35-39' = 0.6,
    '40-44' = 0.4,
    '45-49' = 0.2,
    
    '50-54' = 1,
    '55-59' = 0.5,
    '60-64' = 1,
    '65-69' = 0.5,
    '70-74' = 1,
    '75-79' = 0.5,
    '80-84' = 1,
    '85+' = 0.5)) +
  theme_minimal() +
  theme(legend.position="right") +
  ylab('Broj slučajeva na 100k stanovnika') +
  labs(title = paste('Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama u Hrvatskoj (02.03.2020. - ', last_date, ')', sep='')) +
  theme(text = element_text(size=18)) +
  labs(caption = paste('Izvori podataka: koronavirus.hr (broj slučajeva), dzs.hr (broj stanovnika po dobnim skupinama, podaci iz 2021.). Generirano:', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h.'), 'Autor: Petar Palašek, ppalasek.github.io')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p


ggsave(paste('img/', last_date_, '_cases_per_age_group_lines.png', sep = ''),
       plot = p, dpi=300, width=1600*4, height=700*4, units="px",
       bg = "white")



p <- ggplot() + 
  geom_line(data=data_to_plot, aes(Datum, y=`20-24`, colour='20-24', size='20-24')) +
  
  geom_line(data=data_to_plot, aes(Datum, y=`30-34`, colour='30-34', size='30-34')) +
  geom_line(data=data_to_plot, aes(Datum, y=`35-39`, colour='35-39', size='35-39')) +
  geom_line(data=data_to_plot, aes(Datum, y=`40-44`, colour='40-44', size='40-44')) +
  geom_line(data=data_to_plot, aes(Datum, y=`45-49`, colour='45-49', size='45-49')) +
  geom_line(data=data_to_plot, aes(Datum, y=`50-54`, colour='50-54', size='50-54')) +
  geom_line(data=data_to_plot, aes(Datum, y=`55-59`, colour='55-59', size='55-59')) +
  geom_line(data=data_to_plot, aes(Datum, y=`60-64`, colour='60-64', size='60-64')) +
  geom_line(data=data_to_plot, aes(Datum, y=`65-69`, colour='65-69', size='65-69')) +
  
  
  
  geom_line(data=data_to_plot, aes(Datum, y=`70-74`, colour='70-74', size='70-74')) +
  geom_line(data=data_to_plot, aes(Datum, y=`75-79`, colour='75-79', size='75-79')) + 
  geom_line(data=data_to_plot, aes(Datum, y=`80-84`, colour='80-84', size='80-84')) +
  geom_line(data=data_to_plot, aes(Datum, y=`85+`, colour='85+', size='85+')) +
  
  geom_line(data=data_to_plot, aes(Datum, y=`25-29`, colour='25-29', size='25-29')) +
  
  geom_line(data=data_to_plot, aes(Datum, y=`0-4`, colour='0-4', size='0-4')) +
  geom_line(data=data_to_plot, aes(Datum, y=`5-9`, colour='5-9', size='5-9')) +
  
  
  geom_line(data=data_to_plot, aes(Datum, y=`15-19`, colour='15-19', size='15-19')) + 
  geom_line(data=data_to_plot, aes(Datum, y=`10-14`, colour='10-14', size='10-14')) +
  
  #scale_x_date(labels = date_format("%d.%m."), date_breaks = "1 week") +
  scale_x_date(labels = date_format("%b %Y."), date_breaks = "1 month") +
  scale_color_manual(name='Dobna skupina', values = c(
    '0-4' = 'black',
    '5-9' = 'black',
    '10-14' = 'black',
    
    '15-19' = colour_hex_codes[1],
    '20-24' = colour_hex_codes[1],
    
    
    '25-29' = colour_hex_codes[2],
    '30-34' = colour_hex_codes[2],
    '35-39' = colour_hex_codes[2],
    '40-44' = colour_hex_codes[2],
    '45-49' = colour_hex_codes[2],
    
    '50-54' = colour_hex_codes[3],
    '55-59' = colour_hex_codes[3],
    '60-64' = colour_hex_codes[4],
    '65-69' = colour_hex_codes[4],
    '70-74' = colour_hex_codes[5],
    '75-79' = colour_hex_codes[5],
    '80-84' = colour_hex_codes[6],
    '85+' = colour_hex_codes[6]))  +
  scale_size_manual(name='Dobna skupina', values = c(
    '0-4' = 0.3,
    '5-9' = 0.7,
    '10-14' = 1.2,
    
    '15-19' = 1.2,
    '20-24' = 0.5,
    
    '25-29' = 1.2,
    '30-34' = 0.9,
    '35-39' = 0.6,
    '40-44' = 0.4,
    '45-49' = 0.2,
    
    '50-54' = 1,
    '55-59' = 0.5,
    '60-64' = 1,
    '65-69' = 0.5,
    '70-74' = 1,
    '75-79' = 0.5,
    '80-84' = 1,
    '85+' = 0.5)) +
  theme_minimal() +
  theme(legend.position="right") +
  ylab('Broj slučajeva na 100k stanovnika') +
  scale_y_continuous(trans='pseudo_log', breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)) +
  labs(title = paste('Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama u Hrvatskoj (02.03.2020. - ', last_date, ') (logaritamska skala)', sep='')) +
  theme(text = element_text(size=18)) +
  labs(caption = paste('Izvori podataka: koronavirus.hr (broj slučajeva), dzs.hr (broj stanovnika po dobnim skupinama, podaci iz 2021.). Generirano:', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h.'), 'Autor: Petar Palašek, ppalasek.github.io')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p


ggsave(paste('img/', last_date_, '_cases_per_age_group_lines_log.png', sep = ''),
       plot = p, dpi=300, width=1600*4, height=700*4, units="px",
       bg = "white")

p


sink("table.txt")

cat(paste("| Dobna skupina | Na 100k stanovnika<br>u 7 dana do ", last_date, " | 1 na svakih | Promjena u odnosu<br>na prošli tjedan |\n", sep=''))
cat("| :-----------: | :----------------: | :---------: | :--------------------------------: |\n")

for (r in colnames(data_to_plot)) {
  curr <- data_to_plot[nrow(data_to_plot), r]
  prev <- data_to_plot[nrow(data_to_plot) - 7, r]
  
  if (r != 'Datum') {
    curr_1_na <- 100000 / curr 
    
    cat(sprintf("| %s | %2.0f | %2.0f | %+2.0f%% |\n", r, curr, curr_1_na, (((curr - prev) / prev) * 100)))
  }
}

sink()
