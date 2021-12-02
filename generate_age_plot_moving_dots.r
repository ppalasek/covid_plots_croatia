library(jsonlite)
library(ggplot2)
library(zoo) 
library(readr)

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

Sys.Date()


library("scales")                                   
Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

# get last_date_
load('data/latest/last_date_.Rda')

# get age data
json_data <- fromJSON('data/latest/last_data_po_osobama.json')


step <- 5
n <- 574


population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')


json_data <- json_data %>%  mutate(dob = ifelse(dob < 1901, NA, dob))

json_data$Datum <- as.Date(json_data$Datum, format="%Y-%m-%d")

min_date = min(json_data$Datum)
max_date = max(json_data$Datum)


json_data <- json_data[order(json_data$Datum),]

last_date <- strftime(json_data$Datum[nrow(json_data)], "%d.%m.%Y.")


json_data$dob <- as.Date(paste0(json_data$dob, '-01-01'), format="%Y-%m-%d")

json_data$age <- round(time_length(difftime(json_data$Datum, json_data$dob), "years"))






json_data$age_group <- findInterval(json_data$age, seq(0, 85, by=step))

labels <- paste0(seq(0, 85, by=step), '-', seq(step - 1, 85, by=step), sep='')

labels[length(labels)] = '85+'


counties <- unique(json_data[c("Zupanija")])


f <- list(size = 13, color = "black")

p <- list()
a <- list()
i <-22 # Hrvatska total



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

data_to_plot <- age_reshaped_sum7 # tail(age_reshaped_sum7, n=n)

d <- melt(data_to_plot, id.vars="Datum")

colnames(d)[2:3] <- c('Dobna_skupina', 'Broj_zadnjih_7_data')

data_to_plot

d

library(tidyverse)

for (j in seq(0, difftime(Sys.Date() - 2, as.Date('2021-08-01'), units = c("days")))) {
  print(j)
  current_date <- as.Date('2021-08-01') + days(j)
  
  prev_date  = as.Date(current_date) - days(6)
  
  current_day <- data_to_plot[data_to_plot$Datum <= current_date & data_to_plot$Datum >= prev_date, ]
  
  cd <- melt(current_day, id.vars="Datum")
  
  p <- ggplot() + 
    geom_point(data=cd[cd$Datum!=max(cd$Datum), ], aes(x=variable, y=value), colour='gray', size=0.8) +
    geom_point(data=cd[cd$Datum==max(cd$Datum), ], aes(x=variable, y=value), size=2) +
    ylab('Broj slučajeva na 100k stanovnika (sedmodnevni prosjek)') +
    xlab('Dobna skupina') +
    ylim(0, 1750) +
    labs(title = 'Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama u Hrvatskoj',
         subtitle=paste(format(as.Date(current_date), "%d.%m.%Y."), 'Crna točka označava sedmodnevni prosjek za prikazani datum, sive točke označavaju sedmodnevni prosjek za 6 dana ranije.'),
         caption = 'Izvori podataka: koronavirus.hr (broj slučajeva), dzs.hr (broj stanovnika po dobnim skupinama, podaci iz 2019.). Autor: Petar Palašek. Inspirirano animacijom: @ProfColinDavis') +
    theme_minimal()
  p
  
  ggsave(paste('img/anim_dots/anim_', str_pad(j, 6, pad = "0"), sep = '', '.png'),
         plot = p, dpi=300, width=250, height=150, units="mm",
         bg = "white")
}


for (k in seq(40)) {
  ggsave(paste('img/anim_dots/anim_', str_pad(j + k, 6, pad = "0"), sep = '', '.png'),
         plot = p, dpi=300, width=250, height=150, units="mm",
         bg = "white")
}


Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/home/pero/.cargo/bin/", sep=":"))
system(paste("cd img/anim_dots && gifski -o anim_aug_1200.gif anim*.png --width 1200 --fps 8 && cp anim_aug_1200.gif ../", last_date_, "anim_aug_1200.gif", sep=""))
