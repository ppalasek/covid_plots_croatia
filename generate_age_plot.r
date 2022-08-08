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
library(lubridate)

library(reshape2)
library(reticulate)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

# get last_date_
load('data/latest/last_date_.Rda')

# get age data
json_data <- fromJSON('data/latest/last_data_po_osobama.json')

step <- 5
n <- 180


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


counties <- unique(json_data[c("Zupanija")])


f <- list(size = 13, color = "black")

p <- list()
a <- list()
i <- 0

p_change <- list()

for(county in c(sort(counties$Zupanija), 'Hrvatska')) {
  if (county == '') {
    next
  }
  
  i <- i + 1
  
  if (county == 'Hrvatska') {
    current_county_data <- json_data
  }
  else {
    current_county_data <- json_data[json_data$Zupanija == county, ]
  }
  
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
  
  # remove last few days as data might not be complete
  age_reshaped_sum7 <- head(age_reshaped_sum7, n=nrow(age_reshaped_sum7) - 1)
  
  data_to_plot <- tail(age_reshaped_sum7, n=n)
  
  d <- melt(data_to_plot, id.vars="Datum")
  
  colnames(d)[2:3] <- c('Dobna_skupina', 'Broj_zadnjih_7_data')
  
  my_breaks <-c(0, 50, 100, 200, 400, 800, 1600, 3200)
  my_labels <-c('0', '50', '100', '200', '400', '800', '1600', '3200+')
  
  p[[i]] <- ggplot(d, aes_string('Datum', colnames(d)[2], fill='Broj_zadnjih_7_data')) + 
    geom_tile() +
    ylab("Dobna skupina") +
    scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika',
                         limits = c(0, 3200), labels=my_labels, breaks=my_breaks) +
    theme_minimal()
  
  p[[i]] <- ggplotly(p[[i]])
  
  a[[i]] <- list(
    text = county,
    font = f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  p[[i]] <- p[[i]] %>%
    layout(annotations = a[i])
}

last_date <- strftime(data_to_plot$Datum[nrow(data_to_plot)], "%d.%m.%Y.")


from_date <- dmy(last_date) - days(n - 1)
from_date <- paste(day(from_date), month(from_date), year(from_date), sep='.')


s <- subplot(p, nrows = 5, margin=c(0.02,0.02,0.02,0.02), titleY = TRUE) %>%
  add_annotations(x = 0.7,
                  y = 0.07,
                  text = paste('COVID 19 u Hrvatskoj: Pregled broja zaraženih po dobnim skupinama (', from_date, ' - ', last_date ,')\n\nBoje prikazuju ukupan broj zaraženih u zadnjih 7 dana, normalizirano na 100000 stanovnika u svakoj dobnoj skupini.\nSvaka županija prikazana je na istoj skali. Crvena boja označava veći broj slučajeva.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), '\nIzvor podataka: koronavirus.hr (broj slučajeva), dzs.hr (broj stanovnika po dobnim skupinama, podaci iz 2021.)\n\nAutor: Petar Palašek', sep=''),
                  font = f,
                  xref = "paper",
                  yref = "paper",
                  align='left',
                  ax = 0,
                  ay = 0)

s

save_image(s, file = paste('img/', last_date_, '_per_age_group.png', sep = ''), width = 27 * 72, height = 13 * 72)


saveWidget(s, file = "html/index_per_age.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po dobnim skupinama (", last_date, ")", sep=""))
