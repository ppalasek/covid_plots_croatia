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
library(tidyverse)
library(reshape2)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

# get last_date_
load('data/latest/last_date_.Rda')
load('data/latest/last_date.Rda')



# get age data
json_data <- fromJSON('data/latest/last_data_po_osobama.json')


step <- 5
n <- 180
skip_dates_before <- as.Date('2022-03-01')


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



for (j in seq(0, difftime(Sys.Date() - 3, as.Date('2022-08-01'), units = c("days")))) {
  print(j)
  current_date <- as.Date('2022-08-01') + days(j)
  
  if (current_date < skip_dates_before) {
    next
  }
  
  prev_date  = as.Date(current_date) - days(6)


  
  f <- list(size = 18, color = "black")
  
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
  
    
    current_day <- data_to_plot[data_to_plot$Datum <= current_date & data_to_plot$Datum >= prev_date, ]
    
    
    cd <- melt(current_day, id.vars="Datum")
    
    if ((i - 1) %% 5 == 0) {
      ylab <- ylab('Broj slučajeva / 100k\n(ukupno u 7 dana)')
    }
    else {
      ylab <- ylab('')
    }
    
    p[[i]] <- ggplot() + 
      geom_point(data=cd[cd$Datum!=max(cd$Datum), ], aes(x=variable, y=value), colour='gray', size=0.6) +
      geom_point(data=cd[cd$Datum==max(cd$Datum), ], aes(x=variable, y=value), size=1.5) +
      ylab +
      xlab('Dobna skupina') +
      ylim(0, max(1000, max(cd$value))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

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
  
  f <- list(size = 15, color = "black")
  
  s <- subplot(p, nrows = 5, margin=c(0.01,0.01,0.04,0.04), titleY = TRUE) %>%
    add_annotations(x = 0.7,
                    y = 0.07,
                    text = paste('Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po županijama i dobnim skupinama u Hrvatskoj (', format(as.Date(current_date), "%d.%m.%Y."), ')\n\n', 'Crna točka: broj slučajeva u 7 dana do prikazanog datuma i dobnu skupinu, sive točke označavaju sedmodnevni broj slučajeva za 6 dana ranije.\nNa y-osi prikazan je broj slučajeva na 100 tisuća stanovnika, na x-osi prikazane su dobne skupine.\n\n', 'Izvori podataka: koronavirus.hr (broj slučajeva), dzs.hr (broj stanovnika po dobnim skupinama, podaci iz 2021.).\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ' Autor: Petar Palašek, ppalasek.github.io', sep=''),

                    font = f,
                    xref = "paper",
                    yref = "paper",
                    align='left',
                    ax = 0,
                    ay = 0)
  
  orca(s, file = paste('img/anim_dots_counties/anim_', str_pad(j, 6, pad = "0"), sep = '', '.png'), width = 27 * 72, height = 13 * 72)
  
  
}

# last frame for page
orca(s, file = paste('img/', last_date_, 'all_counties_dots.png', sep = ''), width = 27 * 72, height = 13 * 72)


# generate gif
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/home/pero/.cargo/bin/", sep=":"))
system(paste("cd img/anim_dots_counties && gifski -o anim_all_1920.gif anim*.png --width 1920 --fps 8")) # && cp anim_aug_1200.gif img/", last_date_, "anim_aug_1200.gif", sep=""))

