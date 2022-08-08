library(jsonlite)
library(zoo) 
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(readr)

library(grid)
library(patchwork)


library(tidyr)
# 
library(lubridate)
library(plyr)
# 
library(dplyr)
# 
# 
# library(reshape2)
library(ggpubr)


library(sf)
sf_use_s2(FALSE)

library("viridis")

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")



# get last_date_
load('data/latest/last_date_.Rda')

print(last_date_)
# get age data
json_data <- fromJSON('data/latest/last_data_po_osobama.json')

population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')


# age grouping step
step <- 5


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

json_data %>% group_by(Zupanija) %>% tally()

counties


json_d <- json_data %>% group_by(Datum, Zupanija, age_group) %>% tally()

print(nrow(json_data))
# 3: 10-14
print(nrow(json_d))


# 
# 

f <- list(size = 13, color = "black")


generated_time = Sys.time() + as.difftime(1, units="hours")

start_j = 873

# 875 25/07

j = start_j
max_j = 0

while ((j < max_j) | (j == start_j)) {
  j <- j + 1
  
  print(j)
  
  p <- list()
  a <- list()
  
  for (current_group_index in seq(1, 18)) {
    current_age_group_data <- json_d[json_d$age_group == current_group_index,]
    
    current_age_group_name <- population_by_age[current_group_index, "Dobna_skupina"]
    
    if (current_age_group_name == '85plus') {
      current_age_group_name <- '85+'
    }
    
    
    current_age_group_data_reshaped <- current_age_group_data %>%
                           select(-age_group)%>%
                           drop_na(Datum) %>%
                           pivot_wider( names_from = Zupanija, values_from = n) %>%
                           mutate_at(vars(-("Datum")), ~replace(., is.na(.), 0))
    
    
    col_names <- names(current_age_group_data_reshaped)
    col_names <- col_names[! col_names %in% c('Datum')]
    
    current_age_group_data_reshaped <- current_age_group_data_reshaped[, c('Datum', sort(col_names))]
    
    
    
    # add missing dates
    missing_rows <- as.Date(setdiff(seq.Date(min_date, max_date, by="day"), current_age_group_data_reshaped$Datum))
    missing_data <- data.frame(missing_rows)
    colnames(missing_data)[1] <- "Datum"
    
    current_age_group_data_reshaped <- rbind.fill(current_age_group_data_reshaped, missing_data)
    current_age_group_data_reshaped[is.na(current_age_group_data_reshaped)] <- 0
    
    # sort by date  
    current_age_group_data_reshaped <- current_age_group_data_reshaped[order(current_age_group_data_reshaped$Datum),]
    
    # calc 7 day sums
    age_reshaped_sum7 <- as.data.frame(rollapply(current_age_group_data_reshaped[, names(current_age_group_data_reshaped) != "Datum"], 7, sum, fill=0, align="right"))
    
    age_reshaped_sum7$Datum <- current_age_group_data_reshaped$Datum
    
    
    population_per_county_current_age_group <- population_by_age[current_group_index, ]
    
    population_reordered <- as.numeric(population_per_county_current_age_group[1, c(2:22)])
    
    sum_7_df  <- age_reshaped_sum7
    
    max_j <- nrow(sum_7_df)
    
    print(max_j)
    
    last_date <- format(as.Date(sum_7_df[j, ]$Datum), '%d.%m.%Y.')
    last_date__ <- format(as.Date(sum_7_df[j, ]$Datum), '%Y_%m_%d')

    sum_7_reordered <- t(sum_7_df[j, c(1:21)])
    sum_7_reordered_norm <- (sum_7_reordered / population_reordered) * 100000
    
    
    colnames(sum_7_reordered)[1] <- 'Ukupno_7d'
    colnames(sum_7_reordered_norm)[1] <- 'Ukupno_7d_norm'
    
    
    hr <- st_read(dsn = "data/Official_Croatia_Boundaries/CROATIA_HR_Županije_ADMIN1.shp")
    
    hr <- hr[order(hr$ZUP_IME),]
    hr$Zupanija <- hr$ZUP_IME
    
    
    hr <-cbind(hr, sum_7_reordered_norm, sum_7_reordered, population_reordered)
    # colnames(hr)
    colnames(hr)[6] <- 'Populacija'
    
    # print(colnames(hr))
    
    my_breaks <-c(0, 100, 200, 400, 800, 1600)
    my_labels <-c('0',  '100', '200', '400', '800', '1600+')
    
    p[[current_group_index]] <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 7 dana na 100k stanovnika: ", round(Ukupno_7d_norm, digits= 2), sep=""))) +


    
      ggtitle(paste("Dobna skupina ", current_age_group_name, sep="")) +
      
      geom_sf(aes_string(fill = 'Ukupno_7d_norm')) 
      # scale_fill_viridis(option='B', direction=-1, oob = scales::squish, breaks=my_breaks, guide = "none") +
      
      #if (current_group_index != 12) {
      #  p[[current_group_index]] <- p[[current_group_index]] + scale_fill_viridis(option='B', direction=-1, oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks, guide = "none")
      #} else {
        p[[current_group_index]] <- p[[current_group_index]] + scale_fill_viridis(option='B', direction=-1, oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks)
      #}
    
      
       p[[current_group_index]] <- p[[current_group_index]] + theme_void()
    
    
    
  }

  fp <- p[[1]]

  for (kk in seq(2, 18)) {
    fp <- fp + p[[kk]]
  }
    
  fp <- fp + plot_layout(guides = 'collect',  ncol = 6, nrow=3) +
    plot_annotation(title = paste("COVID 19 u Hrvatskoj: Ukupan broj slučajeva po dobnim skupinama u prethodnih 7 dana na 100000 stanovnika (", last_date, ")", sep="")) +
    plot_annotation(caption = paste('Boje prikazuju ukupan broj slučajeva u prethodnih 7 dana u svakoj županiji i dobnoj skupini, normalizirano na 100000 stanovnika. Generirano: ', format(generated_time, '%d.%m.%Y.'), ', izvor podataka: koronavirus.hr, dzs.hr (popis stanovništva 2021.), autor: Petar Palašek, ppalasek.github.io', sep='')) +
    plot_annotation(theme = theme(plot.title = element_text(size = 20), plot.caption = element_text(size = 13)))
  
  print(paste('img/per_age_group_img/', last_date__, '_map_7_day_per_100k_age_groups.png', sep = ''))
  
  ggsave(paste('img/per_age_group_img/', last_date__, '_map_7_day_per_100k_age_groups.png', sep = ''), plot = fp, dpi=75, width=1920, height=1080, units="px", bg='white', limitsize = FALSE)
  
  
}

ggsave(paste('img/per_age_group_img/', last_date_, '_map_7_day_per_100k_age_groups.png', sep = ''), plot = fp, dpi=75, width=1920, height=1080, units="px", bg='white', limitsize = FALSE)
 


ggsave(paste('img/', last_date_, '_map_7_day_per_100k_age_groups.png', sep = ''), plot = fp, dpi=75, width=1920, height=1080, units="px", bg='white', limitsize = FALSE)
