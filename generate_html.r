library(jsonlite)
library(ggplot2)
library(zoo) 

library(colorspace)
library(gridExtra)

library(plotly)
library(htmlwidgets)

library(sf)

json_data <- fromJSON('https://www.koronavirus.hr/json/?action=po_danima_zupanijama')

missing_data <-fromJSON('data/missing_data.json')

json_data$Datum <- as.Date(json_data$Datum, format="%Y-%m-%d")
missing_data$Datum <- as.Date(missing_data$Datum, format="%Y-%m-%d")

json_data <- rbind(json_data, missing_data)

data_sorted <- json_data[order(json_data$Datum),]

counties <- data_sorted[1, ]$PodaciDetaljno[[1]]$Zupanija

col_classes = c("Date", rep("integer", length(counties)))
col_names = c('Datum', counties)

cumulative_cases <- read.table(text="", colClasses=col_classes, col.names=col_names)

for(d in 1:nrow(data_sorted)) {
  cumulative_cases[nrow(cumulative_cases) + 1,] = data.frame(data_sorted[d, ]$Datum, t(data_sorted[d, ]$PodaciDetaljno[[1]]$broj_zarazenih))
}

cumulative_cases$Hrvatska <- rowSums(cumulative_cases[,-c(1)])

diff_df <- cumulative_cases[-1, -c(1)] - cumulative_cases[-nrow(cumulative_cases), -c(1)]

diff_df$Datum <- cumulative_cases$Datum[-c(nrow(cumulative_cases))]

avg7_df <- as.data.frame(rollapply(diff_df[, 1:ncol(diff_df) - 1], 7, mean, fill=NA, align="right"))

avg7_df_lag <- rbind(NA, head(avg7_df, -7))

percentage_change = (tail(avg7_df, n=nrow(avg7_df_lag)) / avg7_df_lag - 1) * 100

sum_14_df = (tail(avg7_df, n=nrow(avg7_df_lag)) + avg7_df_lag) * 7
sum_7_df = tail(avg7_df, n=nrow(avg7_df_lag)) * 7

colnames(sum_14_df) <- paste("ukupno_14d", colnames(percentage_change), sep = "_")
colnames(sum_7_df) <- paste("ukupno_7d", colnames(percentage_change), sep = "_")

avg7_df$Datum <- cumulative_cases$Datum[-c(nrow(cumulative_cases))]
avg7_df_lag$Datum <- cumulative_cases$Datum[c(8:nrow(cumulative_cases) - 1)]

sum_14_df$Datum <- cumulative_cases$Datum[c(8:nrow(cumulative_cases) - 1)]
sum_7_df$Datum <-  cumulative_cases$Datum[c(8:nrow(cumulative_cases) - 1)]


colnames(percentage_change) <- paste("tjedna_razlika", colnames(percentage_change), sep = "_")

percentage_change$Datum <- cumulative_cases$Datum[c(8:nrow(cumulative_cases) - 1)]

plot_df <- merge(diff_df, percentage_change, by=c("Datum"))

last_date <- strftime(percentage_change$Datum[nrow(percentage_change)], "%d.%m.%Y.")

n <- 60

data_to_plot <- tail(plot_df, n=n)

f <- list(
  size = 13,
  color = "black")

for(use_log_scale in c(FALSE, TRUE)) {
  p <- list()
  a <- list()
  
  for(i in 1:22) {
    title <- colnames(data_to_plot)[1 + i]
  
    if (i == 21) {
      title <- gsub("[.]", "", title)
    }
    else  
    {
      title <- gsub("[.]", "-", title)
    }
  
    p[[i]] <- ggplot(data=data_to_plot, aes_string(x='Datum', y=colnames(data_to_plot)[1 + i])) +
      ylab("Broj slučajeva") +
      geom_bar(stat="identity") +
      geom_col(aes_string(fill=colnames(data_to_plot)[1 + i + 22])) +
      scale_fill_distiller(palette = "RdYlGn", limits = c(-50, 50), oob = scales::squish, name='') +
      geom_line(data=tail(avg7_df, n=n), aes_string(x='Datum', y=colnames(data_to_plot)[1 + i]), size=1, colour='blue') +  #
      theme_minimal()
    
    if (use_log_scale) {
      p[[i]] <- p[[i]] + scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
    }
    
    p[[i]] <- ggplotly(p[[i]])
    
    a[[i]] <- list(
      text = paste('\n',title, ' (', diff_df[nrow(diff_df), i], ')\nTjedna razlika: ', format(round(data_to_plot[nrow(data_to_plot), 1 + i + 22], 2), nsmall = 2), '%', sep=''),
      font = f,
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 0.92,
      showarrow = FALSE
    )
    
    p[[i]] <- p[[i]] %>%
      layout(annotations = a[i])
  }
  
  s <- subplot(p, nrows = 5, margin=c(0.02,0.02,0.05,0.02), titleY = TRUE) %>%
    add_annotations(x = 0.65,
                    y = 0.07,
                    text = paste('COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (', last_date ,')\n\nBoje prikazuju promjenu prosječnog broja slučajeva zadnjih tjedan\ndana u usporedbi s prosjekom broja slučajeva prethodnog tjedna.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), '\nIzvor podataka: koronavirus.hr\n\nAutor: Petar Palašek', sep=''),
                    font = f,
                    xref = "paper",
                    yref = "paper",
                    align='left',
                    ax = 0,
                    ay = 0)
  
  if (use_log_scale) {
    saveWidget(s, file = "html/index_log.html", title = "COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama")
  }
  else {
    saveWidget(s, file = "html/index.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (", last_date, ")", sep=""))
  }
}

# before running download data from https://www.diva-gis.org/gdata and save into data folder
hr <- st_read(dsn = "data/HRV_adm", layer = "HRV_adm1")

colnames(hr)[5] <- "Zupanija"

percentage_change_reordered <- t(percentage_change[nrow(percentage_change), c(14, 1:13, 15:21)])
colnames(percentage_change_reordered)[1] <- 'Tjedna razlika'


sum_7_reordered <- t(sum_7_df[nrow(sum_7_df), c(14, 1:13, 15:21)])

# broj stanovnika po zupanijama preuzet s https://www.dzs.hr/
population = c()
population = c(106258, 137487, 121816, 807254, 209573, 115484, 106367, 124517, 44625, 109232, 272673, 66256, 282730, 99210, 145904, 447747, 166112, 73641, 150985, 168213, 309169)
population_reordered = population[c(14, 1:13, 15:21)]


sum_7_reordered <- round((sum_7_reordered / population_reordered) * 100000)

colnames(sum_7_reordered)[1] <- 'Ukupno_7d'

sum_14_reordered <- t(sum_14_df[nrow(sum_14_df), c(14, 1:13, 15:21)])

sum_14_reordered <- round((sum_14_reordered / population_reordered) * 100000)

colnames(sum_14_reordered)[1] <- 'Ukupno_14d'

sum_14_reordered

hr <-cbind(hr, percentage_change_reordered, sum_7_reordered, sum_14_reordered)

hr

hr_map <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Tjedna razlika: ", round(Tjedna.razlika, digits= 2), "%", "<br>", "Ukupno u zadnjih 7 dana na 100k: ", Ukupno_7d, "<br>", "Ukupno u zadnjih 14 dana na 100k: ", Ukupno_14d,  sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Pregled tjedne promjene broja zaraženih po županijama (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Tjedna.razlika')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(-50, 50), oob = scales::squish, name='Promjena u postocima') +
  geom_sf_text(aes(label=paste(round(Tjedna.razlika, digits= 2), "%", sep="")), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju promjenu prosječnog broja slučajeva zadnjih tjedan dana u usporedbi s prosjekom broja slučajeva prethodnog tjedna.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map

ggsave("img/map.png", plot = hr_map)


hr_map7 <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 7 dana na 100k stanovnika: ", Ukupno_7d, sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 7 dana na 100000 stanovnika (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_7d')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(0,  50), oob = scales::squish, name='Broj slučajeva') +
  geom_sf_text(aes(label=Ukupno_7d), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 7 dana u svakoj županiji, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map7

ggsave("img/map_7_day_per_100k.png", plot = hr_map7)

hr_map14 <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 14 dana na 100k stanovnika: ", Ukupno_14d, sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_14d')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(0,  100), oob = scales::squish, name='Broj slučajeva') +
  geom_sf_text(aes(label=Ukupno_14d), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 14 dana u svakoj županiji, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map14

ggsave("img/map_14_day_per_100k.png", plot = hr_map14)

hr_map <- ggplotly(hr_map, tooltip = c("text"))

saveWidget(hr_map, file = "html/index_map.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (", last_date, ")", sep=""))
