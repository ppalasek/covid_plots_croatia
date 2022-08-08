library(plotly)
library(htmlwidgets)
library(ggtext) 
library("viridis")
library(sf)
library(jsonlite)
sf_use_s2(FALSE)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

hr <- st_read(dsn = "data/Official_Croatia_Boundaries/CROATIA_HR_Županije_ADMIN1.shp")

hr <- hr[order(hr$ZUP_IME),]
hr$Zupanija <- hr$ZUP_IME

hr$ZUP_IME

Regija <- rep(NA, nrow(hr))

Regija[c(1, 18, 12, 2, 11, 19, 6, 15)] <- 'Panonska Hrvatska'
Regija[c(13, 9, 20, 14, 16, 5, 3)] <- 'Jadranska Hrvatska'
Regija[c(4)] <- 'Grad Zagreb'
Regija[c(10, 17, 7, 8, 21)] <- 'Sjeverna Hrvatska'

hr <- cbind(hr, Regija)



load('data/latest/percentage_change.Rda')
load('data/latest/sum_7_df.Rda')
load('data/latest/sum_14_df.Rda')


holiday_note <- fromJSON('holiday_note.json')$note

print(holiday_note)

# get last_date_
load('data/latest/last_date_.Rda')
load('data/latest/last_date.Rda')

hr$ZUP_IME

percentage_change_reordered <- t(percentage_change[nrow(percentage_change), c(1:21)])
colnames(percentage_change_reordered)[1] <- 'Tjedna razlika'


population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')
population_per_county <- population_by_age[nrow(population_by_age), ]


hr$ZUP_IME

# broj stanovnika po zupanijama preuzet s https://www.dzs.hr/
population_reordered <- as.numeric(population_per_county[1, c(2:22)])

population_reordered

sum_7_df[nrow(sum_7_df),c(1:21)]

sum_7_reordered <- t(sum_7_df[nrow(sum_7_df), c(1:21)])
sum_7_reordered_norm <- (sum_7_reordered / population_reordered) * 100000

colnames(sum_7_reordered)[1] <- 'Ukupno_7d'
colnames(sum_7_reordered_norm)[1] <- 'Ukupno_7d_norm'


sum_14_reordered <- t(sum_14_df[nrow(sum_14_df), c(1:21)])
sum_14_reordered_norm <- (sum_14_reordered / population_reordered) * 100000

colnames(sum_14_reordered)[1] <- 'Ukupno_14d'
colnames(sum_14_reordered_norm)[1] <- 'Ukupno_14d_norm'

hr <-cbind(hr, percentage_change_reordered, sum_7_reordered_norm, sum_14_reordered_norm, sum_7_reordered, sum_14_reordered, population_reordered)
colnames(hr)
colnames(hr)[10] <- 'Populacija'

print(colnames(hr))



Ukupno_regija <- hr %>% group_by(Regija) %>% summarise(Ukupno_7d = sum(Ukupno_7d),
                                                       Ukupno_14d = sum(Ukupno_14d),
                                                       Populacija_regija = sum(Populacija))

Ukupno_regija

Ukupno_regija$Ukupno_7d_norm_regija <- Ukupno_regija$Ukupno_7d / Ukupno_regija$Populacija_regija * 100000
Ukupno_regija$Ukupno_14d_norm_regija <- Ukupno_regija$Ukupno_14d / Ukupno_regija$Populacija_regija  * 100000

Ukupno_regija

my_breaks <-c(0, 200, 400, 800, 1600, 3200)
my_labels <-c('0', '200', '400', '800', '1600', '3200+')
# hr_region_map14

hr_region_map14 <- ggplot(Ukupno_regija, aes(text = paste("Regija: ", Regija, "<br>", "Ukupno u zadnjih 14 dana na 100k stanovnika: ", round(Ukupno_14d_norm_regija, digits= 2), sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika po regijama (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_14d_norm_regija')) +
  #scale_fill_distiller(palette = "RdYlGn", limits = c(0,  200), oob = scales::squish, name='Broj slučajeva') +
  #scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 14 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks) +
  #scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 14 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks) +
  scale_fill_viridis(option='B', direction=-1, oob = scales::squish, name='Ukupno\nu 14 dana\nna 100000\nstanovnika', limits = c(0, 3200), labels=my_labels, breaks=my_breaks) +
  geom_sf_label(aes(label=round(Ukupno_14d_norm_regija, digits= 0)), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 14 dana po regijama, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr (popis stanovništva 2021.), autor: Petar Palašek, ppalasek.github.io', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_region_map14

ggsave(paste('img/', last_date_, '_map_14_day_per_100k_region.png', sep = ''), plot = hr_region_map14, dpi=300, width=309.80, height=215.90, units="mm")


if (is.null(holiday_note)) {
  caption <- paste('Boje prikazuju promjenu prosječnog broja slučajeva zadnjih tjedan dana u usporedbi s prosjekom broja slučajeva prethodnog tjedna.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr (popis stanovništva 2021.), autor: Petar Palašek, ppalasek.github.io', sep='')
} else {
  caption <- paste('Boje prikazuju promjenu prosječnog broja slučajeva zadnjih tjedan dana u usporedbi s prosjekom broja slučajeva prethodnog tjedna.\n\n<span style="color:#ff0000;">NAPOMENA: ', holiday_note, '</span>\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr (popis stanovništva 2021.), autor: Petar Palašek, ppalasek.github.io', sep='')
}

hr_map <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Tjedna razlika: ", round(Tjedna.razlika, digits= 2), "%", "<br>",
                                      "Ukupno u zadnjih 7 dana: ", Ukupno_7d, "<br>",
                                      "Ukupno u zadnjih 14 dana: ", Ukupno_14d, "<br>",
                                      "Ukupno u zadnjih 7 dana na 100k: ", round(Ukupno_7d_norm, digits= 2), "<br>",
                                      "Ukupno u zadnjih 14 dana na 100k: ", round(Ukupno_14d_norm, digits= 2),  "<br>",
                                      "Populacija: ", Populacija,  "<br>",
                                      sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Pregled tjedne promjene broja zaraženih po županijama (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Tjedna.razlika')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(-75, 75), oob = scales::oob_squish_any, name='Promjena u postocima') +
  geom_sf_label(aes(label=paste(sprintf("%+d", round(Tjedna.razlika, digits= 0)), "%", sep="")), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = caption) +
  theme(plot.caption = element_markdown(hjust = 0))

hr_map

ggsave(paste('img/', last_date_, '_map.png', sep = ''), plot = hr_map, dpi=300, width=309.80, height=215.90, units="mm", bg='#ffffff')

my_breaks <-c(0,  100, 200, 400, 800, 1600)
my_labels <-c('0', '100', '200', '400', '800', '1600+')

hr_map7 <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 7 dana na 100k stanovnika: ", round(Ukupno_7d_norm, digits= 2), sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 7 dana na 100000 stanovnika (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_7d_norm')) +
  #scale_fill_distiller(palette = "RdYlGn", limits = c(0,  1600), oob = scales::squish, name='Broj slučajeva') +
  #scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika', limits = c(0, 800), labels=my_labels, breaks=my_breaks) +
  # scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks) +
  scale_fill_viridis(option='B', direction=-1, oob = scales::squish, name='Ukupno\nu 7 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks) +
  geom_sf_label(aes(label=round(Ukupno_7d_norm, digits= 0)), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 7 dana u svakoj županiji, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr (popis stanovništva 2021.), autor: Petar Palašek, ppalasek.github.io', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map7

ggsave(paste('img/', last_date_, '_map_7_day_per_100k.png', sep = ''), plot = hr_map7, dpi=300, width=309.80, height=215.90, units="mm", bg='#ffffff')

my_breaks <-c(0, 200, 400, 800, 1600, 3200)
my_labels <-c('0',  '200', '400', '800', '1600', '3200+')

hr_map14 <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 14 dana na 100k stanovnika: ", round(Ukupno_14d_norm, digits= 2), sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_14d_norm')) +
  # scale_fill_distiller(palette = "RdYlGn", limits = c(0,  3200), oob = scales::squish, name='Broj slučajeva') +
  # scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 14 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks) +
  # scale_fill_distiller(palette="Spectral", oob = scales::squish, name='Ukupno\nu 14 dana\nna 100000\nstanovnika', limits = c(0, 1600), labels=my_labels, breaks=my_breaks) +
  scale_fill_viridis(option='B', direction=-1, oob = scales::squish, name='Ukupno\nu 14 dana\nna 100000\nstanovnika', limits = c(0, 3200), labels=my_labels, breaks=my_breaks) +
  geom_sf_label(aes(label=round(Ukupno_14d_norm, digits= 0)), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 14 dana u svakoj županiji, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr (popis stanovništva 2021.), autor: Petar Palašek, ppalasek.github.io', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map14

ggsave(paste('img/', last_date_, '_map_14_day_per_100k.png', sep = ''), plot = hr_map14, dpi=300, width=309.80, height=215.90, units="mm", bg='#ffffff')

hr_map <- ggplotly(hr_map, tooltip = c("text"))

hr_map

saveWidget(hr_map, file = "html/index_map.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (", last_date, ")", sep=""))
