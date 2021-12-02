library(plotly)
library(htmlwidgets)

library(sf)
sf_use_s2(FALSE)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")


# before running download data from https://www.diva-gis.org/gdata and save into data folder
hr <- st_read(dsn = "data/HRV_adm", layer = "HRV_adm1")

region <- rep(NA, nrow(hr))

region[c(1, 18, 12, 2, 11, 19, 6, 15)] <- 'Panonska Hrvatska'
region[c(13, 9, 20, 14, 16, 5, 3)] <- 'Jadranska Hrvatska'
region[c(4)] <- 'Grad Zagreb'
region[c(10, 17, 7, 8, 21)] <- 'Sjeverna Hrvatska'

colnames(hr)[5] <- "Zupanija"

region <- region[c(14, 1:13, 15:21)]

hr <- cbind(hr, region)
colnames(hr)[10] <- "Regija"



load('data/latest/percentage_change.Rda')
load('data/latest/sum_7_df.Rda')
load('data/latest/sum_14_df.Rda')



# get last_date_
load('data/latest/last_date_.Rda')
load('data/latest/last_date.Rda')



percentage_change_reordered <- t(percentage_change[nrow(percentage_change), c(14, 1:13, 15:21)])
colnames(percentage_change_reordered)[1] <- 'Tjedna razlika'


population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')
population_per_county <- population_by_age[nrow(population_by_age), ]

# broj stanovnika po zupanijama preuzet s https://www.dzs.hr/
population_reordered <- as.numeric(population_per_county[1, c(15, 2:14, 16:22)])

sum_7_reordered <- t(sum_7_df[nrow(sum_7_df), c(14, 1:13, 15:21)])
sum_7_reordered_norm <- (sum_7_reordered / population_reordered) * 100000

colnames(sum_7_reordered)[1] <- 'Ukupno_7d'
colnames(sum_7_reordered_norm)[1] <- 'Ukupno_7d_norm'

sum_14_reordered <- t(sum_14_df[nrow(sum_14_df), c(14, 1:13, 15:21)])
sum_14_reordered_norm <- (sum_14_reordered / population_reordered) * 100000

colnames(sum_14_reordered)[1] <- 'Ukupno_14d'
colnames(sum_14_reordered_norm)[1] <- 'Ukupno_14d_norm'

hr <-cbind(hr, percentage_change_reordered, sum_7_reordered_norm, sum_14_reordered_norm, sum_7_reordered, sum_14_reordered, population_reordered)

colnames(hr)[16] <- 'Populacija'

print(colnames(hr))




Ukupno_regija <- hr %>% group_by(Regija) %>% summarise(Ukupno_7d = sum(Ukupno_7d),
                                                       Ukupno_14d = sum(Ukupno_14d),
                                                       Populacija_regija = sum(Populacija))

Ukupno_regija$Ukupno_7d_norm_regija <- Ukupno_regija$Ukupno_7d / Ukupno_regija$Populacija_regija * 100000
Ukupno_regija$Ukupno_14d_norm_regija <- Ukupno_regija$Ukupno_14d / Ukupno_regija$Populacija_regija  * 100000

hr_region_map14 <- ggplot(Ukupno_regija, aes(text = paste("Regija: ", Regija, "<br>", "Ukupno u zadnjih 14 dana na 100k stanovnika: ", round(Ukupno_14d_norm_regija, digits= 2), sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika po regijama (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_14d_norm_regija')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(0,  200), oob = scales::squish, name='Broj slučajeva') +
  geom_sf_text(aes(label=round(Ukupno_14d_norm_regija, digits= 2)), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 14 dana po regijama, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_region_map14

ggsave(paste('img/', last_date_, '_map_14_day_per_100k_region.png', sep = ''), plot = hr_region_map14, dpi=300, width=309.80, height=215.90, units="mm")



hr_map <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Tjedna razlika: ", round(Tjedna.razlika, digits= 2), "%", "<br>",
                                      "Ukupno u zadnjih 7 dana: ", Ukupno_7d, "<br>",
                                      "Ukupno u zadnjih 14 dana: ", Ukupno_14d, "<br>",
                                      "Ukupno u zadnjih 7 dana na 100k: ", round(Ukupno_7d_norm, digits= 2), "<br>",
                                      "Ukupno u zadnjih 14 dana na 100k: ", round(Ukupno_14d_norm, digits= 2),  "<br>",
                                      "Populacija: ", Populacija,  "<br>",
                                      sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Pregled tjedne promjene broja zaraženih po županijama (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Tjedna.razlika')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(-50, 50), oob = scales::oob_squish_any, name='Promjena u postocima') +
  geom_sf_text(aes(label=paste(round(Tjedna.razlika, digits= 2), "%", sep="")), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju promjenu prosječnog broja slučajeva zadnjih tjedan dana u usporedbi s prosjekom broja slučajeva prethodnog tjedna.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map

ggsave(paste('img/', last_date_, '_map.png', sep = ''), plot = hr_map, dpi=300, width=309.80, height=215.90, units="mm")


hr_map7 <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 7 dana na 100k stanovnika: ", round(Ukupno_7d_norm, digits= 2), sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 7 dana na 100000 stanovnika (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_7d_norm')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(0,  50), oob = scales::squish, name='Broj slučajeva') +
  geom_sf_text(aes(label=round(Ukupno_7d_norm, digits= 2)), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 7 dana u svakoj županiji, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map7

ggsave(paste('img/', last_date_, '_map_7_day_per_100k.png', sep = ''), plot = hr_map7, dpi=300, width=309.80, height=215.90, units="mm")

hr_map14 <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Ukupno u zadnjih 14 dana na 100k stanovnika: ", round(Ukupno_14d_norm, digits= 2), sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika (", last_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'Ukupno_14d_norm')) +
  scale_fill_distiller(palette = "RdYlGn", limits = c(0,  100), oob = scales::squish, name='Broj slučajeva') +
  geom_sf_text(aes(label=round(Ukupno_14d_norm, digits= 2)), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Boje prikazuju ukupan broj slučajeva zadnjih 14 dana u svakoj županiji, normalizirano na 100000 stanovnika.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: koronavirus.hr, dzs.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

hr_map14

ggsave(paste('img/', last_date_, '_map_14_day_per_100k.png', sep = ''), plot = hr_map14, dpi=300, width=309.80, height=215.90, units="mm")

hr_map <- ggplotly(hr_map, tooltip = c("text"))

hr_map

saveWidget(hr_map, file = "html/index_map.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (", last_date, ")", sep=""))