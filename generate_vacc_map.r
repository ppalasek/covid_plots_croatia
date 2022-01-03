library(jsonlite)
library(ggplot2)
library(zoo) 
library(readr)

library(colorspace)
library(gridExtra)

library(plotly)
library(htmlwidgets)

library(sf)

library(lubridate)
library(plyr)

library(dplyr)

library(tidyr)
library(lubridate)

library(reshape2)
library(readr)

# get last_date_
load('data/latest/last_date_.Rda')

#https://www.hzjz.hr/aktualnosti/covid-19-izvjesce-hzjz-a/
vacc_data <- read.csv(file = 'data/vaccination/cro_vaccination_per_county_2021_12_26.csv')
vacc_date <- "26.12.2021."


first_dose_reordered <- as.numeric(vacc_data[1, c(15, 2:14, 16:22)])
fully_vacc_reordered <- as.numeric(vacc_data[2, c(15, 2:14, 16:22)])

first_dose_reordered
fully_vacc_reordered

# before running download data from https://www.diva-gis.org/gdata and save into data folder
hr <- st_read(dsn = "data/HRV_adm", layer = "HRV_adm1")

colnames(hr)[5] <- "Zupanija"

hr <-cbind(hr, first_dose_reordered, fully_vacc_reordered)

hr_map <- ggplot(hr, aes(text = paste("Županija: ", Zupanija, "<br>", "Prva doza: ", first_dose_reordered, "%<br>",
                                      "Završeno cijepljenje: ", fully_vacc_reordered, "%<br>",
                                      sep=""))) +
  ggtitle(paste("COVID 19 u Hrvatskoj: Procijepljenost po županijama (", vacc_date, ")", sep="")) +
  geom_sf(aes_string(fill = 'fully_vacc_reordered')) +
  scale_fill_distiller(palette = "RdYlGn", direction=1, limits = c(0, 100), oob = scales::squish, name='Postotak\nprocijepljenosti') +
  geom_sf_text(aes(label=paste(round(fully_vacc_reordered, digits= 0), "%", sep="")), fontface="bold", size=5, color="black") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(caption = paste('Postotak procijepljenosti po županijama (s obje doze) u odnosu na ukupno stanovništvo županije.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), ', izvor podataka: hzjz.hr, autor: Petar Palašek', sep='')) +
  theme(plot.caption = element_text(hjust = 0))

ggsave(paste('img/', last_date_, '_vaccination.png', sep = ''), plot = hr_map, dpi=300, width=309.80, height=215.90, units="mm")

hr_map_p <- ggplotly(hr_map, tooltip = c("text"))

hr_map_p

print(last_date_)

saveWidget(hr_map_p, file = "html/index_vaccination.html", title = paste("COVID 19 u Hrvatskoj: Postotak procijepljenosti po županijama (", vacc_date, ")", sep=""))


