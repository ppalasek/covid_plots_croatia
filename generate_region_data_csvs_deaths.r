library(jsonlite)
library(ggplot2)
library(zoo) 

library(colorspace)
library(gridExtra)

library(plotly)
library(htmlwidgets)

library(sf)
sf_use_s2(FALSE)

# TODO: rewrite this whole script, very badly written.

json_data <- fromJSON('data/latest/last_data_po_danima_zupanijama.json')
json_data <- json_data[, c("Datum", "PodaciDetaljno")]

missing_data <-fromJSON('data/missing_data.json')

json_data$Datum <- as.Date(json_data$Datum, format="%Y-%m-%d")
missing_data$Datum <- as.Date(missing_data$Datum, format="%Y-%m-%d")


json_data <- rbind(json_data, missing_data)

data_sorted <- json_data[order(json_data$Datum),]

population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')


counties <- data_sorted[1, ]$PodaciDetaljno[[1]]$Zupanija

col_classes = c("Date", rep("integer", length(counties)))
col_names = c('Datum', counties)

cumulative_cases <- read.table(text="", colClasses=col_classes, col.names=col_names)

for(d in 1:nrow(data_sorted)) {
  cumulative_cases[nrow(cumulative_cases) + 1,] = data.frame(data_sorted[d, ]$Datum, t(data_sorted[d, ]$PodaciDetaljno[[1]]$broj_umrlih))
}


# # hack to fix wrong data
cumulative_cases[cumulative_cases$Datum == '2021-08-30', c(10)] <- 150
cumulative_cases[cumulative_cases$Datum == '2021-08-30', c(17)] <- 641


cumulative_cases[cumulative_cases$Datum == '2021-09-01', c(5)] <- 2564 # zagreb
cumulative_cases[cumulative_cases$Datum == '2021-09-01', c(21)] <- 255 # zadarska

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
last_date_ <- strftime(percentage_change$Datum[nrow(percentage_change)], "%Y_%m_%d")

n <- 60

data_to_plot <- tail(plot_df, n=n)

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

percentage_change_reordered <- t(percentage_change[nrow(percentage_change), c(14, 1:13, 15:21)])
colnames(percentage_change_reordered)[1] <- 'Tjedna razlika'

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


Ukupno_regija <- hr %>% group_by(Regija) %>% summarise(Ukupno_7d = sum(Ukupno_7d),
                                                       Ukupno_14d = sum(Ukupno_14d),
                                                       Populacija_regija = sum(Populacija))

Ukupno_regija$Ukupno_7d_norm_regija <- Ukupno_regija$Ukupno_7d / Ukupno_regija$Populacija_regija * 100000
Ukupno_regija$Ukupno_14d_norm_regija <- Ukupno_regija$Ukupno_14d / Ukupno_regija$Populacija_regija  * 100000



regije = t(c('Grad Zagreb','Jadranska Hrvatska', 'Panonska Hrvatska','Sjeverna Hrvatska'))

all_values <- data.frame(rbind(regije, Ukupno_regija$Populacija_regija, Ukupno_regija$Ukupno_7d, Ukupno_regija$Ukupno_14d, Ukupno_regija$Ukupno_7d_norm_regija, Ukupno_regija$Ukupno_14d_norm_regija))

write.csv(all_values, paste("data/region_data_csvs_deaths/regions", last_date_, ".csv", sep=""), row.names = FALSE)
