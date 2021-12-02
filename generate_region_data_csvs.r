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

# very poorly written, TODO rewrite

population_by_age <- read.csv(file = 'data/cro_population_by_age.csv')
diff_df <- read_csv('data/latest/diff_df.csv')

load('data/latest/cumulative_cases.Rda')


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

write.csv(all_values, paste("data/region_data_csvs/regions", last_date_, ".csv", sep=""), row.names = FALSE)