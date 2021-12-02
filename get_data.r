library(jsonlite)
library(readr)
library(zoo) 

json_data <- fromJSON('https://www.koronavirus.hr/json/?action=po_danima_zupanijama')

write(toJSON(json_data), 'data/latest/last_data_po_danima_zupanijama.json')
write(toJSON(json_data), paste('data/per_day/po_danima_zupanijama_', format(Sys.time(), '%Y_%m_%d'), '.json', sep=''))

json_data <- json_data[, c("Datum", "PodaciDetaljno")]

# drop newly added, previously missing data as we have it stored in data/missing_data.json and there were issues with the new data
json_data<-json_data[!json_data$Datum=="2021-11-02 00:00", ]
json_data<-json_data[!json_data$Datum=="2021-11-08 00:00", ]
json_data<-json_data[!json_data$Datum=="2021-11-10 00:00", ]
json_data<-json_data[!json_data$Datum=="2021-11-15 00:00", ]
json_data<-json_data[!json_data$Datum=="2021-11-18 00:00", ]

json_data <- unique(json_data, by = "Datum")

# not all info is correct in the missing_data.json (e.g. active cases is wrong for sure)
missing_data <- fromJSON('data/missing_data.json')

json_data$Datum <- as.Date(json_data$Datum, format="%Y-%m-%d")
missing_data$Datum <- as.Date(missing_data$Datum, format="%Y-%m-%d")

# add missing data
json_data <- rbind(json_data, missing_data)

# sort by date
data_sorted <- json_data[order(json_data$Datum),]

# all counties
counties <- data_sorted[1, ]$PodaciDetaljno[[1]]$Zupanija

col_classes = c("Date", rep("integer", length(counties)))
col_names = c('Datum', counties)

cumulative_cases <- read.table(text="", colClasses=col_classes, col.names=col_names)

for(d in 1:nrow(data_sorted)) {
  cumulative_cases[nrow(cumulative_cases) + 1,] = data.frame(data_sorted[d, ]$Datum, t(data_sorted[d, ]$PodaciDetaljno[[1]]$broj_zarazenih))
}

# sum all counties data to get total for Croatia
cumulative_cases$Hrvatska <- rowSums(cumulative_cases[,-c(1)])

# hack to fix wrong data caused by some issues in the koronavirus.hr json data
true_d = c(27, 24, 19, 166, 14, 15, 17, 17, 11, 7, 62, 8, 26, 2, 14, 221, 16, 4, 29, 43, 63, 805)
reported_d = c(28, 27, 21, 167, 10, 3, 19, 22, 14, 9, 61, 4, 23, 0, -3, 230, 10, 4, 24, 46, 86, 805)

diff_to_add = true_d - reported_d
cumulative_cases[cumulative_cases$Datum >= '2021-09-01', -c(1)] <- cumulative_cases[cumulative_cases$Datum >= '2021-09-01', -c(1)] + rep(diff_to_add, each = nrow( cumulative_cases[cumulative_cases$Datum >= '2021-09-01', ]))

diff_df <- cumulative_cases[-1, -c(1)] - cumulative_cases[-nrow(cumulative_cases), -c(1)]
diff_df$Datum <- cumulative_cases$Datum[-c(nrow(cumulative_cases))]

# more issues
diff_df[diff_df$Datum=='2021-09-24', c(4, 9, 13)] <- c(364, 0, 59)

# save diff data frame to data/latest/
# this will contain numbers of cases per day per county and the total
write.csv(diff_df, 'data/latest/diff_df.csv', row.names=FALSE, quote=FALSE) 




cumulative_deaths <- read.table(text="", colClasses=col_classes, col.names=col_names)

for(d in 1:nrow(data_sorted)) {
  cumulative_deaths[nrow(cumulative_deaths) + 1,] = data.frame(data_sorted[d, ]$Datum, t(data_sorted[d, ]$PodaciDetaljno[[1]]$broj_umrlih))
}


# # hack to fix wrong data
cumulative_deaths[cumulative_deaths$Datum == '2021-08-30', c(10)] <- 150
cumulative_deaths[cumulative_deaths$Datum == '2021-08-30', c(17)] <- 641


cumulative_deaths[cumulative_deaths$Datum == '2021-09-01', c(5)] <- 2564 # zagreb
cumulative_deaths[cumulative_deaths$Datum == '2021-09-01', c(21)] <- 255 # zadarska

cumulative_deaths$Hrvatska <- rowSums(cumulative_deaths[,-c(1)])


diff_deaths_df <- cumulative_deaths[-1, -c(1)] - cumulative_deaths[-nrow(cumulative_deaths), -c(1)]

diff_deaths_df$Datum <- cumulative_deaths$Datum[-c(nrow(cumulative_deaths))]

# save diff data frame to data/latest/
# this will contain numbers of cases per day per county and the total
write.csv(diff_deaths_df, 'data/latest/diff_deaths_df.csv', row.names=FALSE, quote=FALSE) 




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
# 
# write.csv(cumulative_cases, 'data/latest/cumulative_cases.csv', row.names=FALSE, quote=FALSE) 
# write.csv(avg7_df, 'data/latest/avg7_df.csv', row.names=FALSE, quote=FALSE) 

colnames(percentage_change) <- paste("tjedna_razlika", colnames(percentage_change), sep = "_")

percentage_change$Datum <- cumulative_cases$Datum[c(8:nrow(cumulative_cases) - 1)]

save(percentage_change, file='data/latest/percentage_change.Rda')
save(cumulative_cases, file='data/latest/cumulative_cases.Rda')
save(avg7_df, file='data/latest/avg7_df.Rda')
save(sum_7_df, file='data/latest/sum_7_df.Rda')
save(sum_14_df, file='data/latest/sum_14_df.Rda')


last_date <- strftime(percentage_change$Datum[nrow(percentage_change)], "%d.%m.%Y.")
last_date_ <- strftime(percentage_change$Datum[nrow(percentage_change)], "%Y_%m_%d")

save(last_date, file='data/latest/last_date.Rda')
save(last_date_, file='data/latest/last_date_.Rda')



# get age data
json_data <- fromJSON('https://www.koronavirus.hr/json/?action=po_osobama')
write(toJSON(json_data), 'data/latest/last_data_po_osobama.json')
