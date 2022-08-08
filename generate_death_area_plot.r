library(tidyverse)
library(jsonlite)
library(lubridate)
library(zoo) 
library(scales)


library(ggplot2)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

# get data from owid
data <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')  %>%
  filter(location %in% "Croatia")

data$date <- as.Date(data$date, format="%Y-%m-%d")

# get last_date_
load('data/latest/last_date_.Rda')

print(last_date_)

last_date_owid_ = format(as.Date(data$date[nrow(data)] -as.difftime(1, units="days"), format="%Y-%m-%d"), format="%Y_%m_%d")


# read latest data we fetched from koronavirus.hr
diff_deaths_df <- read_csv('data/latest/diff_deaths_df.csv')
print(diff_deaths_df[nrow(diff_deaths_df), c('Hrvatska', 'Datum')])

if (last_date_owid_ != last_date_) {
  new_date = as.Date(last_date_, format="%Y_%m_%d") + as.difftime(1, units="days")
  
  new_deaths <- c(diff_deaths_df[nrow(diff_deaths_df), c('Hrvatska')])
  
  new_deaths
  
  df2 <- data.frame(date=c(new_date),
                    new_deaths=c(new_deaths))

  colnames(df2)[2] <- "new_deaths"
  
  data <- dplyr::bind_rows(data, df2)
  
} else {
  print('last dates same.')
}

# fix
data[data$date=="2022-03-31", "new_deaths"] <- 12
data[data$date=="2022-04-01", "new_deaths"] <- 9


merged_data <- data %>%
  mutate(new_deaths_7da = zoo::rollmean(new_deaths, k = 7, fill = NA, align = "right"))



merged_data$new_deaths[is.na(merged_data$new_deaths)] <- 0

merged_data <- within(merged_data, cum_deaths <- Reduce("+", new_deaths, accumulate = TRUE))

merged_data$cum_deaths_1k <- merged_data$cum_deaths %/% 1000
merged_data$cum_deaths_1k_alt <- merged_data$cum_deaths_1k %% 2

merged_data <- transform(merged_data, cum_deaths_1k_colour = ifelse(cum_deaths_1k_alt==1, 'white', 'red'))


print(merged_data$cum_deaths[nrow(merged_data)])
total_deaths <- merged_data$cum_deaths[nrow(merged_data)]

last_count <- total_deaths %% 1000

print(last_count)

col_1 <- 'gray'
col_2 <- 'gray'
col_3 <- 'pink'
alpha_val <- 0.5
alpha_val2 <- 0.15

g3 <- ggplot(data=merged_data, aes_string(x='date', y="new_deaths_7da")) + 
  geom_point(data=merged_data, aes_string(x='date', y="new_deaths"), colour="black", alpha=0.2) +
  geom_line(size=1.5, alpha=0.7) # +
  
for (i in seq(0, merged_data$cum_deaths[nrow(merged_data)] %/% 1000 - 1)) {
  print(i)
  if (i %% 2 == 0) {
    col = col_1
    alpha = alpha_val
    alpha_last = alpha_val2
  }
  else {
    col = col_2
    alpha = alpha_val2
    alpha_last = alpha_val
  }
  g3 <- g3 + geom_ribbon(data = filter(merged_data, cum_deaths_1k %in% c(i)), aes_string(x='date', ymin="0", ymax="new_deaths_7da"), fill=col, alpha=alpha)
}

g3 <- g3 + geom_ribbon(data = filter(merged_data, cum_deaths_1k %in% c(i + 1)), aes_string(x='date', ymin="0", ymax="new_deaths_7da"), fill=col_3, alpha=alpha_last)
g3 <- g3 +  
  ylab('Broj umrlih') +
  xlab('Datum') + 
  ggtitle(paste('COVID-19: Kretanje broja umrlih u Hrvatskoj (sedmodnevni prosjek). Svaki prikazani segment ispod krivulje predstavlja 1000 osoba (ukupno: ', total_deaths,').', sep='')) +
  scale_x_date(labels = date_format("%b %Y."), date_breaks = "1 month") +
  theme_minimal() +
  theme(text = element_text(size=18)) +
  labs(caption = paste('Izvori podataka: ourworldindata.com, twitter.com/koronavirus_hr. Generirano:', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h.'), 'Autor: Petar Palašek, ppalasek.github.io')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

g3

ggsave(paste('img/', last_date_, '_deaths_shaded.png', sep = ''),
       plot = g3, dpi=300, width=1600*4, height=700*4, units="px",
       bg = "white")
