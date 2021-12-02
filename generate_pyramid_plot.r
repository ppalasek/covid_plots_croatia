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

# get last_date_
load('data/latest/last_date_.Rda')

# get age data
json_data <- fromJSON('data/latest/last_data_po_osobama.json')

step <- 5
n <- 60


json_data <- json_data %>%  mutate(dob = ifelse(dob < 1901, NA, dob))

json_data$Datum <- as.Date(json_data$Datum, format="%Y-%m-%d")

json_data <- json_data[order(json_data$Datum),]


last_date <- strftime(json_data$Datum[nrow(json_data)], "%d.%m.%Y.")


json_data$dob <- as.Date(paste0(json_data$dob, '-01-01'), format="%Y-%m-%d")

json_data$age <- round(time_length(difftime(json_data$Datum, json_data$dob), "years"))



json_data$age_group <- findInterval(json_data$age, seq(0, 85, by=step))

labels <- paste0(seq(0, 85, by=step), '-', seq(step - 1, 85, by=step), sep='')

labels[length(labels)] = '85+'


json_data <- json_data %>% drop_na('Datum')


json_d2 <- json_data %>% group_by(Datum, age_group, spol, Zupanija) %>% tally()


age_reshaped2 <- pivot_wider(json_d2, names_from = age_group, values_from = n) %>%
  mutate_at(vars(-("Datum")), ~replace(., is.na(.), 0))

age_reshaped3 <- age_reshaped2[c('Datum', 'Zupanija', 'spol', as.character(1:length(labels)))]

colnames(age_reshaped3)[4:(3+length(labels))] <- labels

max_date = max(age_reshaped3$Datum) - days(2)
two_weeks_ago  = max_date - days(6)


counties <- unique(json_data[c("Zupanija")])


population_by_age_and_sex <- read.csv(file = 'data/cro_population_by_age_and_sex.csv')

f <- list(size = 13, color = "black")

i <- 0 

p <- list()
a <- list()

for(county in c(sort(counties$Zupanija), 'Hrvatska')) {
  if (county == '') {
    next
  }
  
  i <- i + 1

  current_population <-c(population_by_age_and_sex[1:nrow(population_by_age_and_sex) - 1, c(i + 1)])
  
  current_population_M <-c(population_by_age_and_sex[1:nrow(population_by_age_and_sex) - 1, c((i + 1)) + 22])
  current_population_Z <-c(population_by_age_and_sex[1:nrow(population_by_age_and_sex) - 1, c((i + 1)) + 44])
  
  if (county == 'Hrvatska') {
    print(subset(age_reshaped3, (spol=='M') & (Datum >= two_weeks_ago)))
    total_nums_M <- as.data.frame(t(colSums(subset(age_reshaped3, (spol=='M') & (Datum >= two_weeks_ago))[, -c(1, 2, 3)])))
    total_nums_F <- as.data.frame(t(colSums(subset(age_reshaped3, (spol=='Ž') & (Datum >= two_weeks_ago))[, -c(1, 2, 3)])))
  }
  else {
    total_nums_M <- as.data.frame(t(colSums(subset(age_reshaped3, (spol=='M') & (Zupanija == county) & (Datum >= two_weeks_ago))[, -c(1, 2, 3)])))
    total_nums_F <- as.data.frame(t(colSums(subset(age_reshaped3, (spol=='Ž') & (Zupanija == county) & (Datum >= two_weeks_ago))[, -c(1, 2, 3)])))
  }
  
  total_nums <- sum(total_nums_M + total_nums_F)
  
  total_nums_M <- total_nums_M * -1
  
  m_melt <- melt(total_nums_M, value.name='Broj_slučajeva', variable.name = 'Dobna_skupina')
  f_melt <- melt(total_nums_F, value.name='Broj_slučajeva', variable.name = 'Dobna_skupina')
  
  m_melt$Spol <- 'M'
  f_melt$Spol <- 'Ž'
  
  m_melt$Populacija <- current_population_M
  f_melt$Populacija <- current_population_Z
  
  total_population <- sum(current_population_M + current_population_Z)
  
  m_melt$Incidencija_7d_na_100k <- (m_melt$Broj_slučajeva / m_melt$Populacija) * 100000 
  f_melt$Incidencija_7d_na_100k <- (f_melt$Broj_slučajeva / f_melt$Populacija) * 100000

  total_7d_na_100k <- (total_nums / total_population) * 100000
  
  max_inc <- max(c(max(abs(m_melt$Incidencija_7d_na_100k)), max(f_melt$Incidencija_7d_na_100k)))
  
  max_inc <- max(c(300, (max_inc %/% 50 + 1) * 50))
  
  data <- rbind(m_melt, f_melt)
  
  data$pos <- ifelse(data$Broj_slučajeva < 0, 0, -20)
  

  g1 <- ggplot(data, aes(x = Dobna_skupina, y = Incidencija_7d_na_100k, fill= Spol,
                         text=paste('Broj slučajeva:', abs(Broj_slučajeva), '\nPopulacija:', Populacija))) +
    geom_bar(stat = "identity")+
    coord_flip(ylim = c(-max_inc, max_inc)) +
    scale_fill_brewer(palette = 'Set2', direction = -1)  +
    labs(x = 'Dobna skupina') +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(labels = abs)
    
  
  g1
  
  
  
  p[[i]] <- ggplotly(g1)
  
  a[[i]] <- list(
    text = paste(county, round(total_7d_na_100k, digits=2), '/100k'),
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

last_date <- paste(day(max_date), month(max_date), year(max_date), sep='.')
from_date <- paste(day(two_weeks_ago), month(two_weeks_ago), year(two_weeks_ago), sep='.')


s <- subplot(p, nrows = 5, margin=c(0.02), titleY = TRUE)%>%
  add_annotations(x = 0.7,
                  y = 0.07,
                  text = paste('COVID 19 u Hrvatskoj: Pregled broja zaraženih po dobnim skupinama i spolu (', from_date, ' - ', last_date ,')\n\nBroj zaraženih u zadnjih dostupnih 7 dana, normalizirano na 100000 stanovnika u svakoj dobnoj skupini.\nMuškarci su prikazani narančastom bojom na lijevoj strani, žene zelenom bojom na desnoj.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), '\nIzvor podataka: koronavirus.hr (broj slučajeva), dzs.hr (broj stanovnika po dobnim skupinama i spolu, podaci iz 2019.)\n\nAutor: Petar Palašek', sep=''),
                  font = f,
                  xref = "paper",
                  yref = "paper",
                  align='left',
                  ax = 0,
                  ay = 0)

s

orca(s, file = paste('img/', last_date_, '_pyramid.png', sep = ''), width = 27 * 72, height = 13 * 72)


saveWidget(s, file = "html/index_pyramid.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po dobnim skupinama i spolu (", last_date, ")", sep=""))

