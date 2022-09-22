library(jsonlite)
library(readr)

library(colorspace)
library(gridExtra)

# read last_date
load('data/latest/last_date_.Rda')

# gmt to gmt + 1
generated_time <- format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h')

table_text <- read_file("table.txt")

md_source <- paste('# COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama\n\n',
                   '### (generirano ',  generated_time, ')\n\n',
                   '#### NAPOMENA (04.11.2021.): Stranica se generira automatski. Ako ima grešaka u strojno čitljivim podacima objavljenim na koronavirus.hr, prikazani grafovi neće biti točni. U tom slučaju pokušat ću ih ispraviti čim nađem vremena.\n\n',
                   '#### NAPOMENA (15.01.2022.): Od danas se koriste podaci iz popisa stanovništva 2021. u svim prikazima u kojima je to moguće. U prikazima po dobnim skupinama i dalje se koriste podaci iz 2019. jer detaljniji podaci još nisu dostupni. Promijenjene su skale na nekim grafovima kako bi bili informativniji.\n\n',
                   '#### NAPOMENA (24.01.2022.): Promijenjene su skale na nekim grafovima kako bi bili informativniji.\n\n',
                   '#### NAPOMENA (07.03.2022.): Od danas se koriste podaci iz popisa stanovništva 2021. u svim prikazima.\n\n',
                   '#### NAPOMENA (12.03.2022.): Zbog nedostupnosti detaljnih podataka o individualnim slučajevima na koronavirus.hr neke grafove nemoguće je generirati. Bit će ispravljeno kad i ako ti podaci ponovno budu dostupni.\n\n',
                   '#### NAPOMENA (22.09.2022.): Od danas se koriste konačni podaci iz popisa stanovništva 2021. u svim prikazima (ukupan broj stanovnika 3871833).\n\n',
		   'Neki od prikaza dostupni su i na https://twitter.com/CroatiaCovid odmah nakon generiranja.\n\n',
                   'Interaktivni prikazi dostupni su na sljedećim linkovima:\n\n',
                   '- [Standardni prikaz](html/index.html) (zadnjih 100 dana)\n',
                   '- [Prikaz na logaritamskoj skali](html/index_log.html) (zadnjih 100 dana)\n',
                   '- [Prikaz na karti](html/index_map.html) (tjedna promjena, zadnjih 7 i 14 dana na 100000 stanovnika)\n',
                   '- [Prikaz po dobnim skupinama](html/index_per_age.html) (zadnjih 180 dostupnih dana)\n',
                   '- [Prikaz po dobnim skupinama i spolu](html/index_pyramid.html) (zadnjih 7 dostupnih dana, na 100000 stanovnika)\n',
                   '- [Prikaz procijepljenosti na karti](html/index_vaccination.html) (zadnji dostupni podaci, ne osvježava se automatski)\n\n',
                   '-----\n\n',
                   '## Pregled broja zaraženih po županijama\n\n',
                   '![](img/', last_date_, '_line_plots.png)\n\n',
                   '## Pregled tjedne promjene broja zaraženih po županijama\n\n',
                   '![](img/', last_date_, '_map.png)\n\n',
                   '## Kretanje broja umrlih\n\n',
                   '![](img/', last_date_, '_deaths_shaded.png)\n\n',
                   '## Kretanje broja COVID-19 slučajeva, hospitalizacija i umrlih\n\n(napomena: podaci o hospitalizacijama i broju osoba na respiratorima se ne objavljuju svakodnevno, prikazani su zadnji dostupni podaci)\n\n',
                   '![](img/', last_date_, '_cases_hospitalisations_deaths_perc.png)\n\n',
                   '![](img/', last_date_, '_cases_hospitalisations_deaths_perc_log.png)\n\n',
                   '![](img/', last_date_, '_cases_hospitalisations_deaths_log_age.png)\n\n',
                   
                   '## Kretanje udjela pozitivnih testova\n\n(zadnji dostupni podaci s HZJZ, plavom bojom prikazani su podaci s koronavirus_hr twittera)\n\n',
                   '![](img/', last_date_, '_percentage_positive_tests.png)\n\n',
                   '## Kretanje broja učinjenih testova\n\n(zadnji dostupni podaci s HZJZ, plavom bojom prikazani su podaci s koronavirus_hr twittera)\n\n',
                   '![](img/', last_date_, '_num_tests.png)\n\n',
                   
                   '## Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama\n\n',
                   '![](img/', last_date_, '_cases_per_age_group_lines.png)\n\n',
                   '![](img/', last_date_, '_cases_per_age_group_lines_log.png)\n\n',
                   '(Incidencija po dobnim skupinama u tablici ispod prikazuje samo broj pozitivnih testova iz službenih podataka o testiranjima u odnosu na ukupan broj stanovnika u svakoj dobnoj skupini. Kako je skup podataka službenih testiranja malen, podaci u tablici ne prikazuju pravu sliku (podcijenjuju pravo stanje) pa apsolutne brojeve treba interpretirati pažljivo. Tablica može biti korisna za praćenje trenda kretanja pozitivnih testova po dobnim skupinama.)\n\n',
                   
                   table_text, 
                   
                   '## Animirani prikaz kretanja broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama\n\n',
                   '![](img/', last_date_, 'anim_aug_1200.gif)\n\n',
                   '![](img/anim_cases_', last_date_, '_vs_2020.gif)\n\n',
                   '![](img/', last_date_, 'all_counties_dots.png)\n\n',
                   '## Ukupan broj zaraženih u zadnjih 7 dana na 100000 stanovnika\n\n',
                   '![](img/', last_date_, '_map_7_day_per_100k.png)\n\n',
                   '## Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika\n\n',
                   '![](img/', last_date_, '_map_14_day_per_100k.png)\n\n',
                   '## Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika po regijama\n\n(napomena: kod ECDC-a boja regije ovisi i o postotku pozitivnih testova, ovdje je prikazan samo broj slučajeva)\n\n',
                   '![](img/', last_date_, '_map_14_day_per_100k_region.png)\n\n',
                   '## Ukupan broj zaraženih u zadnjih 7 dana na 100000 stanovnika po dobnim skupinama\n\n',
                   '![](img/', last_date_, '_map_7_day_per_100k_age_groups.png)\n\n',
                   '(Trend kretanja 14-dnevnog broja slučajeva na 100k stanovnika opisan eksponencijalnom krivuljom n(t) = a * e^(b * t) po regijama. Krivulja aproksimira podatke zadnjih 7 dana, izračunate iz podataka objavljenih na koronavirus.hr, koristeći broj stanovnika po županijama iz 2021. s dzs.hr. Krivulja se prikazuje ukoliko je R^2 aproksimacije > 0.9, prikazana je narančastom bojom. Zelena krivulja prikazuje vrijednosti aproksimacijske krivulje 7 dana u budućnosti (deblja linija), dok su tanjom zelenom linijom prikazane vrijednosti krivulje do dana u kojem bi vrijednost mogla doći do praga od 75/200 zaraženih na 100k stanovnika. Generirano automatski, nakon objave službenih podataka na koronavirus.hr.)\n\n',
                   '![](img/', last_date_, '_current_Jadranska_Hrvatska.png)\n\n',
                   '![](img/', last_date_, '_current_Panonska_Hrvatska.png)\n\n',
                   '![](img/', last_date_, '_current_Grad_Zagreb.png)\n\n',
                   '![](img/', last_date_, '_current_Sjeverna_Hrvatska.png)\n\n',
                   '![](img/', last_date_, '_current_Republika_Hrvatska.png)\n\n',
                   '![](img/', last_date_, '_cases_hospitalisations_deaths_Republika_Hrvatska.png)\n\n',
                   '## Procijepljenost po županijama\n\n(ne osvježava se automatski)\n\n',
                   '![](img/', last_date_, '_vaccination.png)\n\n',
                   '## Pregled broja zaraženih po dobnim skupinama na 100000 stanovnika\n\n',
                   '(Podaci kasne par dana, prikazano stanje nije finalno.)\n\n',
                   '![](img/', last_date_, '_per_age_group.png)\n\n',
                   '![](img/', last_date_, '_per_age_group_all_0.png)\n\n',
                   '![](img/', last_date_, '_per_age_group_all_1.png)\n\n',
                   '## Pregled broja zaraženih po dobnim skupinama i spolu (u zadnjih 7 dostupnih dana) na 100000 stanovnika\n\n',
                   '(Podaci kasne par dana, prikazano stanje nije finalno.)\n\n',
                   '![](img/', last_date_, '_pyramid.png)\n\n',
                   '-----\n\n',
                   '- [Kod](https://github.com/ppalasek/covid_plots_croatia)\n', sep='')

writeLines(md_source, 'index.md')
