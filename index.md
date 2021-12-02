# COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama

### (generirano 02.12.2021. 11:10:07 h)

### NAPOMENA (04.11.2021.): Stranica se generira automatski. Ako ima grešaka u strojno čitljivim podacima objavljenim na koronavirus.hr, prikazani grafovi neće biti točni. U tom slučaju pokušat ću ih ispraviti čim nađem vremena.

Interaktivni prikazi dostupni su na sljedećim linkovima:

- [Standardni prikaz](html/index.html) (zadnjih 100 dana)
- [Prikaz na logaritamskoj skali](html/index_log.html) (zadnjih 100 dana)
- [Prikaz na karti](html/index_map.html) (tjedna promjena, zadnjih 7 i 14 dana na 100000 stanovnika)
- [Prikaz po dobnim skupinama](html/index_per_age.html) (zadnjih 180 dostupnih dana)
- [Prikaz po dobnim skupinama i spolu](html/index_pyramid.html) (zadnjih 7 dostupnih dana, na 100000 stanovnika)
- [Prikaz procijepljenosti na karti](html/index_vaccination.html) (zadnji dostupni podaci, ne osvježava se automatski)

-----

## Pregled broja zaraženih po županijama

![](img/2021_11_30_line_plots.png)

## Pregled tjedne promjene broja zaraženih po županijama

![](img/2021_11_30_map.png)

## Kretanje broja COVID-19 slučajeva, hospitalizacija i umrlih

![](img/2021_11_30_cases_hospitalisations_deaths.png)

## Kretanje udjela pozitivnih testova

![](img/2021_11_30_percentage_positive_tests.png)

## Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama

![](img/2021_11_30_cases_per_age_group_lines.png)

## Animirani prikaz kretanja broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama

![](img/2021_11_30anim_aug_1200.gif)

![](img/anim_cases_2021_11_30_vs_2020.gif)

![](img/2021_11_30all_counties_dots.png)

## Ukupan broj zaraženih u zadnjih 7 dana na 100000 stanovnika

![](img/2021_11_30_map_7_day_per_100k.png)

## Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika

![](img/2021_11_30_map_14_day_per_100k.png)

## Ukupan broj zaraženih u zadnjih 14 dana na 100000 stanovnika po regijama

(napomena: kod ECDC-a boja regije ovisi i o postotku pozitivnih testova, ovdje je prikazan samo broj slučajeva)

![](img/2021_11_30_map_14_day_per_100k_region.png)

(Trend kretanja 14-dnevnog broja slučajeva na 100k stanovnika opisan eksponencijalnom krivuljom n(t) = a * e^(b * t) po regijama. Krivulja aproksimira podatke zadnjih 7 dana, izračunate iz podataka objavljenih na koronavirus.hr, koristeći broj stanovnika po županijama iz 2019. s dzs.hr. Krivulja se prikazuje ukoliko je R^2 aproksimacije > 0.95, prikazana je narančastom bojom. Zelena krivulja prikazuje vrijednosti aproksimacijske krivulje 7 dana u budućnosti (deblja linija), dok su tanjom zelenom linijom prikazane vrijednosti krivulje do dana u kojem bi vrijednost mogla doći do praga od 75/200 zaraženih na 100k stanovnika. Generirano automatski, nakon objave službenih podataka na koronavirus.hr.)

![](img/2021_11_30_current_Jadranska_Hrvatska.png)

![](img/2021_11_30_current_Panonska_Hrvatska.png)

![](img/2021_11_30_current_Grad_Zagreb.png)

![](img/2021_11_30_current_Sjeverna_Hrvatska.png)

![](img/2021_11_30_current_Republika_Hrvatska.png)

![](img/2021_11_30_cases_hospitalisations_deaths_Republika_Hrvatska.png)

## Procijepljenost po županijama

(ne osvježava se automatski)

![](img/2021_11_30_vaccination.png)

## Pregled broja zaraženih po dobnim skupinama na 100000 stanovnika

(Podaci kasne par dana, prikazano stanje nije finalno.)

![](img/2021_11_30_per_age_group.png)

![](img/2021_11_30_per_age_group_all_0.png)

![](img/2021_11_30_per_age_group_all_1.png)

## Pregled broja zaraženih po dobnim skupinama i spolu (u zadnjih 7 dostupnih dana) na 100000 stanovnika

(Podaci kasne par dana, prikazano stanje nije finalno.)

![](img/2021_11_30_pyramid.png)

-----

- [Kod](https://github.com/ppalasek/covid_plots_croatia)

