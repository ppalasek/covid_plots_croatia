# wait for tweet and publish percentage plot
python wait_for_tweet.py

# wait for all data
python check_covid_data.py

# check if there were holidays in the past 2 weeks and generate a note
python check_holidays.py

mv img/*.png old_img/
mv img/*.gif old_img/

echo "get data"
Rscript get_data.r

echo "generate region data"
Rscript generate_region_data.r

echo "generate cases line plots"
Rscript generate_cases_line_plots.r

# read -n 1 -p "Continue? (ctrl+c to stop)"

python publish_tweet.py 'img/{}_line_plots.png' 'Pregled broja slučajeva po županijama. [HOLIDAY_NOTE]'

echo "generate death area plot"
Rscript generate_death_area_plot.r

#read -n 1 -p "Continue? (ctrl+c to stop)"
convert -append img/*_deaths_shaded.png img/*_deaths_shaded_log.png img/deaths_stacked.png
#python publish_tweet.py 'img/deaths_stacked.png' 'Kretanje broja umrlih u Hrvatskoj (sedmodnevni prosjek, linearna/logaritamska skala).'

python publish_tweet.py 'img/deaths_stacked.png' 'Kretanje broja umrlih u Hrvatskoj (sedmodnevni prosjek, linearna/logaritamska skala). Broj umrlih u zadnjih 24 sata: [DEATHS_24H], u zadnjih 7 dana: [DEATHS_7D], u zadnjih 30 dana: [DEATHS_30D], ukupno: [DEATHS_TOTAL].'

echo "generate map plots"
Rscript generate_map_plots.r

python publish_tweet.py 'img/{}_map_7_day_per_100k.png' 'Sedmodnevna incidencija po županijama.'
python publish_tweet.py 'img/{}_map_14_day_per_100k.png' 'Dvotjedna incidencija po županijama.'
python publish_tweet.py 'img/{}_map.png' 'Pregled tjedne promjene broja zaraženih po županijama. [HOLIDAY_NOTE]'

echo "generate age plot lines"
Rscript generate_age_plot_lines.r

cd img
# convert *_cases_per_age_group_lines*.png -resize 6400x6400 -gravity center -background "rgb(255,255,255)" -extent 6400x6400 -set filename:base "%[basename]" "%[filename:base]_twitter.png"
convert -append *_cases_per_age_group_lines*.png age_group_lines_stacked.png
cd ..


python publish_tweet.py 'img/age_group_lines_stacked.png' 'Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po dobnim skupinama u Hrvatskoj (linearna/logaritamska skala).'


echo "generate cases hosp deaths log"
Rscript generate_cases_hosp_deaths_log.r

# python publish_tweet.py 'img/{}_cases_hospitalisations_deaths_log.png' 'Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika (plavo), hospitaliziranih na 1 milijun stanovnika (crveno), osoba na respiratoru (zeleno) i umrlih (crno) u Hrvatskoj (sedmodnevni prosjek, logaritamska skala).'


Rscript generate_cases_hosp_deaths_perc.r

cd img
#convert *_cases_hospitalisations_deaths_perc*.png -resize 6400x6400 -gravity center -background "rgb(255,255,255)" -extent 6400x6400 -set filename:base "%[basename]" "%[filename:base]_twitter.png"
convert -append *_cases_hospitalisations_deaths_perc*.png cases_hospitalisations_deaths_perc_stacked.png

cd ..

# python publish_tweet.py 'img/{}_cases_hospitalisations_deaths_perc_twitter.png' 'img/{}_cases_hospitalisations_deaths_perc_log_twitter.png' 'Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika (plavo), hospitaliziranih na 1 milijun stanovnika (crveno), osoba na respiratoru (zeleno), umrlih (crno) i udjela pozitivnih testova (narančasto) u Hrvatskoj (sedmodnevni prosjek).'
python publish_tweet.py 'img/cases_hospitalisations_deaths_perc_stacked.png' 'Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika (plavo), hospitaliziranih na 1 milijun stanovnika (crveno), osoba na respiratoru (zeleno), umrlih (crno) i udjela pozitivnih testova (narančasto) u Hrvatskoj (sedmodnevni prosjek).'



echo "generate cases hosp deaths"
Rscript generate_cases_hosp_deaths.r


echo "fit exp all deaths and hospitalisations"
python fit_exp_all_deaths_and_hospitalisations.py

python publish_tweet.py 'img/{}_cases_hospitalisations_deaths_Republika_Hrvatska.png' 'Kretanje broja COVID-19 slučajeva/hospitalizacija/umrlih i procijenjeni periodi udvostručavanja/prepolavljanja.'


echo "generate cases hosp deaths log age"
Rscript generate_cases_hosp_deaths_log_age.r



echo "generate age plot moving dots per county"
Rscript generate_age_plot_moving_dots_per_county.r

python publish_tweet.py 'img/{}all_counties_dots.png' 'Kretanje broja COVID-19 slučajeva na 100 tisuća stanovnika po županijama i dobnim skupinama u Hrvatskoj.'



echo "geneate age plot"
Rscript generate_age_plot.r

echo "generate age plot all data"
Rscript generate_age_plot_all_data.r

python publish_tweet.py 'img/{}_per_age_group_all_0.png' 'Pregled broja zaraženih po dobnim skupinama.'


echo "generate generate_age_group_map_plot_inferno_all_groups"
Rscript generate_age_group_map_plot_inferno_all_groups.r

python publish_tweet.py 'img/{}_map_7_day_per_100k_age_groups.png' 'Ukupan broj slučajeva po dobnim skupinama u prethodnih 7 dana na 100000 stanovnika.'




echo "generate vacc map"
Rscript generate_vacc_map.r

echo "generate age plot moving dots"
Rscript generate_age_plot_moving_dots.r

echo "generate age plot moving dots last year"
Rscript generate_age_plot_moving_dots_last_year.r


python publish_tweet.py "img/anim_cases_{}_vs_2020.gif" 'Animirano kretanje broja slučajeva na 100000 stanovnika po dobnim skupinama u prethodnih 2 mjeseca, u usporedbi s istim periodom pred godinu dana.'



echo "fit exp all"
python fit_exp_all.py

echo "generate pyrmaid plot"
Rscript generate_pyramid_plot.r

echo "generate html"
Rscript generate_html.r

echo "adding tracking"

python add_tracking.py

echo "tracking added"

