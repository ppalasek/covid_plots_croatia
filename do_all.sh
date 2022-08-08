python check_covid_data.py

mv img/*.png old_img/
mv img/*.gif old_img/

echo "get data"
Rscript get_data.r

echo "generate region data"
Rscript generate_region_data.r

echo "generate death area plot"

Rscript generate_death_area_plot.r

echo "generate cases line plots"
Rscript generate_cases_line_plots.r

echo "generate age plot lines"
Rscript generate_age_plot_lines.r

echo "generate cases hosp deaths"

Rscript generate_cases_hosp_deaths.r

echo "generate cases hosp deaths log"
Rscript generate_cases_hosp_deaths_log.r

echo "generate cases hosp deaths log age"
Rscript generate_cases_hosp_deaths_log_age.r


echo "generate vacc map"
Rscript generate_vacc_map.r

echo "generate map plots"
Rscript generate_map_plots.r

echo "generate age plot moving dots"
Rscript generate_age_plot_moving_dots.r

echo "generate age plot moving dots last year"
Rscript generate_age_plot_moving_dots_last_year.r

echo "geneate age plot"
Rscript generate_age_plot.r

echo "generate age plot all data"
Rscript generate_age_plot_all_data.r

echo "fit exp all"
python fit_exp_all.py

echo "fit exp all deaths and hospitalisations"
python fit_exp_all_deaths_and_hospitalisations.py


echo "generate pyrmaid plot"
Rscript generate_pyramid_plot.r

echo "generate age plot moving dots per county"

Rscript generate_age_plot_moving_dots_per_county.r

echo "generate html"
Rscript generate_html.r

echo "adding tracking"

python add_tracking.py

echo "tracking added"

