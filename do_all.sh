python check_covid_data.py

Rscript get_data.r

Rscript generate_region_data.r


Rscript generate_cases_line_plots.r
Rscript generate_age_plot_lines.r
Rscript generate_cases_hosp_deaths.r

Rscript generate_map_plots.r

Rscript generate_age_plot_moving_dots.r
Rscript generate_age_plot_moving_dots_last_year.r
Rscript generate_age_plot_moving_dots_per_county.r

Rscript generate_age_plot.r
Rscript generate_age_plot_all_data.r

python fit_exp_all.py 
python fit_exp_all_deaths_and_hospitalisations.py

Rscript generate_pyramid_plot.r
Rscript generate_vacc_map.r

Rscript generate_html.r

echo "adding tracking"

python add_tracking.py

echo "tracking added"

