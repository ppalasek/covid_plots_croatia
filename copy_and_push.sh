mv ../ppalasek.github.io/img/*.png ../ppalasek.github.io/old_img/
mv ../ppalasek.github.io/img/*.gif ../ppalasek.github.io/old_img/

cp img/*map.png ../ppalasek.github.io/img/
cp img/*deaths_shaded.png ../ppalasek.github.io/img/
cp img/*map_7_day_per_100k.png ../ppalasek.github.io/img/
cp img/*per_age_group.png ../ppalasek.github.io/img/
cp img/*map_14_day_per_100k.png ../ppalasek.github.io/img/
cp img/*map_14_day_per_100k_region.png ../ppalasek.github.io/img/
cp img/*_map_7_day_per_100k_age_groups.png ../ppalasek.github.io/img/
cp img/*pyramid.png ../ppalasek.github.io/img/
cp img/*vaccination.png ../ppalasek.github.io/img/
cp img/*current*.png ../ppalasek.github.io/img/
cp img/*line_plots.png ../ppalasek.github.io/img/
cp img/*cases_hospitalisations_deaths*.png ../ppalasek.github.io/img/
cp img/*_per_age_group_all_0.png ../ppalasek.github.io/img/
cp img/*_per_age_group_all_1.png ../ppalasek.github.io/img/

cp img/*cases_hospitalisations_deaths.png ../ppalasek.github.io/img/
cp img/*cases_per_age_group_lines*.png ../ppalasek.github.io/img/

cp img/*anim*.gif ../ppalasek.github.io/img/
cp img/*percentage*.png ../ppalasek.github.io/img/
cp img/*num_tests.png ../ppalasek.github.io/img/
cp img/*all_counties_dots.png ../ppalasek.github.io/img/

cp html/index.html ../ppalasek.github.io/html/
cp html/index_log.html ../ppalasek.github.io/html/
cp html/index_map.html ../ppalasek.github.io/html/
cp html/index_per_age.html ../ppalasek.github.io/html/
cp html/index_pyramid.html ../ppalasek.github.io/html/
cp html/index_vaccination.html ../ppalasek.github.io/html/

cp index.md ../ppalasek.github.io/

cd ../ppalasek.github.io/

git add html/*.html img/*.png img/*.gif old_img/*.png old_img/*.gif index.md
git commit -m "automagic update"
git push
