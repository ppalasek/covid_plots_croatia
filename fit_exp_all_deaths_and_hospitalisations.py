import numpy as np
from scipy.optimize import curve_fit

#%matplotlib inline
import matplotlib.pyplot as plt

from matplotlib.ticker import ScalarFormatter
import matplotlib
import pandas as pd
from pandas import read_csv
from pathlib import Path
import datetime

def curve_model(t, a, k):
    # exp curve
    return a * np.exp(k * t)


font = {'size': 35}
matplotlib.rc('font', **font)


r2_threshold = 0.95

# num points to use for fitting 
n_for_fitting = 7# 7

# num future points
n_fut = 7 #7

# num points to plot
n_to_plot = 150

# cvs paths
csv_path = {}

csv_path['cases'] = 'data/latest/region_diff_cases_df.csv'
csv_path['deaths'] = 'data/latest/region_diff_deaths_df.csv'

all_vals = {}

all_vals['cases'] = []
all_vals['deaths'] = []
all_vals['hospitalisations'] = []


modalities = ['cases', 'deaths']

for modality in modalities:
    current_df = read_csv(csv_path[modality])

    for row_index in range(len(current_df)):
        vals = {}
        total = {}

        current_row = current_df.iloc[row_index]

        vals['date'] = datetime.datetime.strptime(current_row['Datum'], "%Y-%m-%d").date()

        for region_index, region in enumerate(['Grad Zagreb', 'Jadranska Hrvatska', 'Panonska Hrvatska', 'Sjeverna Hrvatska']):
            vals[region] = {}

            for i, name in enumerate(['pop', '7d', '14d', '7d_norm', '14d_norm']):

                if 'norm' in name:
                    v = float(current_row.loc['{}.{}'.format(region.replace(' ', '.'), name[:-len('_norm')])]) / float(current_row.loc['{}.pop'.format(region.replace(' ', '.'))]) * 100000
                else:
                    v = float(current_row.loc['{}.{}'.format(region.replace(' ', '.'), name)])

                if region_index == 0:
                    total[name] = v
                else:
                    total[name] += v

                vals[region][name] = v


        total['7d_norm'] = (total['7d'] / total['pop']) * 100000
        total['14d_norm'] = (total['14d'] / total['pop']) * 100000

        vals['Republika Hrvatska'] = total


        all_vals[modality].append(vals)



print(all_vals['cases'])

print(all_vals['cases'][-1])


last_date_cases = all_vals['cases'][-1]['date']


hosp_data = read_csv('data/latest/last_hzjz_data.csv')



n_days = 14

assert n_days in [7, 14]

hosp_data.columns = [c.replace(' ', '_') for c in hosp_data.columns]


hosp_data = hosp_data.sort_values(by='datum', key=lambda x:pd.to_datetime(x, format='%d/%m/%Y'), ascending=False)

dates = hosp_data.datum.to_list()

print('dates')
print(dates)

last_date_hosp = datetime.datetime.strptime(dates[0], "%d/%m/%Y").date()

print(last_date_cases, last_date_hosp)
diff_dates = last_date_cases - last_date_hosp


hosp_patients = hosp_data.hospitalizirani_u_zadnja_24_sata.to_list()

print(n_to_plot - diff_dates.days)

for i in range(n_to_plot - diff_dates.days):
    patients_14_d = sum(hosp_patients[i : i + n_days])

    patients_14_d = patients_14_d / 4058165 * 1000000

    curr_date = datetime.datetime.strptime(dates[i], "%d/%m/%Y").date()

    print(curr_date)
    print('patients', patients_14_d)
    print()

    vals = {}

    vals['date'] = curr_date
    vals['Republika Hrvatska'] = {'{}d'.format(n_days) : patients_14_d}

    all_vals['hospitalisations'].append(vals)


for x in all_vals['deaths']:
    print(x['date'], x['Republika Hrvatska']['{}d'.format(n_days)])


# 

print(len(all_vals['hospitalisations']))
print(all_vals['hospitalisations'][0])


modalities.append('hospitalisations')

all_vals['hospitalisations'] = all_vals['hospitalisations'][::-1]


last_date = {}

formatter = ScalarFormatter()
formatter.set_scientific(False)


n_plots = 2

for region in all_vals['cases'][0].keys():
    if region == 'date':
        continue

    if region != 'Republika Hrvatska':
        continue

    fig, axs = plt.subplots(1, n_plots, figsize=(60, 30))

    axs[0].set_yscale('symlog', basey=2)
    axs[0].yaxis.set_major_formatter(formatter)

    y_vals = {}
    x_for_fit, y_for_fit = {}, {}

    p_opt, p_cov = {}, {}

    doubling_time, tau, eerror, doubling_time_error = {}, {}, {}, {}
    resid, ss_tot, ss_res, r2 = {}, {}, {}, {}
    

    for modality in modalities:
        if modality == 'deaths':
            y_vals[modality] = [x[region]['{}d'.format(n_days)] for x in all_vals[modality]][-n_to_plot:]

            modality_name = 'umrlih'
        elif modality == 'hospitalisations':
            y_vals[modality] = [x[region]['{}d'.format(n_days)] for x in all_vals[modality]][-n_to_plot:]

            modality_name = 'hospitaliziranih'
        else:
            y_vals[modality] = [x[region]['{}d_norm'.format(n_days)] for x in all_vals[modality]][-n_to_plot:]

            modality_name = 'slučajeva'

        dates = [x['date'].strftime("%d.%m.") for x in all_vals[modality]][-n_to_plot:]

        print(dates)

        y = np.asarray(y_vals[modality])
        x = np.asarray(list(range(len(y))))
    
        if modality == 'hospitalisations':
            x = np.asarray(list(range(len(y))))

        first_date = dates[-n_for_fitting]
        last_date[modality] = dates[-1]
        br = y_vals[modality][-1]

        if modality == 'cases':
            note = ' (na 100k stanovnika)'
        elif modality == 'hospitalisations':
            note = ' (na 1M stanovnika)'
        else:
            note = ''

        for i in range(n_plots):
            axs[i].plot(x, y, 'o', label="Službeni broj {} u {} dana{}".format(modality_name, n_days, note), markersize=10)
            
        x_for_fit[modality] = np.asarray(list(range(n_for_fitting)))
        y_for_fit[modality] = y[-n_for_fitting:]


        if modality == 'hospitalisations':
            p_opt[modality], p_cov[modality] = curve_fit(curve_model, x_for_fit[modality], y_for_fit[modality], bounds=([0, -0.9], [2000, 0.9]), maxfev=10000)
        else:
            p_opt[modality], p_cov[modality] = curve_fit(curve_model, x_for_fit[modality], y_for_fit[modality], bounds=([0, -0.9], [2000, 0.9]), maxfev=10000)

        print('params:', p_opt[modality])

        tau[modality] = 1 / p_opt[modality][1]

        print('k', p_opt[modality][1])
        print('Tau', tau[modality])

        doubling_time[modality] = np.log(2) / p_opt[modality][1]

        print('doubling time', doubling_time[modality])

        eerror[modality] = np.sqrt(np.diag(p_cov[modality]))
        doubling_time_error[modality] = doubling_time[modality] * np.abs(eerror[modality][1] / p_opt[modality][1]) * 1.96

        print('x', x_for_fit[modality])
        print('y_true', y_for_fit[modality])
        print('y_pred', curve_model(x_for_fit[modality], *p_opt[modality]))

        # groundtruth - fit
        resid[modality] = y_for_fit[modality] - curve_model(x_for_fit[modality], *p_opt[modality])

        ss_tot[modality] = np.sum((y_for_fit[modality] - np.mean(y_for_fit[modality])) ** 2)
        ss_res[modality] = np.sum(resid[modality] ** 2)

        # R^2
        r2[modality] = 1 - (ss_res[modality] / ss_tot[modality])

        print(r2[modality])


        last_index = max(x) + n_fut + 1

        thrs = []

        if r2[modality] > r2_threshold:
            axs[0].plot(x[-n_for_fitting:], curve_model(range(n_for_fitting), *p_opt[modality]), '-', label='Fit {}: n(t)={:.3f}*e^({:.3f}t), R^2={:.3f}, t_({})=0.'.format(modality_name, p_opt[modality][0], p_opt[modality][1], r2[modality], first_date), linewidth=10, alpha=0.7)

            xfut = range(n_for_fitting -1, n_for_fitting + n_fut - 1)

            axs[0].plot(range(max(x), max(x) + n_fut), curve_model(xfut, *p_opt[modality]), ':', label='n(t)={:.3f}*e^({:.3f}t), 7 dana u budućnosti (pod pretpostavkom istog trenda)'.format(p_opt[modality][0], p_opt[modality][1]), linewidth=8)
            

            axs[1].plot(x[-n_for_fitting:], curve_model(x_for_fit[modality], *p_opt[modality]), '-', label='Fit {}: n(t)={:.3f}*e^({:.3f}t)'.format(modality_name, p_opt[modality][0], p_opt[modality][1]), linewidth=10, alpha=0.7)
            
            p = axs[1].plot(range(max(x), max(x) + n_fut), curve_model(xfut, *p_opt[modality]), ':', label='n(t)={:.3f}*e^({:.3f}t), 7 dana u budućnosti (pod pretpostavkom istog trenda)'.format(p_opt[modality][0], p_opt[modality][1]), linewidth=8)
            

            print('doubling_time: ', round(doubling_time[modality], 2), '(±', round(doubling_time_error[modality], 2),')')
            print('R^2', r2)

            future_ticks = range(max(x + 7), last_index + 4, 7)
            
            future_dates_dt = [all_vals[modality][-1]['date'] + datetime.timedelta((i + 1) * 7) for i in range(len(future_ticks))]
            
            future_dates = [x.strftime("%d.%m.") for x in future_dates_dt] # ['a']  * len(future_ticks)
            
            # plt.suptitle('{}: Procjena broja {} u 14 dana na 100000 stanovnika.\nt_({}) = 0. Broj {} {}: {:.2f}. Procijenjeni period udvostručavanja: {:.2f} ± {:.2f} dana'.format(region, modality_name, first_date, modality_name, last_date, br, round(doubling_time[modality], 2), round(doubling_time_error[modality], 2)), y=1.05)
        else:
            # plt.suptitle('{}: Broj {} u 14 dana na 100000 stanovnika.\nt_({}) = 0. Broj {} {}: {:.2f}.'.format(region, modality_name, first_date, modality_name, last_date, br), y=1.05)
        
            future_ticks = []
            future_dates = []

    
        if modality != 'hospitalisations':
            for i in range(n_plots):
                # for thr in thrs:
                #     if r2[modality] <= r2_threshold:
                #         last_index = max(x) + 1

                #     axs[i].plot(range(last_index), [thr] * last_index, '--', label='{} {}'.format(thr, modality_name), linewidth=2)


                axs[i].set_ylabel('Broj slučajeva u {} dana na 100k stanovnika\nBroj umrlih u {} dana (bez normalizacije)\nBroj hospitaliziranih u {} dana na 1M stanovnika'.format(n_days, n_days, n_days))
                axs[i].set_xlabel('Dan')
                
                axs[i].set_ylim(bottom=0) # , top=2000)
                
                axs[i].set_xticks(list(range(max(x) + 1)[::-7]) + list(future_ticks))
                axs[i].set_xticklabels(dates[::-7] + future_dates)

                for tick in axs[i].get_xticklabels():
                    tick.set_ha('right')
                    tick.set_rotation(30)
                
                axs[i].grid('on')
            
    axs[0].legend(loc='lower left')

    axs[0].set_yticks(list(axs[0].get_yticks()) + [1])


    title = '{}: Kretanje broja slučajeva/hospitalizacija/umrlih. ({})\nKretanje aproksimirano eksponencijalnom funkcijom na temelju podataka za zadnjih 7 dana.'.format(region, last_date['cases']) + '\n(izvori podataka: koronavirus.hr (slučajevi, umrli), hzjz.hr (hospitalizacije). y-os na lijevom grafu je na logaritamskoj skali, linearna na desnom.)\n\nBroj slučajeva u {} dana na 100k stanovnika: {:.2f} ({}).\nBroj umrlih u {} dana: {} ({}).\nBroj hospitaliziranih u {} dana na 1M stanovnika: {:.2f} ({}).\n'.format(n_days, y_vals['cases'][-1], last_date['cases'], n_days, int(y_vals['deaths'][-1]), last_date['deaths'], n_days, y_vals['hospitalisations'][-1], last_date['hospitalisations'])

    if r2['cases'] > r2_threshold:
        if doubling_time['cases'] > 0:
            period_type = 'udvostručavanja'
        else:
            period_type = 'prepolavljanja'

        title += '\nProcijenjeni period {} broja slučajeva: {:.2f} ± {:.2f} dana.'.format(period_type, abs(round(doubling_time['cases'], 2)), abs(round(doubling_time_error['cases'], 2)))
    
    if r2['deaths'] > r2_threshold:
        if doubling_time['deaths'] > 0:
            period_type = 'udvostručavanja'
        else:
            period_type = 'prepolavljanja'
        title += '\nProcijenjeni period {} broja umrlih: {:.2f} ± {:.2f} dana.'.format(period_type, abs(round(doubling_time['deaths'], 2)), abs(round(doubling_time_error['deaths'], 2)))

    if r2['hospitalisations'] > r2_threshold:
        if doubling_time['hospitalisations'] > 0:
            period_type = 'udvostručavanja'
        else:
            period_type = 'prepolavljanja'
        title += '\nProcijenjeni period {} broja hospitaliziranih: {:.2f} ± {:.2f} dana.'.format(period_type, abs(round(doubling_time['hospitalisations'], 2)), abs(round(doubling_time_error['hospitalisations'], 2)))

    
    plt.suptitle(title, y=1)

    plt.tight_layout()
    
    if n_days == 14:
        plt.savefig("img/{}_cases_hospitalisations_deaths_{}.png".format(all_vals['cases'][-1]['date'].strftime("%Y_%m_%d"), region.replace(' ', '_')), dpi=100, transparent=False, bbox_inches='tight', facecolor="white")
    else:
        plt.savefig("img/{}_cases_hospitalisations_deaths_{}_{}.png".format(all_vals['cases'][-1]['date'].strftime("%Y_%m_%d"), region.replace(' ', '_'), n_days), dpi=100, transparent=False, bbox_inches='tight', facecolor="white")	
        print("img/{}_cases_hospitalisations_deaths_{}_{}.png".format(all_vals['cases'][-1]['date'].strftime("%Y_%m_%d"), region.replace(' ', '_'), n_days))
    #plt.show()


# print('deaths y_pred', curve_model(xfut, *p_opt['deaths']))
