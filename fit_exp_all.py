import numpy as np
from scipy.optimize import curve_fit

#%matplotlib inline
import matplotlib.pyplot as plt

from matplotlib.ticker import ScalarFormatter
import matplotlib

from pandas import read_csv
from pathlib import Path
import datetime

def curve_model(t, a, k):
    # exp curve
    return a * np.exp(k * t)


font = {'size': 23}
matplotlib.rc('font', **font)

r2_threshold = 0.95

# num points to use for fitting 
n_for_fitting = 7# 7

# num future points
n_fut = 7

# num points to plot
n_to_plot = 120

# cvs root dir
data_dir = 'data/region_data_csvs/'


cases_df = read_csv('data/latest/region_diff_cases_df.csv')
deaths_df = read_csv('data/latest/region_diff_deaths_df.csv')


# print(cases_df)



all_vals = []

for row_index in range(len(cases_df)):
    vals = {}
    total = {}

    current_row = cases_df.iloc[row_index]

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


    all_vals.append(vals)



formatter = ScalarFormatter()
formatter.set_scientific(False)




for region in all_vals[0].keys():
    if region == 'date':
        continue

    y_vals = [x[region]['14d_norm'] for x in all_vals][-n_to_plot:]
    dates = [x['date'].strftime("%d.%m.") for x in all_vals][-n_to_plot:]

    print(dates)

    y = np.asarray(y_vals)
    x = np.asarray(list(range(len(y))))
    
    first_date = dates[-n_for_fitting]
    last_date = dates[-1]
    br = y_vals[-1]

    fig, axs = plt.subplots(1,2, figsize=(25, 10))

    axs[0].plot(x, y, 'o', label="Službeni broj slučajeva")
    axs[1].plot(x, y, 'o', label="Službeni broj slučajeva")

    # axs[0].plot(range(len(x))[:-n_for_fitting], y[:-n_for_fitting], 'o', label="Novi podaci")
    # axs[1].plot(range(len(x))[:-n_for_fitting], y[:-n_for_fitting], 'o', label="Novi podaci")

    x_for_fit = np.asarray(list(range(n_for_fitting)))
    y_for_fit = y[-n_for_fitting:]

    p_opt, p_cov = curve_fit(curve_model, x_for_fit, y_for_fit, bounds=([0, -0.9], [2000, 0.9]), maxfev=10000)
    
    print('params:', p_opt)

   
    tau = 1 / p_opt[1]

    print('k', p_opt[1])
    print('Tau', tau)

    doubling_time = np.log(2) / p_opt[1]

    print('doubling time', doubling_time)

    eerror = np.sqrt(np.diag(p_cov))
    doubling_time_error = doubling_time * np.abs(eerror[1] / p_opt[1]) * 1.96

    print('x', x_for_fit)
    print('y_true', y_for_fit)
    print('y_pred', curve_model(x_for_fit, *p_opt))

    # groundtruth - fit
    resid = y_for_fit - curve_model(x_for_fit, *p_opt)

    ss_tot = np.sum((y_for_fit - np.mean(y_for_fit)) ** 2)
    ss_res = np.sum(resid ** 2)

    # R^2
    r2 = 1 - (ss_res / ss_tot)

    print(r2)

    last_index = max(x) + n_fut + 1

    axs[0].set_yscale('symlog', basey=2)
    
    
    axs[0].yaxis.set_major_formatter(formatter)


    

    
    if r2 > r2_threshold:
        axs[0].plot(x[-n_for_fitting:], curve_model(range(n_for_fitting), *p_opt), '-', label='n(t)={:.3f}*e^({:.3f}t), R^2={:.3f}'.format(p_opt[0], p_opt[1], r2), linewidth=5, alpha=0.7)

        xfut = range(n_for_fitting -1, n_for_fitting + n_fut - 1)

        axs[0].plot(range(max(x), max(x) + n_fut), curve_model(xfut, *p_opt), ':', label='n(t)={:.3f}*e^({:.3f}t),\nu budućnosti (pod pretpostavkom istog trenda)'.format(p_opt[0], p_opt[1]), linewidth=5)
        

        axs[1].plot(x[-n_for_fitting:], curve_model(x_for_fit, *p_opt), '-', label='n(t)={:.3f}*e^({:.3f}t)'.format(p_opt[0], p_opt[1]), linewidth=5, alpha=0.7)
        
        p = axs[1].plot(range(max(x), max(x) + n_fut), curve_model(xfut, *p_opt), ':', label='n(t)={:.3f}*e^({:.3f}t),\nu budućnosti (pod pretpostavkom istog trenda)'.format(p_opt[0], p_opt[1]), linewidth=5)
        


        print('doubling_time: ', round(doubling_time, 2), '(±', round(doubling_time_error, 2),')')
        print('R^2', r2)

        future_ticks = range(max(x + 7), last_index + 4, 7)
        
        future_dates_dt = [all_vals[-1]['date'] + datetime.timedelta((i + 1) * 7) for i in range(len(future_ticks))]
        
        future_dates = [x.strftime("%d.%m.") for x in future_dates_dt] # ['a']  * len(future_ticks)
        
        if doubling_time > 0:
            period_type = 'udvostručavanja'
        else:
            period_type = 'prepolavljanja'

        plt.suptitle('{}: Procjena broja slučajeva u 14 dana na 100000 stanovnika.\nt_({}) = 0. Broj slučajeva {}: {}. Procijenjeni period {}: {:.2f} ± {:.2f} dana'.format(region, first_date, last_date, int(br), period_type, abs(round(doubling_time, 2)), abs(round(doubling_time_error, 2))), y=1.05)
    else:
        plt.suptitle('{}: Broj slučajeva u 14 dana na 100000 stanovnika.\nt_({}) = 0. Broj slučajeva {}: {:.2f}.'.format(region, first_date, last_date, br), y=1.05)
    
        future_ticks = []
        future_dates = []
    
    for i in range(2):

        for thr in [75, 200]:
            if r2 <= r2_threshold:
                last_index = max(x) + 1

            axs[i].plot(range(last_index), [thr] * last_index, '--', label='{} slučajeva'.format(thr), linewidth=2)


        axs[i].set_ylabel('Broj slučajeva u 14 dana na 100k stanovnika')
        axs[i].set_xlabel('Dan')
        
        axs[i].set_ylim(bottom=0)
        
        axs[i].set_xticks(list(range(max(x) + 1)[::-7]) + list(future_ticks))
        axs[i].set_xticklabels(dates[::-7] + future_dates)

        for tick in axs[i].get_xticklabels():
            tick.set_ha('right')
            tick.set_rotation(30)
        
        axs[i].grid('on')
    
    axs[0].legend(loc='lower left')

    axs[0].set_yticks(list(axs[0].get_yticks()) + [1])

    plt.tight_layout()
    
    plt.savefig("img/{}_current_{}.png".format(all_vals[-1]['date'].strftime("%Y_%m_%d"), region.replace(' ', '_')), dpi=200, transparent=False, bbox_inches='tight', facecolor="white")


    #plt.show()



