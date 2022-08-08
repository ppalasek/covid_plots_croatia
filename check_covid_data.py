import json
import requests
import os
import time
import datetime


today = datetime.date.today()
date_to_find = today.strftime('%Y-%m-%d')
print(date_to_find)

done = False

while not done:
    print('checking for {}... {}'.format(date_to_find, time.time()))

    source_url = 'https://www.koronavirus.hr/json/?action=po_danima_zupanijama_zadnji'
    data = json.loads(requests.get(source_url).content)

    if not isinstance(data, list):
        data = [data]

    print(data)

    data = sorted(data, key=lambda x:x['Datum'])

    dates = [x['Datum'] for x in data]


    for date in dates:
        if date_to_find in date:
            print(date)
            print('done')
            done = True

            for i in range(1):
               # os.system('spd-say "new data"')
                time.sleep(1)

            break
    
    if not done:
        time.sleep(60)




with open('data_to_check.json', 'r') as f:
    data_to_check = json.load(f)

correct_num_deaths = data_to_check['deaths']



done = False

while not done:
    print('checking... {}'.format(time.time()))

    source_url = 'https://www.koronavirus.hr/json/?action=po_danima_zupanijama'
    data = json.loads(requests.get(source_url).content)

    data = sorted(data, key=lambda x:x['Datum'])

    dates = [x['Datum'] for x in data]

    for date in dates:
        if date_to_find in date:
            

            # check deaths
            deaths = [x['broj_umrlih'] for x in data[-1]['PodaciDetaljno']]

            sum_deaths = sum(deaths)

            prev_deaths = [x['broj_umrlih'] for x in data[-2]['PodaciDetaljno']]
            sum_prev_deaths = sum(prev_deaths)

            new_total_deaths = sum_deaths - sum_prev_deaths


            print('deaths from json', new_total_deaths)


            if new_total_deaths == correct_num_deaths:
                print('correct!')
                done = True


                # save data, making sure we have the correct json
                with open('data/latest/last_data_po_danima_zupanijama.json', 'w') as f:
                    json.dump(data, f)



                for i in range(1):
                    #os.system('spd-say "new data"')
                    # time.sleep(5)
                    pass
                break

            else:
                print('wrong num deaths. have: {}, need: {}'.format(new_total_deaths, correct_num_deaths))

                done = False
                break
    
    if not done:
        time.sleep(60)





time.sleep(10)
