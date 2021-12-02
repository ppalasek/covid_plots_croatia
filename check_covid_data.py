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
    print('checking... {}'.format(time.time()))

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
                os.system('spd-say "new data"')
                time.sleep(1)

            break
    
    if not done:
        time.sleep(60)

done = False

while not done:
    print('checking... {}'.format(time.time()))

    source_url = 'https://www.koronavirus.hr/json/?action=po_danima_zupanijama'
    data = json.loads(requests.get(source_url).content)

    data = sorted(data, key=lambda x:x['Datum'])

    dates = [x['Datum'] for x in data]


    for date in dates:
        if date_to_find in date:
            print(date)
            print('done')
            done = True

            for i in range(1):
                os.system('spd-say "new data"')
                # time.sleep(5)

            break
    
    if not done:
        time.sleep(60)


