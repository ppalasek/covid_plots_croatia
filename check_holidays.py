from datetime import datetime, timedelta
import json

holidays = ['2022_01_01',
            '2022_01_06',
            '2022_04_17',
            '2022_04_18',
            '2022_05_01',
            '2022_05_30',
            '2022_06_16',
            '2022_06_22',
            '2022_08_05',
            '2022_08_15',
            '2022_11_01',
            '2022_11_18',
            '2022_12_25',
            '2022_12_26',

            '2023_01_01',
            '2023_01_06',
            '2023_04_09',
            '2023_04_10',
            '2023_05_01',
            '2023_05_30',
            '2023_06_08',
            '2023_06_22',
            '2023_08_05',
            '2023_08_15',
            '2023_11_01',
            '2023_11_18',
            '2023_12_25',
            '2023_12_26']


holidays = [datetime.strptime(x, '%Y_%m_%d').date() for x in holidays]


def count_holidays(current_date, offset, num_days):
    start_date = current_date - timedelta(days=offset)
    
    matched_holidays = []
    
    for i in range(num_days):
        cur_date = start_date + timedelta(days=i)
            
        if cur_date in holidays:
            matched_holidays.append(cur_date)
    
    return matched_holidays


last_date = datetime.now().date() - timedelta(days=1)

holidays_this_week = count_holidays(last_date, 6, 7)
holidays_last_week = count_holidays(last_date, 13, 7)

note = None

if len(holidays_this_week) == len(holidays_last_week) and len(holidays_this_week) == 0:
    print('no holidays. fine.')
else:
    str_holidays = [x.strftime('%d.%m.') for x in holidays_last_week]
    str_holidays += [x.strftime('%d.%m.') for x in holidays_this_week]
    
    note = ', '.join(str_holidays[:-1])
    
    if len(holidays_this_week) + len(holidays_last_week) > 1:
        note += ' i '
        note += str_holidays[-1]
        note += ' bili su praznici.'
    else:
        note = '{} bio je praznik.'.format(str_holidays[0])
        
    if len(holidays_this_week) > len(holidays_last_week):
        note += ' Prikazani postotak tjedne promjene vjerojatno je podcijenjen (stvarni pad je manji, tj. stvarni rast je veći).'
    elif len(holidays_this_week) < len(holidays_last_week):
        note += ' Prikazani postotak tjedne promjene vjerojatno je precijenjen (stvarni pad je veći, tj. stvarni rast je manji).'
    else:
        note += ' Prikazane tjedne promjene broja slučajeva treba pažljivo interpretirati.'


with open('holiday_note.json', 'w') as f:
    json.dump({'note' : note}, f)

print(note)
