import json
import tweepy

from datetime import datetime, timedelta

from pandas import read_csv

import re
import time


with open('twitter_keys.json', 'r') as f:
    twitter_keys = json.load(f)


bearer_token = twitter_keys['bearer_token']
access_token = twitter_keys['access_token']
access_token_secret = twitter_keys['access_token_secret']
api_key = twitter_keys['api_key']
api_key_secret = twitter_keys['api_key_secret']




class MyListener(tweepy.StreamingClient):
    def __init__(self, bearer_token):       
        super(MyListener, self).__init__(bearer_token)
    
    def on_tweet(self, tweet):
        print(tweet.id, tweet.text)
        
        self.disconnect()


print('Listening for new tweet from koronavirus_hr...')

printer = MyListener(bearer_token)



# print(printer.get_rules())
# printer.delete_rules(1552562663024214017)
# printer.delete_rules(1560160651464609793)
# printer.delete_rules(1560295800479137797)


printer.add_rules(tweepy.StreamRule('("slučaj" OR "slučaja" OR "slučajeva" OR "ukupno testirano" OR "preminulo") from:koronavirus_hr'))

print(printer.get_rules())




printer.filter()

print('RUN!')


client = tweepy.Client(bearer_token=bearer_token,
                       consumer_key=api_key,
                       consumer_secret=api_key_secret,
                       access_token=access_token,
                       access_token_secret=access_token_secret,
                       wait_on_rate_limit=True)


print(datetime.now().date())
print(datetime.now().date() - timedelta(days=1))



csv_data = read_csv('data/latest/last_hzjz_data_with_twitter.csv')

last_date_dt = datetime.strptime(csv_data['datum'].to_list()[-1], "%d/%m/%Y").date()

print(last_date_dt)




print('Waiting for num tested and num positive...')


found = False

while not found:
    query = '("slučajeva" OR "slučaj" OR "slučaja" OR "ukupno testirano") from:koronavirus_hr'

    tweets = client.search_recent_tweets(query=query, tweet_fields=['created_at'], max_results=10)


    num_tested = []
    num_positive = []
    num_deaths = []

    dates = []

    for i, tweet in enumerate(tweets.data):

        users_liked = client.get_liking_users(tweet.id)
        
        i_liked = False
        
        if users_liked.data is not None:
            for user in users_liked.data:
                if user.id == 1550459771542802434:
                    print('I LIKED!')
                    i_liked = True
                    break

        if not i_liked:
            client.like(tweet_id=tweet.id)

        print(i, tweet.created_at)

        print(tweet.created_at.date())

        print('"{}"'.format(tweet.text))
        print('-' * 50)

        m = re.search("testirano.*od toga (\d+[\.\,]*\d*[^\d]+)u posljednja", tweet.text)

        if m:
            testirano_24h = m.groups()[0].replace('.', '').replace(',', '')

            print('>>>', tweet.created_at, 'testirano:', int(testirano_24h))
            print('-' * 50)

            num_tested.append((tweet.created_at.date() - timedelta(days=1), int(testirano_24h)))

        m = re.search("zabilježen.* (\d+[\.\,]*\d*[^\d]+)nov.* slučaj", tweet.text)

        if m:
            pozitivnih_24h = m.groups()[0].replace('.', '').replace(',', '')
            print('>>>', tweet.created_at, 'pozitivnih:', int(pozitivnih_24h))
            print('-' * 50)

            num_positive.append((tweet.created_at.date() - timedelta(days=1), int(pozitivnih_24h)))


        m = re.search("reminul.*\s+(\d+)\s+osob", tweet.text)

        if m:
            preminulo_24h = m.groups()[0].replace('.', '').replace(',', '')
            print('>>>', tweet.created_at, 'pozitivnih:', int(pozitivnih_24h))
            print('-' * 50)

            num_deaths.append((tweet.created_at.date() - timedelta(days=1), int(preminulo_24h)))



        print()

    num_tested = sorted(num_tested)
    num_positive = sorted(num_positive)
    num_deaths = sorted(num_deaths)

    print('new date from twitter:', num_tested[-1][0])

    print('it should be the last date in csv + 1 day:', last_date_dt + timedelta(days=1))

    a = num_tested[-1][0] == last_date_dt + timedelta(days=1)
    b = num_positive[-1][0] == last_date_dt + timedelta(days=1)

    print(num_tested)
    print('pos')
    print(num_positive)
    print('deaths')
    print(num_deaths)

    print('a', a)
    print('b', b)
    # we need both tweets
    found = a and b
    if not found:
        print('waiting 1 min')
        time.sleep(60)
        
    print('done')




print(num_tested)
print(num_positive)
print(num_deaths)

data_to_check = {'deaths' : num_deaths[-1][1]}

with open('data_to_check.json', 'w') as f:
    json.dump(data_to_check, f)




new_csv_data = csv_data.append({'datum' : '{}'.format((last_date_dt + timedelta(days=1)).strftime('%d/%m/%Y')),
                                'pozitivne osobe' : num_positive[-1][1],
                                'učinjeni testovi' : num_tested[-1][1],
                                'izvor' : 'twitter'},
                                ignore_index=True)

csv_data.to_csv('data/latest/bk_last_hzjz_data_with_twitter.csv', index=False)
new_csv_data.to_csv('data/latest/last_hzjz_data_with_twitter.csv', index=False)


total_new_7 = sum(new_csv_data['pozitivne osobe'].to_list()[-7:])
total_tested_7 = sum(new_csv_data['učinjeni testovi'].to_list()[-7:])

print('{:.1f}%'.format(round(total_new_7 *100 / total_tested_7)))



import subprocess


print('generating positive img')

subprocess.call('Rscript generate_cases_hosp_deaths.r',  shell=True)


text = 'Sedmodnevni prosjek udjela pozitivnih testova u Hrvatskoj {:.1f}%, {:.1f}% u posljednjih 24h.'.format(total_new_7 * 100 / total_tested_7, num_positive[-1][1] * 100 / num_tested[-1][1])

print(text)

from tweepy.auth import OAuthHandler

auth = OAuthHandler(api_key, api_key_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)




last_date_ = last_date_dt.strftime('%Y_%m_%d')

percentages_img_path = 'img/*_percentage_positive_tests*.png'.format(last_date_)
percentages_stacked_img_path = 'img/percentages_stacked.png'

print(percentages_img_path)

subprocess.call('convert -append {} {}'.format(percentages_img_path, percentages_stacked_img_path), shell=True)


media = api.media_upload(filename=percentages_stacked_img_path)
print("MEDIA: ", media)

tweet = api.update_status(status=text, media_ids= [media.media_id_string])
print("TWEET: ", tweet)



