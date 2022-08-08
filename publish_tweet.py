from datetime import datetime, timedelta

import tweepy

import sys
import json

from tweepy.auth import OAuthHandler



if len(sys.argv) == 4:
    print('have two images')
    path_1, path_2, text = sys.argv[1], sys.argv[2], sys.argv[3]

    paths = [path_1, path_2]
else:
    print('have 1 image')
    path, text = sys.argv[1], sys.argv[2]

    paths = [path]

new_date_ = (datetime.now().date() - timedelta(days=1)).strftime('%Y_%m_%d')

print(new_date_)


paths_with_date = []

for path in paths:
    path_with_date = path.format(new_date_)

    paths_with_date.append(path_with_date)


from pathlib import Path 


for path in paths_with_date:
    assert Path(path).exists()

with open('twitter_keys.json', 'r') as f:
    twitter_keys = json.load(f)


holiday_note = ''

if Path('holiday_note.json').exists():
    with open('holiday_note.json', 'r') as f:
        holiday_note = json.load(f)['note']


    if holiday_note is None:
        holiday_note = ''

text = text.replace('[HOLIDAY_NOTE]', 'Napomena: {}'.format(holiday_note))


bearer_token = twitter_keys['bearer_token']
access_token = twitter_keys['access_token']
access_token_secret = twitter_keys['access_token_secret']
api_key = twitter_keys['api_key']
api_key_secret = twitter_keys['api_key_secret']



auth = OAuthHandler(api_key, api_key_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

media_id_strings = []

for path in paths_with_date:
    media = api.media_upload(filename=path)
    print("MEDIA: ", media)

    media_id_strings.append(media.media_id_string)

tweet = api.update_status(status=text, media_ids=media_id_strings)
print("TWEET: ", tweet)

