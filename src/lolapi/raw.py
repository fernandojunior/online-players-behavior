'''
Raw  matches from LOL API using RiotWatcher.

Match filter:
    matchMode: CLASSIC
    season: SEASON2015
    region: BR
    queueType: RANKED_SOLO_5x5
'''

import json
from riotwatcher import RiotWatcher, BRAZIL
from config import API_KEY, DUMP_DIR, STARTING_MATCH_ID

api = RiotWatcher(key=API_KEY, default_region=BRAZIL)

starting_match_id = STARTING_MATCH_ID  # first match to dump

n_matches = 10000  # total matches to dump


def is_classic(match):
    return match['matchMode'] == 'CLASSIC'


def is_season_2015(match):
    return match['season'] == 'SEASON2015'


def is_br(match):
    return match['region'] == 'BR'


def is_ranked_solo_5x5(match):
    return match['queueType'] == 'RANKED_SOLO_5x5'


def is_valid(match):
    return (
        is_classic(match) and
        is_season_2015(match) and
        is_br(match) and
        is_ranked_solo_5x5(match))


counter = 0  # total matches dumped
i = 0  # matches iterator
while counter < n_matches:
    try:  # get_match might have a connection time-out

        match_id = i + starting_match_id
        print match_id
        match = api.get_match(match_id=match_id)

        if is_valid(match):
            print True
            with open(DUMP_DIR + str(match_id) + '.json', 'w+') as f:
                json.dump(match, f)
            counter += 1
            print(counter)
    except:
        pass
    i += 1
