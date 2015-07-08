"""
Raw  matches from LOL API using RiotWatcher.

Based on https://github.com/LionTurtle/TeamCompML/blob/master/src/lolapi/getRawData.py

matchMode: CLASSIC
season: SEASON2015
region: BR
queueType: RANKED_SOLO_5x5
participants: 10
"""

import json
from riotwatcher import RiotWatcher, BRAZIL, LoLException
from config import API_KEY, FIRST_MATCH_ID, DUMP_DIR

# lol api wrapper
w = RiotWatcher(key=API_KEY, default_region=BRAZIL)

starting_match_id = FIRST_MATCH_ID['SEASON2015'] # first match to dump
n_matches = 10000 # total matches to dump

def isClassicMatch(match):
    return match['matchMode'] == 'CLASSIC'

def isSeason2015(match):
    return match['season'] == 'SEASON2015'

def isBr(match):
    return match['region'] == 'BR'

def isRankedSolo5x5(match):
    return match['queueType'] == 'RANKED_SOLO_5x5'

def hasAllParticipants(match):
    return len(match['participants']) == 10

def isValid(match):
    return isClassicMatch(match) and isSeason2015(match) and isBr(match) and isRankedSolo5x5(match) and hasAllParticipants(match)

counter = 0 # total matches dumped
i = 0 # matches iterator
while counter < n_matches:
    try: # get_match might have a connection time-out

        match_id = i + starting_match_id
        print match_id
        match = w.get_match(match_id=match_id)

        if isValid(match):
            print True
            with open(DUMP_DIR +str(match_id)+".json", 'w+') as f:
                json.dump(match, f)
            counter += 1
            print(counter)
    except:
        pass
    i += 1