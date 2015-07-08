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

# lol api wrapper
w = RiotWatcher(key='c349b53e-5086-42cd-b1bd-593683b7725a', default_region=BRAZIL)

# Constants
STARTING_MATCH_ID = 456157041 # first match of season 2015
N_MATCHES = 10000

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
    return isClassicMatch(match) and
        isSeason2015(match) and 
        isBr(match) and
        isRankedSolo5x5(match) and
        hasAllParticipants(match)

counter = 0
i = 0
while counter < N_MATCHES:
    try: # get_match might have a connection time-out

        match_id = i + STARTING_MATCH_ID
        match = w.get_match(match_id=match_id)

        if isValid(match):
            with open("dump/"+str(match_id)+".json", 'w+') as f:
                json.dump(match, f)
            counter += 1
            print(counter)
    except:
        pass
    i += 1