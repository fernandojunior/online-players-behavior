"""
Transforms json files (lol matches) in a CSV file, without normalization.
The matches are looked up by participants.
ONG et al. attributes/features are considered.
"""
import json
import os
from config import DUMP_DIR, TRAINING_DIR


def clnstr(value):
    """
    Convert a value of any type to string.
    Boolean values are converted to int values: 1 (True), 0 (False)
    """
    return clnstr(int(value)) if isinstance(value, bool) else str(value)


def csvrow(*args):
    """
    Convert a list of arguments (any type) into a comma separated value string
    in python.
    """
    return ",".join(map(clnstr, args))


def open_match(path):
    """
    Open a specific match (.json) from the path
    """
    with open(path) as f:  # open match
        return json.load(f)  # read match

filename = TRAINING_DIR + 'ranked_matches_2015_ong_features.csv'
trainingData = open(filename, 'w+')

# Based on ONG et al. (2015) attributes
headers = ','.join('\"%s\"' % header for header in [
    "matchId",
    "matchCreation",
    "summonerId",
    "championId",
    "Win",
    "FirstBlood",
    "FirstTower",
    "FirstTowerAssist",
    "Kills",
    "Assists",
    "Deaths",
    "GoldEarned",
    "TotalDamageDealt",
    "MagicDamageDealt",
    "PhysicalDamageDealt",
    "TotalDamageDealtToChampions",
    "TotalDamageTaken",
    "MinionsKilled",
    "NeutralMinionsKilled",
    "CrowdControl",
    "WardsPlaced",
    "TowerKills",
    "LargestMultiKill",
    "LargestKillingSpree",
    "LargestCritStrike",
    "TotalHealAmount"
])

trainingData.write(headers.strip(' \t\n\r'))
trainingData.write('\n')

for f in os.listdir(DUMP_DIR):  # list matches
    if f.find('json') != -1:  # each match must be a json file

        data = open_match(DUMP_DIR+f)

        # looking up by participants. Each match has 10 participants
        for i in range(10):  # printing loop by participant

            # current participant
            participant = data['participants'][i]

            # participant stats in current match
            stats = participant['stats']

            # row based on ONG et al. attributes
            row = csvrow(
                # General Info -- not used for clustering
                data['matchId'],
                data['matchCreation'],
                data['participantIdentities'][i]['player']['summonerId'],
                participant['championId'],

                # Booleans attributes -- not need be normalized
                stats['winner'],
                stats['firstBloodKill'],
                stats['firstTowerKill'],
                stats['firstTowerAssist'],

                # numeric attributes
                stats['kills'],
                stats['assists'],
                stats['deaths'],
                stats['goldEarned'],
                stats['totalDamageDealt'],
                stats['magicDamageDealt'],
                stats['physicalDamageDealt'],
                stats['totalDamageDealtToChampions'],
                stats['totalDamageTaken'],
                stats['minionsKilled'],
                stats['neutralMinionsKilled'],
                stats['totalTimeCrowdControlDealt'],
                stats['wardsPlaced'],
                stats['towerKills'],
                stats['largestMultiKill'],
                stats['largestKillingSpree'],
                stats['largestCriticalStrike'],
                stats['totalHeal']
            )

            trainingData.write(row)
            trainingData.write('\n')
