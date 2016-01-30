"""
Parse json matches to a CSV file, without normalization/standardization.

The matches are looked up by participants. Only 22 statistical attributes of
participants selected by ONG et al. (2015) are considered.
"""
import json
import os
from datetime import datetime
from config import DUMP_DIR, DATA_DIR


def parse(value):
    """
    Parse a value of any python type to string.

    Boolean values are converted to integer values {True: 1, False: 2}.
    """
    return parse(int(value)) if isinstance(value, bool) else str(value)


def csvrow(*values):
    """
    Create a csv row.

    Convert a list of values (any python type) into a comma separated values
    string.
    """
    return ",".join(map(parse, values))


def load_match(filename):
    """
    Load a specific match in json format.
    """
    with open(filename) as f:  # open match
        return json.load(f)  # read match

filename = 'data.%s.csv' % datetime.now().strftime('%Y%m%d.%H%M%S')
datafile = open(DATA_DIR + filename, 'w+')

headers = ','.join('\"%s\"' % header for header in [
    "matchId",
    "matchCreation",
    "summonerId",
    "championId",
    # the 22 attributes selected by Ong et al. (2015):
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

datafile.write(headers)
datafile.write('\n')

for f in os.listdir(DUMP_DIR):  # list matches

    data = load_match(DUMP_DIR+f)

    # looking up by participants
    for i, participant in enumerate(data['participants']):

        stats = participant['stats']

        # row based on ONG et al. attributes
        row = csvrow(
            # general info - not used for clustering:
            data['matchId'],
            data['matchCreation'],
            data['participantIdentities'][i]['player']['summonerId'],
            participant['championId'],

            # booleans attributes - not need be normalized:
            stats['winner'],
            stats['firstBloodKill'],
            stats['firstTowerKill'],
            stats['firstTowerAssist'],
            # numeric attributes:
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

        datafile.write(row)
        datafile.write('\n')
