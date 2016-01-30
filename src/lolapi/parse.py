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
csvfile = open(DATA_DIR + filename, 'w+')

# general info - not used for clustering
info_attrs = [
    "matchId",
    "matchCreation",
    "summonerId",
    "championId",
]

# the 22 statistical attributes of participants selected by Ong et al. (2015)
stats_attrs = [
    # booleans attributes - not need be normalized:
    "winner",
    "firstBloodKill",
    "firstTowerKill",
    "firstTowerAssist",
    # numeric attributes:
    "kills",
    "assists",
    "deaths",
    "goldEarned",
    "totalDamageDealt",
    "magicDamageDealt",
    "physicalDamageDealt",
    "totalDamageDealtToChampions",
    "totalDamageTaken",
    "minionsKilled",
    "neutralMinionsKilled",
    "totalTimeCrowdControlDealt",
    "wardsPlaced",
    "towerKills",
    "largestMultiKill",
    "largestKillingSpree",
    "largestCriticalStrike",
    "totalHeal"
]

headers = ','.join('\"%s\"' % header for header in info_attrs + stats_attrs)

csvfile.write(headers)
csvfile.write('\n')

for f in os.listdir(DUMP_DIR):  # list matches

    data = load_match(DUMP_DIR+f)

    # looking up by participants
    for i, participant in enumerate(data['participants']):

        # general info values
        info = [
            data['matchId'],
            data['matchCreation'],
            data['participantIdentities'][i]['player']['summonerId'],
            participant['championId']
        ]

        # statistical values
        stats = participant['stats']  # all attributes
        stats = [stats[a] for a in stats_attrs]  # filtering stats values

        csvfile.write(csvrow(*(info + stats)))  # write values in csv format
        csvfile.write('\n')
