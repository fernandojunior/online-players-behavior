# Get one here: https://developer.riotgames.com/
API_KEY = 'YOUR_RIOT_API_HERE'

# Direcotry where matches will be saved
DUMP_DIR = '../dump/'

# Direcotry where parsed matches will be saved
DATA_DIR = '../data/'

# Dictionary of first matches by season
FIRST_MATCH_ID = {
    'SEASON2015': 456157041  # first match of season 2015
}

# Parse only matches that satisfy the following key values
MATCH_FILTER = {
    'matchMode': 'CLASSIC',
    'season': 'SEASON2015',
    'region': 'BR',
    'queueType': 'RANKED_SOLO_5x5',
}

# Statistics of participants to parse
# More in https://developer.riotgames.com/api/methods#!/1064
PARTICIPANT_STATS = [
    'winner',
    'firstBloodKill',
    'firstTowerKill',
    'firstTowerAssist',
    'kills',
    'assists',
    'deaths',
    'goldEarned',
    'totalDamageDealt',
    'magicDamageDealt',
    'physicalDamageDealt',
    'totalDamageDealtToChampions',
    'totalDamageTaken',
    'minionsKilled',
    'neutralMinionsKilled',
    'totalTimeCrowdControlDealt',
    'wardsPlaced',
    'towerKills',
    'largestMultiKill',
    'largestKillingSpree',
    'largestCriticalStrike',
    'totalHeal'
]

# First match to dump
STARTING_MATCH_ID = FIRST_MATCH_ID['SEASON2015']

# Mongodb settings
DATABASE = {
    'host': 'localhost',
    'port': 27017,
    'name': 'lol'
}
