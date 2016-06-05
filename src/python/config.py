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

# 58 Statistics of participants to parse
# More information: https://developer.riotgames.com/api/methods#!/1064
PARTICIPANT_STATS = [
    'assists', 'champLevel', 'combatPlayerScore', 'deaths', 'doubleKills',
    'firstBloodAssist', 'firstBloodKill', 'firstInhibitorAssist',
    'firstInhibitorKill', 'firstTowerAssist', 'firstTowerKill', 'goldEarned',
    'goldSpent', 'inhibitorKills', 'item0', 'item1', 'item2', 'item3', 'item4',
    'item5', 'item6', 'killingSprees', 'kills', 'largestCriticalStrike',
    'largestKillingSpree', 'largestMultiKill', 'magicDamageDealt',
    'magicDamageDealtToChampions', 'magicDamageTaken', 'minionsKilled',
    'neutralMinionsKilled', 'neutralMinionsKilledEnemyJungle',
    'neutralMinionsKilledTeamJungle', 'objectivePlayerScore', 'pentaKills',
    'physicalDamageDealt', 'physicalDamageDealtToChampions',
    'physicalDamageTaken', 'quadraKills', 'sightWardsBoughtInGame',
    'totalDamageDealt', 'totalDamageDealtToChampions', 'totalDamageTaken',
    'totalHeal', 'totalPlayerScore', 'totalScoreRank',
    'totalTimeCrowdControlDealt', 'totalUnitsHealed', 'towerKills',
    'tripleKills', 'trueDamageDealt', 'trueDamageDealtToChampions',
    'trueDamageTaken', 'unrealKills', 'visionWardsBoughtInGame',
    'wardsKilled', 'wardsPlaced', 'winner'
]

# First match to dump
STARTING_MATCH_ID = FIRST_MATCH_ID['SEASON2015']

# Mongodb settings
DATABASE = {
    'host': 'localhost',
    'port': 27017,
    'name': 'lol'
}
