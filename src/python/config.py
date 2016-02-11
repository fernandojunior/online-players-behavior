# Get one here: https://developer.riotgames.com/
API_KEY = 'SUA_API_AQUI'

# Direcotry where matches will be saved
DUMP_DIR = '../dump/'

# Direcotry where parsed matches will be saved
DATA_DIR = '../data/'

# Dictionary of first matches by season
FIRST_MATCH_ID = {
    'SEASON2015': 456157041  # first match of season 2015
}

# Filter used in the parser
MATCH_FILTER = {
    'matchMode': 'CLASSIC',
    'season': 'SEASON2015',
    'region': 'BR',
    'queueType': 'RANKED_SOLO_5x5',
}

# First match to dump
STARTING_MATCH_ID = FIRST_MATCH_ID['SEASON2015']
