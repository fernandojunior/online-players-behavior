from os import listdir

# Last dumped match
last_match = lambda path: max([int(m.split('.')[0]) for m in listdir(path)])

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

# First match to dump
try:
    STARTING_MATCH_ID = last_match(DUMP_DIR)
except ValueError:
    STARTING_MATCH_ID = FIRST_MATCH_ID['SEASON2015']
