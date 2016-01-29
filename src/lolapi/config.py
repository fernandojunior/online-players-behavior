from os import listdir

# last dumped match in path
last_match = lambda path: max([int(m.split('.')[0]) for m in listdir(path)])

API_KEY = 'SUA_API_AQUI'

DUMP_DIR = 'lolapi/dump/'

TRAINING_DIR = 'lolapi/data/'

FIRST_MATCH_ID = {
    'SEASON2015': 456157041  # first match of season 2015
}

STARTING_MATCH_ID = last_match(DUMP_DIR)
