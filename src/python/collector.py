'''
Collect LoL matches from Riot API at a specific match id range.

How to use:
python collector.py --start 710229426 --end 728819142 --path ../data/201602 --api_key SUA_CHAVE_AQUI
'''
import os
import os.path
import time
import json
from riotwatcher.riotwatcher import RiotWatcher, BRAZIL, LoLException
import random


def valid_match(match, criterion={}):
    return all([match[key] == value for key, value in criterion.items()])


def get_match(match_id, api_key):
    api_client = RiotWatcher(key=api_key, default_region=BRAZIL)
    return api_client.get_match(match_id=match_id)


def save_match(match, path):
    path = path if path[-1] is '/' else path + '/'
    filename = '%s%d%s' % (path, match['matchId'], ".json")
    if os.path.isfile(filename):
        raise LoLException('Game already saved')

    with open(filename, 'w') as f:
        json.dump(match, f)
    return match


def collect(start, end, path, criterion, api_key, n=10000):
    collected = []

    random_match_id = random.randint(start, end)
    while len(os.listdir(path)) < n:
        if random_match_id in collected:
            print('Game matchId already collected:', random_match_id)
            random_match_id = random.randint(start, end)

        try:
            match = get_match(random_match_id, api_key=api_key)
            collected.append(random_match_id)
            if valid_match(match, criterion):
                save_match(match, path=path)
                print('OK: ', len(os.listdir(path)), random_match_id,
                      match['queueType'], match['matchVersion'])
            else:
                print('Not valid: ', random_match_id, match['queueType'])
                # find valid match in neighborhood (match id window +- 50)
                random_match_id += random.randrange(50) * random.choice([-1, 1])

        except Exception as e:
            error_message = str(e)
            print('%s: %s' % (error_message, random_match_id))
            if error_message == 'Too many requests':
                time.sleep(5)   # connection time-out
            else:
                collected.append(random_match_id)

if __name__ == '__main__':
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--start", type=int,  help="starting match id")
    parser.add_option("--end", type=int,  help="ending match id")
    parser.add_option("--path", type=str,  help="path to save macthes")
    parser.add_option("--api_key", type=str,  help="Riot API key")
    (options, args) = parser.parse_args()
    criterion = {
        'matchMode': 'CLASSIC',
        'season': 'SEASON2016',
        'region': 'BR',
        'queueType': 'TEAM_BUILDER_DRAFT_RANKED_5x5',
    }
    collect(start=options.start, end=options.end, path=options.path,
            criterion=criterion, api_key=options.api_key)
