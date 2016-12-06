'''
Collect LoL matches from Riot API at a specific match id range.

How to use:
$ python collector.py --start 710229426 --end 728819142 --path ../data/201602 --api_key SUA_CHAVE_AQUI
'''
import os
import os.path
import time
import json
import random
import requests
from riotwatcher.riotwatcher import RiotWatcher, BRAZIL


riot_client = None


def create_riot_client(api_key):
    return RiotWatcher(key=api_key, default_region=BRAZIL)


def rand(start, end):
    ''' Generate a random integer number bewteen start and end '''
    url = 'https://www.random.org/integers/?num=1&min=%d&max=%d&col=5&base=10&format=plain&rnd=new'  # noqa
    try:
        return int(requests.get(url % (min, max)).text)
    except:
        return random.randint(min, max)


def get_match(match_id):
    ''' Get a match from Riot API '''
    try:
        return riot_client.get_match(match_id=match_id)
    except Exception as e:
        if (str(e) == 'Too many requests'):
            time.sleep(5)
            return get_match(match_id)
        else:
            raise e


def valid_match(match, criterion={}):
    ''' Valid if a match is valid given a specific criterion '''
    return all([match[key] == value for key, value in criterion.items()])


def save_match(match, filename):
    ''' Save a match in a file '''
    with open(filename, 'w') as f:
        json.dump(match, f)


def collect(start, end, path, criterion, total=10000):
    ''' Collect and save random matches bewteen start and end '''
    searched = []

    random_match_id = rand(start, end)
    match_filenames = os.listdir(path)
    while len(match_filenames) < total:
        filename = '%s%d%s' % (path, random_match_id, ".json")

        # game has already been searched or saved
        if random_match_id in searched or filename in match_filenames:
            random_match_id = rand(start, end)
            continue

        searched.append(random_match_id)
        match = None
        messages = ['index: %s' % len(searched)]
        try:
            match = get_match(random_match_id)
            if valid_match(match, criterion):
                save_match(match, filename)
                messages.append('status: OK')
                random_match_id = rand(start, end)
            else:  # search in neighborhood
                messages.append('status: Not valid')
                random_match_id += rand(1, 50) * random.choice([-1, 1])

            messages.append('matchId: %s Info:' % match['matchId'])
            messages.extend([match['queueType'], match['matchVersion']])
        except Exception as error:
            messages.append('status: %s matchId: %s' % (error,  searched[-1]))
            random_match_id = rand(start, end)

        # update match filenames
        match_filenames = os.listdir(path)
        messages.append('total: %s' % len(match_filenames))

        # print message for current random match ID
        print(' '.join(str(m) for m in messages))


if __name__ == '__main__':
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--start", type=int,  help="starting match id")
    parser.add_option("--end", type=int,  help="ending match id")
    parser.add_option("--path", type=str,  help="path to save macthes")
    parser.add_option("--api_key", type=str,  help="Riot API key")
    (options, args) = parser.parse_args()

    riot_client = create_riot_client(api_key=options.api_key)

    path = options.path
    path = path if path[-1] is '/' else path + '/'
    if not os.path.exists(path):
        os.makedirs(path)

    criterion = {
        'matchMode': 'CLASSIC',
        'season': 'SEASON2016',
        'region': 'BR',
        'queueType': 'TEAM_BUILDER_DRAFT_RANKED_5x5',
    }

    collect(start=options.start, end=options.end, path=path,
            criterion=criterion)
