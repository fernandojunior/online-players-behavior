'''
Script to find a match at a specific date time by using gradient descendent.

Adaptaded from Lucas Coppio:
https://gist.github.com/Scoppio/2d5cf12311239ca9807f643ef21b88d1
'''
import math
import time
import random
from datetime import datetime
from riotwatcher.riotwatcher import RiotWatcher, BRAZIL
import config

riot_client = RiotWatcher(key=config.API_KEY, default_region=BRAZIL)


def sign(x):
    ''' Get a sign of a given number x '''
    return math.copysign(1, x)


def timestamp_to_datetime(t):
    ''' Convert a timestamp (seconds) to datetime format '''
    return datetime.fromtimestamp(t)


def timestamp(*args, **kwargs):
    ''' Convert a datetime to timestamp (seconds) '''
    return time.mktime(datetime(*args, **kwargs).timetuple())


def get_match(match_id):
    ''' Return a match by its ID '''
    return riot_client.get_match(match_id=match_id)


def find_match(year=datetime.now().year, month=1, day=1, hour=12, minute=0,
               seconds=0, tolerance=43200, starting_match_id=774365872):
    '''
    Find a match at a specific date time with a given tolerance in seconds.
    '''
    converged = False
    alpha = 0.000004  # learning rate

    disered_timestamp = timestamp(year, month, day, hour, minute, seconds)
    iter_count = 0
    match_id = starting_match_id
    match = None

    while not converged:
        try:
            match = get_match(match_id=match_id)
            print('OK,', match_id)

            match_timestamp = match['matchCreation'] / 1e3
            loss = disered_timestamp - match_timestamp
            if abs(loss) < tolerance:
                converged = True
            else:
                cost = 1/2 * (loss) ** 2
                match_id = int(match_id + (alpha * cost * sign(loss)))

        except Exception as e:
            print('%s, %s' % (str(e), match_id))
            if str(e) == 'Game data not found':
                match_id += random.randrange(100)
            if str(e) == 'Too many requests':
                time.sleep(2)   # connection time-out for get_match
        finally:
            iter_count += 1

    match['matchCreation'] = timestamp_to_datetime(match_timestamp)
    response = {k: match[k] for k in ['matchId', 'matchCreation', 'matchVersion']}
    response['iterCount'] = iter_count
    return response

if __name__ == '__main__':
    responses = []
    try:
        match = get_match(match_id=686810871)
        for month in range(1, 12):
            match = find_match(month=month, starting_match_id=match['matchId'])
            responses.append(match)
    except (KeyboardInterrupt, SystemExit):
        print ('Closing process')
    for response in responses:
        print(response)
