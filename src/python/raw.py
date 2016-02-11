'''
Raw  matches from LOL API using RiotWatcher.
'''
import os
import os.path
import time
import logging
import json
from riotwatcher.riotwatcher import RiotWatcher, BRAZIL, LoLException
import config

# Minimal log setting
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def last_match():
    '''
    Returns last dumped match ID from DUMP_DIR
    '''
    matches = [m for m in os.listdir(config.DUMP_DIR) if m.endswith('.json')]

    if len(matches) == 0:
        return None

    return max([int(m.split('.')[0]) for m in matches])

# LOL api wrapper
wrapper = RiotWatcher(key=config.API_KEY, default_region=BRAZIL)

# First match to be dumped
starting_match_id = last_match() if last_match() else config.STARTING_MATCH_ID

n_matches = 10000  # total matches to dump
counter = 0  # total matches dumped
i = 0  # matches iterator

while counter < n_matches:
    try:
        match_id = starting_match_id + i  # next match to dump
        filename = '%s%d%s' % (config.DUMP_DIR, match_id, ".json")

        if os.path.isfile(filename):
            logger.info('%s: Match already exists.', match_id)
            continue

        match = wrapper.get_match(match_id=match_id)

        with open(filename, 'w') as f:
            json.dump(match, f)
            logger.info('%s: Match saved.', match_id)

        counter += 1

    except LoLException as e:
        logger.info('LoLException Error: %s: %s', match_id, e)
        if str(e) == 'Too many requests':
            logger.info('Sleeping way...')
            time.sleep(10)   # connection time-out for get_match
    finally:
        i += 1
