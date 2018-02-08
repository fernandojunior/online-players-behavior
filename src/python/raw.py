'''
Raw  matches from LOL API using RiotWatcher.

Deprecated. Use `collector.py` instead.
'''
import os
import os.path
import time
import logging
import json
from riotwatcher.riotwatcher import RiotWatcher, BRAZIL, LoLException
import config


def last_match(dump_dir):
    '''
    Returns last dumped match ID from DUMP_DIR
    '''
    matches = [m for m in os.listdir(dump_dir) if m.endswith('.json')]

    if len(matches) == 0:
        return None

    return max([int(m.split('.')[0]) for m in matches])

# Minimal log setting
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# LOL api wrapper
wrapper = RiotWatcher(key=config.API_KEY, default_region=BRAZIL)

# First match to be dumped
starting_match_id = last_match(config.DUMP_DIR) or config.STARTING_MATCH_ID

n = 10000  # total n matches to dump
counter = 0  # total matches dumped
i = 0  # matches iterator

while counter < n:
    try:
        # Next match to dump
        match_id = starting_match_id + i

        filename = '%s%d%s' % (config.DUMP_DIR, match_id, ".json")

        if os.path.isfile(filename):
            logger.info('%s: Match already exists.', match_id)
            continue

        # Get match from Riot API
        match = wrapper.get_match(match_id=match_id)

        # Dumping
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
