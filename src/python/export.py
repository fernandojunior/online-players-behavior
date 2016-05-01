'''
Export all dumped matches in JSON format to a mongodb instance.
'''
import json
import os
from pymongo import MongoClient
from config import DATABASE, DUMP_DIR


def load(matchname):
    '''
    Load a specific match (JSON file).
    '''
    with open(matchname) as f:
        return json.load(f)

# MongoDB connection
mongo_client = MongoClient('mongodb://{host}:{port}'.format(**DATABASE))
matches = mongo_client[DATABASE['name']]['match']
matches.drop()  # clean database collection

# File names of the matches to export
matchnames = [name for name in os.listdir(DUMP_DIR) if '.json' in name]
total = len(matchnames)  # total matches to export

# Match loop
for i, matchname in enumerate(matchnames):
    match = load(DUMP_DIR + matchname)
    msg = 'Export %s LoL match to MongoDB [%s/%s]' % (matchname, i + 1, total)
    print(msg)
    matches.insert(match)
