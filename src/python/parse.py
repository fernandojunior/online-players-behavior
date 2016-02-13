'''
Parse a set of matches in JSON format to a file in CSV format. The matches will
be looked up by participants.
'''
import json
import os
from datetime import datetime
from config import DATA_DIR, DUMP_DIR, MATCH_FILTER, PARTICIPANT_STATS


def load(matchname):
    '''
    Load a specific match (JSON file).
    '''
    with open(matchname) as f:
        return json.load(f)


def parse(value):
    '''
    Parse a value of any python type to string.

    Boolean values are converted to integer values {True: 1, False: 0}.
    '''
    return parse(int(value)) if isinstance(value, bool) else str(value)


def satisfied(match, filter):
    '''
    Verify if a match is satisfied by the match filter.
    '''
    for key, value in filter.items():
        if match[key] != value:
            return False
    return True


def write(values, f):
    '''
    Convert a list of values into a comma separated values string and write it
    in a file.
    '''
    f.write(','.join(map(parse, values)) + '\n')


# Creating CSV file in DATA_DIR
filename = DATA_DIR + 'data' + datetime.now().strftime('%Y%m%d%H%M%S') + '.csv'
file_ = open(filename, 'w+')
print('CSV file:', file_.name)

# Writing CSV headers
headers = ['matchId', 'matchCreation', 'summonerId', 'championId']  # info
headers += PARTICIPANT_STATS  # statistical
write(headers, file_)

# Match loop
for matchname in os.listdir(DUMP_DIR):
    if '.json' not in matchname:
        continue

    match = load(DUMP_DIR + matchname)

    if not satisfied(match, MATCH_FILTER):
        continue

    # looking up match by participants
    for i, participant in enumerate(match['participants']):
        # selecting general info values of current participant
        values = [
            match['matchId'],
            match['matchCreation'],
            match['participantIdentities'][i]['player']['summonerId'],
            participant['championId']
        ]

        # selecting statistical values of current participant
        values += [participant['stats'][stat] for stat in PARTICIPANT_STATS]

        # writing participant values in the csv file
        write(values, file_)
