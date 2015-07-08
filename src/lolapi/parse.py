"""
Transforma arquivos em json para um unico arquivo CSV, sem normalizar os dados.
"""

import json
import os

def create_row (*args):
    return ",".join(map(str, args)) 

DUMP_DIR = 'dump/'
counter = 0
trainingData = open('data/sem_normalizacao.csv','w+')

# ong features
labels = '"Win","FirstBlood","FirstTower","FirstTowerAssist","Kills","Assists",'\
    '"Deaths","GoldEarned","TotalDamageDealt","MagicDamageDealt","PhysicalDamageDealt",' \
    '"TotalDamageDealtToChampions","TotalDamageTaken","MinionsKilled","NeutralMinionsKilled",' \
    '"CrowdControl","WardsPlaced","TowerKills","LargestMultiKill","LargestKillingSpree","LargestCritStrike","TotalHealAmount"'

trainingData.write(labels.strip(' \t\n\r'))
trainingData.write('\n')

for f in os.listdir(DUMP_DIR):
    if f.find('json') != -1:

        json_data = open(DUMP_DIR+f)
        data = json.load(json_data)
        json_data.close()

        counter += 1

        for i in range(0,10):  # printing loop by participant

            row = ''

            # team index based on team id of current participant
            team_index = 0 if data['participants'][i]['teamId'] == data['teams'][0]['teamId'] else 1

            # participant stats
            stats = data['participants'][i]['stats']

            row = create_row(
                int(data['teams'][team_index]['winner']), # boolean to int
                int(stats['firstBloodKill']), # boolean to int
                int(stats['firstTowerKill']), # boolean to int
                int(stats['firstTowerAssist']), # boolean to int
                stats['kills'],
                stats['assists'],
                stats['deaths'],
                stats['goldEarned'],
                stats['totalDamageDealt'],
                stats['magicDamageDealt'],
                stats['physicalDamageDealt'],
                stats['totalDamageDealtToChampions'],
                stats['totalDamageTaken'],
                stats['minionsKilled'],
                stats['neutralMinionsKilled'],
                stats['totalTimeCrowdControlDealt'],
                stats['wardsPlaced'],
                stats['towerKills'],
                stats['largestMultiKill'],
                stats['largestKillingSpree'],
                stats['largestCriticalStrike'],
                stats['totalHeal']
            )

            trainingData.write(row)
            trainingData.write('\n')
            print counter
