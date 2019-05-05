import csv
from pprint import pprint

characteristics = [
  "party",
  "handicapped-infants",
  "water-project-cost-sharing",
  "adoption-of-the-budget-resolution",
  "physician-fee-freeze",
  "el-salvador-aid",
  "religious-groups-in-schools",
  "anti-satellite-test-ban", 
  "aid-to-nicaraguan-contras",
  "mx-missile",
  "immigration",
  "synfuels-corporation-cutback",
  "education-spending",
  "superfund-right-to-sue",
  "crime",
  "duty-free-exports",
  "export-administration-act-south-africa"
]

votes = {}
for c in characteristics:
    votes[c] = {
        'republican': {
            'y': 0, 
            'n': 0, 
            '?': 0
        },
        'democrat': {
            'y': 0, 
            'n': 0, 
            '?': 0
        },
    }

with open('data/house-votes-84.data') as f:
    reader = csv.reader(f)
    for row in reader:
        for i, val in enumerate(row):
            try:
                votes[characteristics[i]][row[0]][val] += 1
            except KeyError:
                continue
pprint(votes)
