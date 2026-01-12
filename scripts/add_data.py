import json
import sys

# add a field with new test data to old test data
# python3 scripts/add_data.py tmp/testing.jsonl test/natural.tex natural

OLD_JSONL_FILE = sys.argv[1]
NEW_TEX_FILE = sys.argv[2]
NEW_KEY = sys.argv[3]


with open(OLD_JSONL_FILE) as file:
    old_data = [json.loads(line) for line in file]
    
with open(NEW_TEX_FILE) as file:
    new_data = [line.strip() for line in file if line.strip()]


def match(entry, line):
    dk = entry.get('dedukti', 'NONE')
    label = dk.split()[0].lower()       # Thm1 :
    tex = line.split()[0][:].lower()  # Thm1.
    return label == tex

    
for entry in old_data:
    entry[NEW_KEY] = [line for line in new_data if match(entry, line)]
    # print(entry[NEW_KEY])
    print(json.dumps(entry, ensure_ascii=False))



