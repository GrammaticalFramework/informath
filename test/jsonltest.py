import sys
import json

if sys.argv[1:]:
    JSONL_FILE = sys.argv[1]
else:
    print('usage: jsonltest.py <file> <formalkey>? <informalkey>?')
    exit()

with open(JSONL_FILE) as file:
    data = [json.loads(line) for line in file]

if sys.argv[3:]:
    formal = sys.argv[2]
    informal = sys.argv[3]
    corpus = [{formal: d[formal], informal: e} for d in data for e in d[informal]]
    for item in corpus:
        print(json.dumps(item, ensure_ascii=False))

    
else:
   print(len(data), 'formulas')
   print(sum([len(v) for d in data for k, v in d.items() if k == 'InformathEng']), 'sentences')


         
