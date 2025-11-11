#!/usr/bin/env python3.11

import json
import sys

data = [json.loads(line) for line in sys.stdin]

print(json.dumps(data, indent=2, ensure_ascii=False))


