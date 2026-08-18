#!/usr/bin/env python3
"""Prepare and validate a bounded retrieval batch.

This scaffold deliberately does not infer full-text availability. Retrieval results
must be supplied from verified document acquisition and recorded per attempt.
"""
import csv,json,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
idx=ROOT/'data/pilot_index_100.csv'
start=int(sys.argv[1]) if len(sys.argv)>1 else 1
size=int(sys.argv[2]) if len(sys.argv)>2 else 10
with idx.open(newline='',encoding='utf-8') as f: rows=list(csv.DictReader(f))
batch=rows[start-1:start-1+size]
if len(batch)!=size: raise SystemExit(f'Requested {size}, found {len(batch)}')
out=ROOT/f'outputs/retrieval_batch_{start:03d}_{start+len(batch)-1:03d}.json'
out.parent.mkdir(exist_ok=True)
json.dump({'batch_start':start,'batch_end':start+len(batch)-1,'records':[{'record_number':r.get('record_number'),'title':r.get('title'),'doi':r.get('doi'),'retrieval_status':'pending','full_text_verified':False,'retrieval_attempts':[]} for r in batch]},out.open('w',encoding='utf-8'),ensure_ascii=False,indent=2)
print(out)
