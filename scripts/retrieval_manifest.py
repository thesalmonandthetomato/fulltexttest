#!/usr/bin/env python3
"""Generate a deterministic retrieval manifest from the canonical 100-record index."""
import csv, json, hashlib
from pathlib import Path
from datetime import datetime, timezone
ROOT=Path(__file__).resolve().parents[1]
source=ROOT/'data/pilot_index_100.csv'
out=ROOT/'outputs/retrieval_manifest.json'
out.parent.mkdir(exist_ok=True)
with source.open(newline='',encoding='utf-8') as f: rows=list(csv.DictReader(f))
manifest={'generated_at':datetime.now(timezone.utc).isoformat(),'source_file':str(source.relative_to(ROOT)),'source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'record_count':len(rows),'records':[]}
for r in rows:
    rid=r.get('record_number') or r.get('id') or r.get('record_id')
    manifest['records'].append({'record_number':rid,'title':r.get('title',''),'doi':r.get('doi',''),'retrieval_status':'pending','full_text_verified':False,'retrieval_attempts':[],'extraction_status':'not_started'})
with out.open('w',encoding='utf-8') as f: json.dump(manifest,f,ensure_ascii=False,indent=2)
print(f'Generated {out} for {len(rows)} records; source SHA256 {manifest["source_sha256"]}')
