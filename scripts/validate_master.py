#!/usr/bin/env python3
"""Validate the canonical 100-record master CSV/JSON without modifying them."""
import csv, json, hashlib, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CSV = ROOT / 'data' / 'master_100_extractions.csv'
JSON = ROOT / 'data' / 'master_100_extractions.json'

EXPECTED_COLUMNS = 45
REQUIRED = {
    'record_number', 'title', 'retrieval_status', 'extraction_status',
    'full_text_verified', 'audit_status'
}

def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

with CSV.open(newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f))
    fieldnames = rows[0].keys() if rows else []

errors = []
if len(fieldnames) != EXPECTED_COLUMNS:
    errors.append(f'CSV has {len(fieldnames)} columns; expected {EXPECTED_COLUMNS}')
missing = REQUIRED - set(fieldnames)
if missing:
    errors.append(f'Missing required columns: {sorted(missing)}')
ids = [r['record_number'] for r in rows]
if len(ids) != len(set(ids)):
    errors.append('Duplicate record_number values detected')
if not rows:
    errors.append('CSV contains no records')

for i, r in enumerate(rows, start=2):
    if r['full_text_verified'].lower() != 'true':
        substantive = [k for k,v in r.items() if k in {
            'farmed_species','study_system_code','study_system_subcode','study_system_detail',
            'study_objective','paper_type','evidence_generation','study_approach','study_method',
            'study_context','life_stage','fish_origin','facility_code','facility_detail',
            'temporal_scope','study_unit','focal_factor_code','focal_factor_detail',
            'comparison_type','comparison_detail','study_outcome_code','study_outcome_subcode',
            'study_outcome_detail','evidence_section','evidence_passage'
        } and v.strip()]
        if substantive:
            errors.append(f'Row {i} ({r["record_number"]}) has substantive extraction without verified full text')

if JSON.exists():
    obj = json.loads(JSON.read_text(encoding='utf-8'))
    if 'records' not in obj or 'schema' not in obj:
        errors.append('JSON missing schema/records')
    else:
        if obj['schema'] != list(fieldnames):
            errors.append('JSON schema does not exactly match CSV schema')
        if len(obj['records']) != len(rows):
            errors.append('JSON record count differs from CSV')
else:
    errors.append('Canonical JSON is missing')

print(f'CSV records: {len(rows)}')
print(f'CSV columns: {len(fieldnames)}')
print(f'CSV SHA256: {sha256(CSV)}')
print(f'JSON SHA256: {sha256(JSON) if JSON.exists() else "MISSING"}')

if errors:
    print('\nVALIDATION FAILED')
    for e in errors:
        print(f'- {e}')
    sys.exit(1)

print('\nVALIDATION PASSED')
