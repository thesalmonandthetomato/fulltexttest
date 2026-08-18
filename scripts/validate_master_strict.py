#!/usr/bin/env python3
"""Strict validation of canonical CSV/JSON; never modifies source data."""
import csv, json, sys, hashlib
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
CSV=ROOT/'data/master_100_extractions.csv'
JSON=ROOT/'data/master_100_extractions.json'
SUBSTANTIVE={'farmed_species','study_system_code','study_system_subcode','study_system_detail','geography','study_objective','paper_type','evidence_generation','study_approach','study_method','study_context','life_stage','fish_origin','facility_code','facility_detail','temporal_scope','study_unit','focal_factor_code','focal_factor_detail','comparison_type','comparison_detail','study_outcome_code','study_outcome_subcode','study_outcome_detail','evidence_section','evidence_passage','extraction_confidence','extraction_source_section','extraction_evidence'}
errors=[]
with CSV.open(newline='',encoding='utf-8') as f:
    reader=csv.DictReader(f); rows=list(reader); cols=reader.fieldnames or []
if len(cols)!=45: errors.append(f'CSV columns={len(cols)} expected=45')
if len(rows)!=100: errors.append(f'CSV records={len(rows)} expected=100')
ids=[r.get('record_number','') for r in rows]
if len(ids)!=len(set(ids)): errors.append('duplicate record_number')
if any(not x for x in ids): errors.append('blank record_number')
with JSON.open(encoding='utf-8') as f: obj=json.load(f)
if obj.get('schema')!=cols: errors.append('JSON schema != CSV schema')
jrows=obj.get('records',[])
if len(jrows)!=len(rows): errors.append('JSON record count != CSV record count')
if {r.get('record_number') for r in jrows}!={r.get('record_number') for r in rows}: errors.append('JSON record IDs != CSV record IDs')
for r in rows:
    if str(r.get('full_text_verified','')).lower()!='true' and any(r.get(k,'').strip() for k in SUBSTANTIVE):
        errors.append(f"{r['record_number']}: extraction populated without verified full text")
print(f'CSV records: {len(rows)}')
print(f'CSV columns: {len(cols)}')
print(f'CSV SHA256: {hashlib.sha256(CSV.read_bytes()).hexdigest()}')
print(f'JSON SHA256: {hashlib.sha256(JSON.read_bytes()).hexdigest()}')
if errors:
    print('VALIDATION FAILED'); [print('- '+e) for e in errors]; sys.exit(1)
print('VALIDATION PASSED')
