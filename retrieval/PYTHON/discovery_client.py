#!/usr/bin/env python3
import json, sys, time
from urllib.parse import urlparse
import requests

UA = "Mozilla/5.0 fulltexttest-discovery/7.0"

def fetch(url, timeout=30):
    try:
        r = requests.get(url, headers={"User-Agent": UA, "Accept": "text/html,application/json,application/pdf,*/*"}, timeout=(10, timeout), allow_redirects=True)
        body = r.content
        return {"ok": 200 <= r.status_code < 300, "status": r.status_code, "type": r.headers.get("content-type", ""), "body_b64": body.hex(), "final_url": r.url, "error": "", "bytes": len(body)}
    except requests.RequestException as e:
        return {"ok": False, "status": None, "type": "", "body_b64": "", "final_url": url, "error": type(e).__name__ + ": " + str(e), "bytes": 0}

if __name__ == "__main__":
    for line in sys.stdin:
        line=line.strip()
        if not line: continue
        try: print(json.dumps(fetch(json.loads(line)["url"])), flush=True)
        except Exception as e: print(json.dumps({"ok":False,"status":None,"error":type(e).__name__+": "+str(e)}), flush=True)
