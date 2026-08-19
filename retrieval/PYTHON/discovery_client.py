#!/usr/bin/env python3
import json, sys
import requests

UA = "Mozilla/5.0 fulltexttest-discovery/8.0"

def fetch(url, timeout=30):
    try:
        r = requests.get(url, headers={"User-Agent": UA, "Accept": "text/html,application/json,application/pdf,*/*"}, timeout=(10, timeout), allow_redirects=True)
        ct = r.headers.get("content-type", "")
        text = r.text if any(x in ct.lower() for x in ("text", "json", "html")) else ""
        return {"ok": 200 <= r.status_code < 300, "status": r.status_code,
                "type": ct, "text": text, "final_url": r.url,
                "error": "", "bytes": len(r.content)}
    except requests.RequestException as e:
        return {"ok": False, "status": None, "type": "", "text": "", "final_url": url,
                "error": type(e).__name__ + ": " + str(e), "bytes": 0}

if __name__ == "__main__":
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
            result = fetch(req["url"], int(req.get("timeout", 30)))
            print(json.dumps(result, ensure_ascii=False), flush=True)
        except Exception as e:
            print(json.dumps({"ok": False, "status": None, "type": "", "text": "", "final_url": "", "error": type(e).__name__ + ": " + str(e), "bytes": 0}), flush=True)
