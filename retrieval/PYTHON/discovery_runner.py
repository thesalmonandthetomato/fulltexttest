#!/usr/bin/env python3
import json
import sys
from urllib.parse import urljoin, urlparse, parse_qs
import requests
from bs4 import BeautifulSoup

UA = "Mozilla/5.0 fulltexttest-discovery/8.0"
SESSION = requests.Session()
SESSION.headers.update({"User-Agent": UA, "Accept": "text/html,application/json,application/pdf,*/*"})

def fetch(url, timeout=30):
    try:
        r = SESSION.get(url, timeout=(10, timeout), allow_redirects=True)
        return {"ok": r.ok, "status": r.status_code, "type": r.headers.get("content-type", ""), "text": r.text if not r.content.startswith(b"%PDF") else "", "final_url": r.url, "error": "", "bytes": len(r.content)}
    except requests.RequestException as e:
        return {"ok": False, "status": None, "type": "", "text": "", "final_url": url, "error": type(e).__name__ + ": " + str(e), "bytes": 0}

def urls_from_html(html, base):
    if not html:
        return []
    soup = BeautifulSoup(html, "html.parser")
    out = []
    for a in soup.find_all("a", href=True):
        href = a.get("href", "").strip()
        if href.startswith("/url?"):
            vals = parse_qs(urlparse(href).query).get("q", [])
            out.extend(vals)
        elif href.startswith(("http://", "https://", "/")):
            out.append(urljoin(base, href))
    for tag in soup.find_all(["meta", "link"]):
        v = tag.get("content") or tag.get("href") or ""
        if v.startswith(("http://", "https://", "/")):
            out.append(urljoin(base, v))
    return list(dict.fromkeys(u for u in out if u.startswith(("http://", "https://"))))

def api_urls(obj):
    out = []
    def walk(x):
        if isinstance(x, dict):
            for k,v in x.items():
                if k in {"pdf_url","landing_page_url","url_for_pdf","url"} and isinstance(v, str) and v.startswith("http"):
                    out.append(v)
                else: walk(v)
        elif isinstance(x, list):
            for v in x: walk(v)
    walk(obj)
    return list(dict.fromkeys(out))

def main(req):
    url = req["url"]
    timeout = int(req.get("timeout", 30))
    r = fetch(url, timeout)
    candidates = []
    if r["ok"] and r["text"]:
        candidates = urls_from_html(r["text"], r["final_url"])
        if "api." in (urlparse(r["final_url"]).hostname or ""):
            try: candidates = api_urls(r["text"] and json.loads(r["text"]))
            except Exception: pass
    return {**r, "candidates": candidates}

if __name__ == "__main__":
    req = json.loads(sys.stdin.read())
    print(json.dumps(main(req)), flush=True)
