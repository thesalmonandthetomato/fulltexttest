#!/usr/bin/env python3
import json
import sys
from urllib.parse import urljoin, urlparse, parse_qs, unquote
import requests
from bs4 import BeautifulSoup

UA = "Mozilla/5.0 fulltexttest-discovery/9.0"


def fetch(url, timeout=30):
    try:
        headers = {
            "User-Agent": UA,
            "Accept": "text/html,application/json,application/pdf,*/*",
        }
        r = requests.get(url, headers=headers, timeout=(10, timeout), allow_redirects=True)
        content_type = r.headers.get("content-type", "")
        is_pdf = r.content.startswith(b"%PDF") or "application/pdf" in content_type.lower()
        return {
            "ok": r.ok,
            "status": r.status_code,
            "type": content_type,
            "text": "" if is_pdf else r.text,
            "body_hex": r.content.hex() if is_pdf else "",
            "final_url": r.url,
            "error": "",
            "bytes": len(r.content),
        }
    except requests.RequestException as e:
        return {
            "ok": False,
            "status": None,
            "type": "",
            "text": "",
            "body_hex": "",
            "final_url": url,
            "error": f"{type(e).__name__}: {e}",
            "bytes": 0,
        }


def urls_from_html(html, base):
    if not html:
        return []
    soup = BeautifulSoup(html, "html.parser")
    out = []
    for a in soup.find_all("a", href=True):
        href = a.get("href", "").strip()
        if href.startswith("/url?"):
            vals = parse_qs(urlparse(href).query).get("q", [])
            out.extend(unquote(v) for v in vals)
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
            for k, v in x.items():
                if k in {"pdf_url", "landing_page_url", "url_for_pdf", "url"} and isinstance(v, str) and v.startswith("http"):
                    out.append(v)
                else:
                    walk(v)
        elif isinstance(x, list):
            for v in x:
                walk(v)

    walk(obj)
    return list(dict.fromkeys(out))


def main(req):
    result = fetch(req["url"], int(req.get("timeout", 30)))
    candidates = []
    if result["ok"] and result["text"]:
        host = (urlparse(result["final_url"]).hostname or "").lower()
        try:
            if host.startswith("api.") or result["type"].lower().startswith("application/json"):
                candidates = api_urls(json.loads(result["text"]))
            else:
                candidates = urls_from_html(result["text"], result["final_url"])
        except Exception:
            candidates = []
    result["candidates"] = candidates
    return result


if __name__ == "__main__":
    try:
        req = json.loads(sys.stdin.read())
        print(json.dumps(main(req)), flush=True)
    except Exception as e:
        print(json.dumps({
            "ok": False,
            "status": None,
            "type": "",
            "text": "",
            "body_hex": "",
            "final_url": "",
            "error": f"runner_error: {type(e).__name__}: {e}",
            "bytes": 0,
            "candidates": [],
        }), flush=True)
