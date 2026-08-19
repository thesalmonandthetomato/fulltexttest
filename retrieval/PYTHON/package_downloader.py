#!/usr/bin/env python3
"""Fallback wrapper around computron/fulltext-article-downloader.
Reads one JSON request from stdin and writes one JSON response to stdout.
"""
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path


def main(req):
    doi = str(req.get("doi", "")).strip()
    if not doi:
        return {"ok": False, "error": "missing_doi", "path": "", "log": ""}

    outdir = Path(req.get("output_dir") or tempfile.mkdtemp(prefix="ftad_"))
    outdir.mkdir(parents=True, exist_ok=True)
    log = outdir / "fulltext_article_downloader.log"
    cmd = ["fulltext-download", doi, str(outdir)]
    try:
        p = subprocess.run(cmd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           timeout=int(req.get("timeout", 180)))
    except FileNotFoundError:
        return {"ok": False, "error": "fulltext-download_not_found", "path": "", "log": ""}
    except subprocess.TimeoutExpired as e:
        text = e.stdout or ""
        return {"ok": False, "error": "package_timeout", "path": "", "log": str(text)}

    text = p.stdout or ""
    log.write_text(text, encoding="utf-8", errors="replace")
    files = [x for x in outdir.rglob("*") if x.is_file() and x.name != log.name]
    # Prefer actual full-text artefacts over logs/metadata.
    files = [x for x in files if x.suffix.lower() in {".pdf", ".xml", ".html", ".htm"}]
    files.sort(key=lambda x: x.stat().st_size if x.exists() else 0, reverse=True)
    if p.returncode == 0 and files:
        return {"ok": True, "path": str(files[0]), "log": str(log), "error": ""}
    return {"ok": False, "path": "", "log": str(log),
            "error": f"package_exit_{p.returncode}: {text[-2000:]}"}


if __name__ == "__main__":
    try:
        req = json.loads(sys.stdin.read())
        print(json.dumps(main(req)), flush=True)
    except Exception as e:
        print(json.dumps({"ok": False, "path": "", "log": "",
                          "error": f"runner_error: {type(e).__name__}: {e}"}), flush=True)
