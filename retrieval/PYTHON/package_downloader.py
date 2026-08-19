#!/usr/bin/env python3
"""Controlled fallback wrapper around fulltext-article-downloader."""
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path


def main(req):
    doi = str(req.get("doi", "")).strip().lower()
    if not doi:
        return {"ok": False, "error": "missing_doi", "path": "", "log": ""}
    outdir = Path(req.get("output_dir") or tempfile.mkdtemp(prefix="ftad_"))
    outdir.mkdir(parents=True, exist_ok=True)
    log = outdir / "fulltext_article_downloader.log"
    try:
        p = subprocess.run(["fulltext-download", doi, str(outdir)], text=True,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           timeout=int(req.get("timeout", 180)))
    except FileNotFoundError:
        return {"ok": False, "error": "fulltext-download_not_found", "path": "", "log": ""}
    except subprocess.TimeoutExpired as e:
        text = e.stdout or ""
        return {"ok": False, "error": "package_timeout", "path": "", "log": str(text)}
    text = p.stdout or ""
    log.write_text(text, encoding="utf-8", errors="replace")
    files = [x for x in outdir.rglob("*") if x.is_file() and x.name != log.name and x.suffix.lower() in {".pdf", ".xml", ".html", ".htm"}]
    if p.returncode != 0:
        return {"ok": False, "path": "", "log": str(log), "error": f"package_exit_{p.returncode}: {text[-2000:]}"}
    if not files:
        return {"ok": False, "path": "", "log": str(log), "error": "package_no_fulltext_artifact"}
    doi_token = re.sub(r"[^a-z0-9]+", "", doi)
    matches = [f for f in files if doi_token and (doi_token in re.sub(r"[^a-z0-9]+", "", f.stem.lower()) or re.sub(r"[^a-z0-9]+", "", f.stem.lower()) in doi_token)]
    if len(matches) == 1:
        chosen = matches[0]
    elif len(files) == 1:
        chosen = files[0]
    else:
        return {"ok": False, "path": "", "log": str(log), "error": f"package_ambiguous_artifacts:{len(files)}"}
    return {"ok": True, "path": str(chosen), "log": str(log), "error": ""}


if __name__ == "__main__":
    try:
        print(json.dumps(main(json.loads(sys.stdin.read())), flush=True))
    except Exception as e:
        print(json.dumps({"ok": False, "path": "", "log": "", "error": f"runner_error: {type(e).__name__}: {e}"}, flush=True))
