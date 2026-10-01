# Full-text retrieval test

The active automated retrieval path is deliberately small and reproducible:

**DOI → OpenAlex → Europe PMC → Unpaywall → identity/structure validation → parsed `fulltext.txt`**

Run `.github/workflows/openalex-three-source-retrieval.yml` manually and supply a DOI. An optional expected title can be supplied for an additional identity check.

The workflow prefers OpenAlex content (GROBID TEI, then PDF), then exact-DOI Europe PMC XML, then Unpaywall OA locations. It emits the parsed text plus `metadata.json` and the source representation used. A result is only accepted when it passes the structural/identity gate; otherwise the run fails rather than returning an unverified document.

For records that remain unresolved after automated retrieval, the repository also contains a **human-in-the-loop Google Scholar fallback**. It opens each DOI search in the user's normal browser and automates download detection, PDF validation, DOI-based renaming, checkpointing and queue progression while leaving the actual Scholar/full-text click to the user.

See [docs/scholar-human-loop.md](docs/scholar-human-loop.md).

The previous experimental scripts/workflows have been removed from `main` and preserved in the `archive/legacy-retrieval` branch, so the test history remains available without cluttering the active workflow.
