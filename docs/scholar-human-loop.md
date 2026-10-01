# Google Scholar human-in-the-loop fallback

This fallback is for records that were not retrieved through the automated OpenAlex / Europe PMC / Unpaywall route.

It deliberately **does not scrape Google Scholar or automatically click Scholar results**. The script automates the repetitive local work while leaving selection of a full-text link to the user.

## What it automates

For each DOI in a CSV, the script:

1. opens a Google Scholar search for that DOI in the default browser;
2. waits for the user to choose an accessible full-text link;
3. checks the local Downloads folder for the resulting PDF;
4. validates the file begins with the PDF signature;
5. renames it from the publisher-provided filename to the DOI, replacing filesystem-unsafe characters such as `/` with `_`;
6. moves it to `retrieval/scholar_pdfs/`;
7. checkpoints the result in `retrieval/scholar_status.csv`;
8. proceeds to the next DOI.

The status file makes the process resumable. Records already marked `downloaded` or `not_found` are not presented again.

## Run

From the repository root:

```bash
Rscript scripts/scholar_human_loop.R path/to/input.csv
```

If the DOI column is not literally `doi` or `DOI`, give its name:

```bash
Rscript scripts/scholar_human_loop.R path/to/input.csv DOI_column_name
```

Optional arguments are:

```text
Rscript scripts/scholar_human_loop.R INPUT.csv DOI_COLUMN DOWNLOAD_DIR OUTPUT_DIR STATUS_CSV
```

Defaults:

- downloads watched: `~/Downloads`
- PDFs saved to: `retrieval/scholar_pdfs`
- progress log: `retrieval/scholar_status.csv`

## Per-record controls

After Scholar opens:

- **Enter**: detect the newly downloaded PDF, validate it, rename it and continue
- **n**: mark this DOI as `not_found`
- **s**: skip it for later; skipped DOIs are offered again next run
- **o**: reopen the Scholar search
- **d**: open the DOI landing page
- **q**: save progress and quit

## Important operational notes

- Download **one target PDF at a time** while using the queue. The script associates the newest PDF downloaded after the Scholar search opened with the current DOI.
- Wait until the browser download is complete before pressing Enter.
- PDFs and the status CSV are ignored by Git and remain local by default.
- The script does not authenticate to publishers or circumvent access controls. It uses whatever access is already available in the user's normal browser session.
