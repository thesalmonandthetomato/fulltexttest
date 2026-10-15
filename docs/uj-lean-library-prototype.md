# UJ Lean Library / Chrome retrieval prototype

This is an **isolated local R experiment**, not a GitHub Actions workflow and not part of the existing OA pipeline.

## Scope and current status

The R code attaches to a **user-started Chrome session**, preserves the browser profile during the run, navigates sequentially to supplied DOIs, inspects visible PDF hyperlinks, optionally clicks a likely direct PDF link, and records any downloaded PDFs. It does not bypass logins or download restrictions. It does not claim that Lean Library, OpenAthens or any publisher has been tested: **a UJ account and local Chrome extension are needed for that test**.

### Prerequisites

- macOS with Chrome; R packages `chromote` and `jsonlite` (`install.packages(c("chromote", "jsonlite"))`).
- Configure [Lean Library](https://www.leanlibrary.com/) in a **dedicated Chrome profile**, select University of Johannesburg, and sign in interactively when required.
- Do not put credentials, cookies, Chrome profile data or PDFs in GitHub.
- Confirm relevant institutional/publisher terms permit sequential scripted retrieval. Slow requests alone do not establish permission.

### Start a dedicated Chrome instance

First close any other Chrome instances using the chosen profile, then run in a macOS Terminal:

```sh
mkdir -p "$HOME/.fulltexttest-chrome" "$HOME/fulltexttest-pdfs"
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
  --user-data-dir="$HOME/.fulltexttest-chrome" \
  --remote-debugging-address=127.0.0.1 \
  --remote-debugging-port=9222 \
  --no-first-run
```

Install the Lean Library extension **inside this separate profile**, select UJ and perform any OpenAthens/WAM login. Do not expose port 9222 on a public network.

Create a local CSV named `dois.csv`:

```csv
doi
10.1038/nature12373
```

Then run from the repository root:

```sh
Rscript R/uj_chrome_retrieval.R --input=dois.csv --output="$HOME/fulltexttest-pdfs" --mode=inspect
```

The default **inspect mode does not click or download anything**. It records page title, URL, and candidate PDF links for each DOI in a CSV manifest.

After inspecting results, optionally use `--mode=download`. This clicks a single candidate link on each page when the link appears to point to a PDF; this is experimental and will not cover every publisher or browser PDF viewer.

The script waits **30 seconds** between DOI visits by default (customise `--delay=30`). It never automates sign-in or MFA, and stops a record if the session leads to an access/login page. If `--mode=download` is enabled, only a download with a valid PDF signature is accepted.

### Limitations / next validation

1. Run on the user's Mac with a UJ-authenticated Chrome profile to confirm the Lean Library extension works under DevTools automation.
2. Check one permitted, subscribed DOI at each of three different publishers; verify access, extension resolution, and persistent login.
3. Check direct PDF navigation versus click/download under Chrome's PDF viewer settings. Some providers have no discoverable direct PDF link.
4. Add **publisher-specific adapters only where needed**, with terms/permissions review.
5. Only after local validation consider adding a W11 adapter. No changes to LivingEvidenceMap repositories.

This deliberately avoids storing subscription PDFs in GitHub or uploading them to Zenodo.
