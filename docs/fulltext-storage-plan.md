# Long-term full-text storage plan

## Retrieval

The daily OpenAlex workflow retrieves at most 100 OpenAlex cached content files per UTC day, matching the free API-key allowance documented by OpenAlex. It prefers TEI XML (`has_content.grobid_xml`) and otherwise retrieves PDF, so we never deliberately download both representations for the same DOI in one day.

## Durable storage

GitHub Actions artifacts are **staging/audit storage only**. They are not the corpus: GitHub Free includes 500 MB of artifact storage, and artifact retention is configurable but finite.

Each successful daily batch is compressed into one archive and uploaded to a **restricted Zenodo record**. The Zenodo record stores:

- the compressed full-text bundle;
- the DOI/OpenAlex ID for each work;
- OpenAlex metadata needed for provenance;
- representation (TEI XML or PDF);
- retrieval timestamp;
- SHA-256 checksum;
- batch/run identifier.

The workflow advances its GitHub checkpoint **only after the Zenodo upload and publication succeeds**. A failed storage operation therefore cannot silently mark retrieved text as complete.

Zenodo is appropriate here because it supports restricted files, persistent records/DOIs, and 50 GB per record by default, with additional quota available. Restricted files are not publicly accessible; metadata remains public.

## Copyright/licensing

OpenAlex explicitly states that cached PDFs retain their original copyright and that OpenAlex grants no additional rights. Therefore the default Zenodo visibility is **restricted**, not public. We should only make individual material publicly available where the source licence permits redistribution.

## Digestion layer

The retrieval archive is the source corpus. Analysis/digestion workflows should read from the archived batch and write derived products separately, e.g.:

- normalised text;
- section/chunk records;
- extraction tables;
- embeddings/indexes;
- analysis results.

Derived products can be stored independently and regenerated from the immutable source archive using the recorded SHA-256 checksums.

## GitHub's role

GitHub contains code, manifests, small checkpoint/provenance files and workflow audit records. It does **not** contain the full-text corpus in Git history.
