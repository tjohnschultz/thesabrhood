# Release storage boundary

This directory defines the first, local-first step away from using Git as The
SABRhood's changing data store. It does not contact Supabase, change a scheduled
workflow, or deploy the site.

## The three release components

| Component | Purpose | Current inputs |
| --- | --- | --- |
| `private_state` | Durable inputs needed by the next refresh | PBP snapshots, provider checkpoints, projection ledger, and models under `.private-data/` |
| `public_data` | Validated products used to construct the site | `data/derived/` and `images/graphics-feed/` |
| `site` | Complete static artifact served to visitors | `docs/` |

Old checkout copies, legacy restoration archives, package libraries, and other
replaceable local material are deliberately excluded from `private_state`.

## Stage, validate, then promote

A refresh must never update the live pointer while files are still being
written:

1. The pipeline builds and validates its normal outputs.
2. `stage` copies the approved inputs into an immutable release directory.
3. Every file is recorded in `manifest.json` with its byte size and SHA-256
   checksum.
4. `promote` changes `current.json` only after the release is complete.
5. A failed stage leaves the prior `current.json` unchanged.

The local implementation writes to `.backend/release-store/`, which is ignored
by Git. Hosted Supabase Storage will later implement the same layout with
versioned object keys. PostgreSQL will remain the authority for pipeline health,
account data, preferences, and the atomic current-release record.

The Supabase adapter splits objects larger than 40 MiB before upload. Each part
has its own SHA-256 checksum in the remote manifest, so the current 72 MiB PBP
snapshot can use the hosted free-tier file-size limit without changing the R
analysis format.

## Local commands

Run these from the repository root with the project's R library available:

```powershell
Rscript scripts/backend_release.R inspect
Rscript scripts/backend_release.R stage --release-key=manual-test-1
Rscript scripts/backend_release.R promote --release-key=manual-test-1
```

`inspect` is read-only. `stage` copies substantial data and should be used only
when a complete local snapshot is wanted. None of these commands uploads,
deletes, or untracks current site files.

Run the offline contract test with:

```powershell
Rscript infrastructure/storage/tests/run-tests.R
```

## Supabase connection

The private bucket is named `pipeline-releases`. Network commands read
credentials only from the process environment:

- `SABRHOOD_SUPABASE_URL`: the hosted project URL.
- `SABRHOOD_SUPABASE_SECRET_KEY`: a backend-only `sb_secret_` key.
- `SABRHOOD_SUPABASE_BUCKET`: optional; defaults to `pipeline-releases`.

The secret key must never be committed, placed in a public page, or pasted into
logs. A safe first network operation writes one small connection-test object:

```powershell
Rscript scripts/supabase_storage.R probe
```

Uploading a release and promoting it are deliberately separate:

```powershell
Rscript scripts/supabase_storage.R upload --release-key=KEY
Rscript scripts/supabase_storage.R promote --release-key=KEY
```

An interrupted upload cannot change `current.json` because only the explicit
second command writes that pointer.
