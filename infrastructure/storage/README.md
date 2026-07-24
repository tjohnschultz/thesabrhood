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
