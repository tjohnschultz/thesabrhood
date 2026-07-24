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

Before upload, each component is compressed into one `tar.gz` package. This
reduces hundreds of small files to three component packages plus two manifests.
The package retains the original folder layout and the local manifest retains
per-file checksums, so restoration remains verifiable.

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
Rscript scripts/supabase_storage.R verify --release-key=KEY
Rscript scripts/supabase_storage.R restore --release-key=KEY --target=PATH
Rscript scripts/supabase_storage.R inventory
Rscript scripts/supabase_storage.R promote --release-key=KEY
```

An interrupted upload cannot change `current.json` because only the explicit
second command writes that pointer. Staged package objects are retryable: a
second `upload` for the same locally immutable release replaces only those
unpromoted objects with the same checksummed content.

`verify` downloads the staged objects into temporary space, reconstructs chunked
packages, and checks part, package, and local-release SHA-256 values. Temporary
verification files are removed after the command. `promote` repeats this
verification before it is allowed to update `current.json`.

`restore` defaults to the `private_state` component and refuses to use a target
that already exists. It downloads and verifies the remote manifest, local
manifest, and only the selected component packages. It safely checks every
archive path, validates every extracted file, builds a new staging directory,
and renames that directory into place only after the restore is complete. This
isolated behavior is the precursor to replacing the Actions cache.

`inventory` is read-only. It lists release folders through the Storage API,
reads their small remote manifests, and reports the number of files, objects,
and manifest-declared bytes in each staged release. An incomplete release with
no readable remote manifest is reported as `unreadable` and is never silently
treated as safe to delete.

## GitHub Actions shadow restore

The manual **Validate backend contracts** workflow includes a **Supabase shadow
round trip** job that proves a fresh GitHub runner can recover private pipeline
state without relying on the Actions cache. It then stages, uploads, and
checksum-verifies a new uniquely named release without promoting it. The
network job runs only for a manual dispatch or the branch-restricted bootstrap
marker; ordinary pushes and pull requests continue to run only the offline
contract checks. It requires these repository secrets:

- `SABRHOOD_SUPABASE_URL`
- `SABRHOOD_SUPABASE_SECRET_KEY`
- `SABRHOOD_SUPABASE_BUCKET`

Run **Validate backend contracts** from the Actions tab and select the
`codex/backend-overhaul` branch. The shadow job currently restores the staged
`shadow-20260724-002` release into the ignored `.private-data/` directory, then
creates a `shadow-gh-RUN_ID-RUN_ATTEMPT` release. The workflow has read-only
repository permissions and checks out without persisting Git credentials. It
does not upload a GitHub artifact, save an Actions cache, commit files, deploy
the site, promote a release, or write `current.json`.

GitHub does not display the manual-run button until a workflow exists on the
default branch. Before this workflow is merged, a bootstrap test can be
requested by pushing to `codex/backend-overhaul` with `[supabase-shadow]` in the
commit message. That marker is ignored on every other branch and for pull
requests. A read-only storage inventory can be requested with
`[supabase-inventory]`; that marker does not run the round-trip upload.
