# Local-first Supabase database

This directory is the first backend slice. It does not require a hosted
Supabase account and does not change the current website refresh.

## What the migration provides

- Immutable provider batches and raw records.
- A small canonical baseball model for teams, people, games, and game events.
- Staged releases with one transactionally switched production pointer.
- Pipeline-run and release-check history.
- RLS-protected public and member serving views in the exposed `api` schema.
- Supabase Auth profiles plus provider-neutral subscription and entitlement
  tables.
- A hard source-permission gate. MLB and FanGraphs are blocked from database
  publication until their records are deliberately updated with documented
  permission. Retrosheet is approved with its required attribution.

The existing static MLB- and FanGraphs-derived pages are not copied into this
database by these migrations.

## Optional local setup

After installing Docker Desktop and the Supabase CLI:

```powershell
supabase start
supabase db reset
supabase test db
```

No hosted project is created by these commands. The local API exposes only the
`api` schema.

## Release invariant

Writers build a complete release under a new UUID. They record validation
checks, then call `publishing.publish_release(release_id)` as `service_role`.
The function locks the single publication pointer and switches it only after
the source-permission and minimum-validation gates pass. Readers therefore see
the old complete release or the new complete release, never a partial update.

