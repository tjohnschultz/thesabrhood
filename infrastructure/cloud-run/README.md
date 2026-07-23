# Containerized R ingestion job

The image currently implements one safe first slice: Retrosheet regular-season
historical ingestion. Provider output is checked against a neutral games/events
contract before any database connection is opened.

This does not replace the existing MLB/FanGraphs GitHub workflows yet.

## Local dry run

With R packages `digest` and `jsonlite` installed:

```powershell
Rscript infrastructure/cloud-run/entrypoint.R retrosheet --season=2025 --dry-run
```

For an offline fixture:

```powershell
Rscript infrastructure/cloud-run/tests/run-tests.R
```

## Database staging

Point `DATABASE_URL` at local Supabase, then omit `--dry-run`. A database run
ingests and stages but does not publish:

```powershell
$env:DATABASE_URL = "postgresql://postgres:postgres@127.0.0.1:54322/postgres"
Rscript infrastructure/cloud-run/entrypoint.R retrosheet --season=2025
```

Add `--publish` only after reviewing release checks. The database function still
enforces source permissions and fails atomically if any source is restricted.

## Container

Build from the repository root so the Docker context matches `COPY` paths:

```powershell
docker build -f infrastructure/cloud-run/Dockerfile -t sabrhood-backend .
docker run --rm sabrhood-backend help
```

Secrets are runtime environment variables. They are never build arguments or
image layers.
