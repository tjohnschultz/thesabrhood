# Backend rollout

## Current state: Phase 0, release boundary

The production site continues to use its existing GitHub Actions refreshes,
checked-in derived products, Quarto render, and GitHub Pages deployment. MLB,
Statcast, and FanGraphs research products remain available exactly as they are
today. No live service has been redirected.

The first migration step is deliberately smaller than the original cloud plan:

- GitHub remains the source-control system.
- `infrastructure/storage/` defines immutable private-state, public-data, and
  rendered-site release components.
- The local release-store test proves that a failed stage cannot promote a new
  current release.
- Google Cloud and Terraform are parked until measured scale justifies them.
- Supabase and Netlify are connected only in explicit later steps.
- Existing scheduled workflows remain unchanged during shadow testing.

## Phase 1: hosted storage setup

1. Measure the release components with `Rscript scripts/backend_release.R
   inspect`.
2. Create one hosted Supabase project; a local Supabase installation is not a
   prerequisite.
3. Create private storage buckets for pipeline state and release manifests.
4. Apply only the pipeline-run, release-metadata, and future preference tables
   needed for this phase.
5. Store service credentials as GitHub Actions secrets, never in the repository.
6. Upload one manually staged release without changing the current production
   pointer.

Exit criterion: the uploaded manifest and its checksums can restore the same
private state locally, while GitHub Pages remains unchanged.

## Phase 2: Actions with external state

1. Restore the current private snapshot from Supabase at workflow start.
2. Run the existing R acquisition, analysis, simulation, validation, and render.
3. Upload a new immutable release only after every existing health check passes.
4. Promote the release pointer after all objects are present.
5. Stop creating a uniquely named GitHub Actions data cache on every run.
6. Continue the existing checked-in deployment temporarily for comparison.

Exit criterion: at least two scheduled refresh cycles restore and publish state
without relying on a GitHub cache. The prior release remains usable after an
intentionally failed shadow run.

## Phase 3: Netlify preview and deployment cutover

1. Connect Netlify to the source repository without changing the custom domain.
2. Deploy the already validated `docs/` artifact to a Netlify preview URL.
3. Compare key pages, downloads, redirects, cache headers, and research tools
   with the live GitHub Pages site.
4. Change the refresh workflow to deploy the complete `docs/` artifact directly
   to Netlify instead of committing it.
5. Remove generated `docs/`, derived-data, include, and graphic updates from the
   automated Git commit only after rollback is proven.
6. Switch the custom domain in a separate, explicit operation.

Exit criterion: a failed data refresh leaves both the Supabase current-release
pointer and the last successful Netlify deployment unchanged.

## Phase 4: scheduler decision

Observe the smaller GitHub Actions job after its data and deployment burden is
removed. Keep Actions if it is reliable enough for the proof of concept. If it
is not, move the same release command to one Render cron job or a Cloud Run job;
no data migration should be required to change schedulers.

Exit criterion: the selected scheduler meets the measured refresh time,
frequency, logging, retry, and cost requirements.

## Phase 5: accounts and incremental source migration

Add Supabase Auth and preference tables for followed teams, players, dashboard
layout, and notification choices before adding payments.

Move one approved product family at a time. For every source:

1. Document terms or written permission in `ingest.providers`.
2. Add a provider adapter that produces the neutral contract.
3. Shadow-run it and compare outputs.
4. Add freshness and quality checks.
5. Explicitly approve its serving audience.

MLB/Statcast and FanGraphs remain blocked from database releases until this
process documents permission. Their current static research pages can remain
until a separate editorial/legal decision changes them.

## Later scale-up

The existing canonical PostgreSQL, Retrosheet, Cloud Run, and Terraform
foundation remains available if the proof of concept outgrows the simplified
pipeline. Payments remain disabled until data permissions, authentication,
entitlements, refunds, cancellation, and webhook replay behavior are complete.

There is no all-at-once data migration or deployment step.
