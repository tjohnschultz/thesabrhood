# Backend rollout

## Current state: Phase 0, no cutover

The production site continues to use its existing GitHub Actions refreshes,
checked-in derived products, Quarto render, and GitHub Pages deployment. MLB,
Statcast, and FanGraphs research products remain available exactly as they are
today. No Supabase, Google Cloud, payment, or Netlify account is required for
this branch.

The new implementation is local-first and opt-in:

- Supabase migrations can run in local Docker.
- Retrosheet parsing has an offline fixture test.
- Terraform creates nothing with its defaults.
- Netlify configuration has no effect until a repository is linked.
- No existing scheduled workflow is disabled.

## Phase 1: local evaluation

1. Install Docker Desktop, Supabase CLI, Terraform, and R 4.4.
2. Run `supabase start`, `supabase db reset`, and `supabase test db`.
3. Run the Retrosheet fixture and one real season with `--dry-run`.
4. Ingest one season into local Supabase without `--publish`.
5. Review counts, checks, attribution, public views, member RLS, and rollback.

Exit criterion: the same staged release can be replayed idempotently and public
queries never expose a restricted provider.

## Phase 2: service and budget setup

Before spending:

1. Choose a Supabase region and paid/free tier based on measured local storage.
2. Create a Google Cloud project with a billing budget and alert thresholds.
3. Create a Netlify project from GitHub but leave the custom domain unchanged.
4. Choose a payment provider only after membership products and refund/support
   obligations are defined.
5. Put secrets in Supabase/Google/Netlify secret stores, never GitHub variables
   used by refresh jobs.

Exit criterion: a staged Retrosheet release succeeds in hosted Supabase from a
manually executed Cloud Run job.

## Phase 3: shadow production

1. Enable Cloud Scheduler with `publish_release = false`.
2. Compare hosted pipeline health against the existing site for at least two
   release cycles.
3. Deploy the existing `docs/` artifact to a Netlify preview URL.
4. Add read-only site components against `api.public_*` views.
5. Test Supabase Auth and manually assigned member entitlements; do not accept
   payments yet.

Exit criterion: backend failures do not change the live site, and the current
release pointer is proven recoverable.

## Phase 4: incremental source migration

Move one approved product family at a time. For every source:

1. Document terms or written permission in `ingest.providers`.
2. Add a provider adapter that produces the neutral contract.
3. Shadow-run it and compare outputs.
4. Add freshness and quality checks.
5. Explicitly approve its serving audience.

MLB/Statcast and FanGraphs remain blocked from database releases until this
process documents permission. Their current static research pages can remain
until a separate editorial/legal decision changes them.

## Phase 5: deployment cutover

Only after shadow operation:

1. Switch the custom domain from GitHub Pages to the validated Netlify site.
2. Remove schedule triggers and write permissions from the legacy refresh
   workflows product by product.
3. Keep GitHub Actions for tests and source-control validation.
4. Enable payment webhooks only after signature verification, replay tests,
   refunds, cancellation, and entitlement-revocation paths pass.

There is no all-at-once data migration or deployment step.
