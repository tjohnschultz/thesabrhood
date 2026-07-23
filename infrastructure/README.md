# The SABRhood backend foundation

This is an introductory, local-first backend that can be adopted one product at
a time without disturbing the current site.

## Components

- `../supabase/`: canonical PostgreSQL migration, source permission registry,
  Supabase Auth membership model, RLS, release checks, and serving views.
- `cloud-run/`: provider-neutral R adapter contract, Retrosheet implementation,
  offline test fixture, database writer, and container.
- `terraform/`: inert-by-default Google Cloud Run, Scheduler, Artifact Registry,
  IAM, and Secret Manager configuration.
- `ROLLOUT.md`: account, budget, shadow, and incremental cutover gates.
- `SOURCE_POLICY.md`: enforceable MLB/FanGraphs/Retrosheet serving boundary.
- `AUTH_AND_PAYMENTS.md`: provider-neutral auth, webhook, and entitlement
  boundary.

## Non-goals of this phase

- No hosted services or paid accounts are created.
- No custom domain or deployment is switched.
- No existing GitHub refresh schedule is removed.
- No MLB/Statcast or FanGraphs data is copied into the serving database.
- No payment provider is selected and no payments are accepted.

