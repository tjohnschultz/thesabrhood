# The SABRhood backend foundation

This is an introductory backend that can be adopted one product at a time
without disturbing the current site. The active proof-of-concept path keeps
GitHub for source control, moves durable refresh state into hosted storage, and
deploys the validated static site to Netlify.

## Components

- `../supabase/`: canonical PostgreSQL migration, source permission registry,
  Supabase Auth membership model, RLS, release checks, and serving views.
- `storage/`: the smaller release boundary used to move changing data out of
  Git before any scheduler migration.
- `cloud-run/`: provider-neutral R adapter contract, Retrosheet implementation,
  offline test fixture, database writer, and optional future container.
- `terraform/`: parked, inert-by-default Google Cloud templates for a later
  scale-up; they are not required for the proof-of-concept migration.
- `ROLLOUT.md`: account, budget, shadow, and incremental cutover gates.
- `SOURCE_POLICY.md`: enforceable MLB/FanGraphs/Retrosheet serving boundary.
- `AUTH_AND_PAYMENTS.md`: provider-neutral auth, webhook, and entitlement
  boundary.

## Non-goals of this phase

- No hosted services or paid accounts are created by repository code.
- No custom domain or deployment is switched.
- No existing GitHub refresh schedule is removed.
- No MLB/Statcast or FanGraphs data is copied into the serving database.
- No payment provider is selected and no payments are accepted.
