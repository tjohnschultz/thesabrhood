# Authentication and payments boundary

Supabase Auth is the identity authority. The migration creates a private profile
after signup and keeps subscription/customer records outside the exposed API
schema. Member serving views rely only on active `member` entitlements, not on
untrusted browser claims or payment-provider metadata.

The billing model is provider-neutral:

- `billing.customer_accounts` maps a user to a provider customer.
- `billing.subscriptions` stores normalized subscription state.
- `billing.webhook_events` supplies an idempotency key and audit payload.
- `billing.entitlements` is the sole access-control input.

No payment provider has been selected or connected. A future webhook handler
must verify the provider signature against the raw body, insert the event
idempotently, update subscription state, and recompute entitlements in one
database transaction. Browser clients must never write these tables.

Before accepting payments, test duplicate and out-of-order events, trial expiry,
failed payment, cancellation at period end, immediate refund, chargeback,
account deletion, and manual support overrides.

