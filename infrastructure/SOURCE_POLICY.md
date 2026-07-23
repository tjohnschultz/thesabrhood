# Source permission policy

The serving database uses a deny-by-default registry in
`ingest.providers`. A source is eligible for an atomic public/member release
only when all three are true:

1. `permission_status = 'approved'`
2. `commercial_serving_allowed = true`
3. `permission_documented_at` is present

`publishing.publish_release()` enforces the rule inside the same transaction
that moves the current-release pointer.

## Initial classifications

| Provider | Database serving | Reason |
|---|---:|---|
| Retrosheet | Allowed with attribution | Its published use notice allows commercial products and requires a specified attribution. |
| MLB / Statcast | Blocked | Permission for a commercial serving layer has not been documented. |
| FanGraphs | Blocked | Permission for a commercial serving layer has not been documented. |
| The SABRhood | Allowed conditionally | First-party material is allowed only when every upstream source is separately eligible. |

The existing static website research products are not imported into the new
database by this work. That preserves the current site while preventing an
automatic expansion into a commercial API/member data product.

## Approval record

When permission changes, update the provider in a new migration. Include:

- the authoritative terms URL or agreement reference;
- the effective date and reviewer;
- allowed audiences and redistribution limits;
- required attribution;
- retention/deletion restrictions; and
- whether derived aggregates are treated differently from raw data.

Do not edit a hosted row manually without a matching migration and review.
