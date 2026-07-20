# Daily publishing design

## What is automated now

The branch workflow in `.github/workflows/site-preview.yml` regenerates all
static fragments, validates approved data files and checksums, renders Quarto,
validates required pages, and uploads a preview artifact. It has no deployment
permission.

## Production workflow contract

A scheduled production workflow should be added only after private data storage
is chosen. It needs four independently failing stages:

| Stage | Input | Output | Failure behavior |
|---|---|---|---|
| Ingest | Private prior snapshot + MLB updates | Versioned raw snapshot | Preserve prior snapshot |
| Analyze | Raw snapshot + `sabrhoodR` | Derived CSV products | Publish nothing |
| Editorial | Derived products | Story queue + newsletter | Mark edition for review or fall back to last edition |
| Publish | Approved derived products | Quarto `docs` artifact | Keep last successful Pages deployment |

## Minimum checks before publish

- Source-through date is expected and never moves backward.
- Game and pitch identifiers are unique at their documented grain.
- Row counts stay inside season-aware tolerance bands.
- All 24 RE24 base-out states exist.
- Recent-form windows do not overlap their baselines.
- Pregame products contain no post-cutoff information.
- Public data contains no raw PBP, secrets, or local absolute paths.
- Every generated page and required link exists.

## Newsletter rhythm

The deterministic daily edition can cycle through a weekly desk:

- Monday: team and divisional race pulse
- Tuesday: hitters and award-race movement
- Wednesday: pitch shape and arsenal changes
- Thursday: rotation and bullpen decisions
- Friday: matchup and weekend-series preview
- Saturday: Triple-A prospects and call-up candidates
- Sunday: week in review and historical anniversaries

Automated text should remain concise and evidence-led. The story score selects
candidates; editorial rules decide which claims are publishable.
