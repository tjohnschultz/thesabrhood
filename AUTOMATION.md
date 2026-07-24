# Daily publishing design

## Backend transition status

The existing GitHub Actions schedules remain the production refresh system
during the local-first backend evaluation. They continue to publish the current
MLB/Statcast- and FanGraphs-derived static research products. No refresh workflow
has been disabled or redirected.

The active successor path lives in `infrastructure/storage/`. It first separates
durable private state, validated public data, and the rendered site into an
immutable release contract. Hosted Supabase storage and a Netlify preview are
then introduced while GitHub Actions remains the scheduler. Cloud Run,
Terraform, and a scheduler migration are optional later steps rather than
proof-of-concept prerequisites. The canonical PostgreSQL and provider-neutral
foundation remains under `supabase/` and `infrastructure/cloud-run/` for that
later scale-up. MLB and FanGraphs remain blocked from new commercial serving
views until permission is documented.

## What is automated now

The production workflow in `.github/workflows/daily-data-refresh.yml` runs on
GitHub Actions each morning at 12:00 UTC and can also be started manually. It
runs `scripts/update_pbp.R`, whose `update_pbp()` function incrementally adds
only final games through the prior completed date. Raw events are stored under
`.private-data/` in the private Actions cache, are ignored by Git, and never
enter `data/` or `docs/`. If that cache expires, the job rebuilds the completed
regular-season history from the MLB Stats API.

The same job rebuilds the complete public intelligence layer from the PBP cache:
player summaries, splits, recent form, change z-scores, pitch identities,
discipline, rolling league trends, unusual awards, manager hooks, run expectancy,
matchups, team intelligence, stories, milestones, and broadcast notes. It also
refreshes FanGraphs, Triple-A, the daily slate, rosters, bullpens, park weather,
simulations, and branded graphics.

On a league off-day, the workflow records an explicit current-date
`no_games_scheduled` slate status, refreshes the non-game intelligence, and
publishes an off-day presentation. It never relabels the previous slate or its
probabilities as current.

Team simulations can run before lineups post. Player simulations wait for a
current complete batting order. Every eligible pregame build is archived
privately, and completed-game PBP settles those forecasts the following morning.

The separate `.github/workflows/lineup-refresh.yml` workflow checks every 30
minutes from 10:00 a.m. through 10:30 p.m. Eastern during daylight-saving time,
from March through November.
It exits before installing data dependencies unless a current-date game is
within four hours of first pitch (or began less than 20 minutes ago). During an
active game wave it refreshes only probable starters and posted batting orders,
then rebuilds team and available player projections and archives the freshest
eligible snapshot. It deliberately does not
repeat the PBP, FanGraphs, roster, bullpen-workload, or full weather acquisition
jobs. The four-hour gate captures orders that post earlier than the usual
90-minute window, and repeated checks can capture subsequent lineup changes.

Prior-day orders are never presented as confirmed current orders. Until a
separately labeled projected-lineup model is built, missing orders keep the
player board gated while the team model uses its documented season-level
fallbacks.

The weekly intelligence workflow rebuilds cumulative FanGraphs award checkpoints
and race timelines each Sunday. The daily workflow writes `refresh-health.csv`,
which compares every public group with its declared daily or weekly cadence.
Only after that gate passes does it update checksums, regenerate fragments,
validate, render, and commit compact products, graphics, fragments, and pages.
A failed acquisition or validation produces no public commit.

The branch workflow in `.github/workflows/site-preview.yml` regenerates all
static fragments, validates approved data files and checksums, renders Quarto,
validates required pages, and uploads a preview artifact. It has no deployment
permission.

The eight imported research notebooks remain checked-in, pre-rendered archive
pages. They are excluded from unattended full-site renders because their old
chunks depend on personal files and one-off analysis environments. The preview
still verifies that every archive HTML page and its legacy table assets exist.
New or substantially revised articles should be rendered deliberately when
their analysis inputs are available, reviewed, and then committed with the
resulting HTML; the daily data workflows do not rerun editorial notebooks.

The private cache carries replaceable PBP, FanGraphs checkpoint, and projection
ledger state together. Only compact derived products are committed. The reusable
package source lives in `packages/sabrhoodR`, so Actions installs the same
functions used during local development.

The season layer also pulls compact FanGraphs hitter and pitcher leaderboards
through BaseballR. Wide source tables stay in the private cache; only selected
season columns, transparent award-watch scores, and the validated graphics
manifest enter the public mirror.

## Production workflow contract

The scheduled workflow implements the acquisition, simulation, and static
publishing spine. GitHub Actions cache is replaceable rather than the sole
permanent archive: a missing cache triggers a completed-game rebuild. Long-term
model snapshots should still move to durable private storage before commercial
launch. Its independently failing stages are:

| Stage | Input | Output | Failure behavior |
|---|---|---|---|
| Ingest | Private prior snapshot + MLB updates | Versioned raw snapshot | Preserve prior snapshot |
| Analyze | Raw snapshot + `sabrhoodR` | Derived CSV products | Publish nothing |
| Simulate | Schedule + approved pregame inputs | Game draws + projection summaries | Withhold incomplete games |
| Editorial | Derived products | Story queue + newsletter | Mark edition for review or fall back to last edition |
| Publish | Approved derived products | Quarto `docs` artifact | Keep last successful Pages deployment |

## Minimum checks before publish

- Source-through date is expected and never moves backward.
- Game and pitch identifiers are unique at their documented grain.
- Row counts stay inside season-aware tolerance bands.
- All 24 RE24 base-out states exist.
- Recent-form windows do not overlap their baselines.
- Pregame products contain no post-cutoff information.
- Every projected game records schedule, starter, lineup, park, weather, and
  roster status; missing required inputs are visible and cannot silently become
  a public probability.
- Active-roster pulls retain the acquisition date and MLBAM player IDs; bullpen
  candidates must survive that join and probable starters are excluded.
- Retractable-roof games keep the forecast and an explicit roof-decision note;
  fixed-dome games are marked indoors instead of receiving fake outdoor impact.
- Triple-A cards are age-and-performance leads, not prospect ranks, and preserve
  plate-appearance or innings thresholds.
- The bullpen chain is rebuilt after confirmed lineups and whenever recent
  workload changes; every step retains the top alternatives and its selector
  method instead of publishing an unexplained reliever choice.
- Win probabilities sum to one, all simulated ties are resolved, and the model
  version and simulation count are attached to every game.
- The daily backtest archive is written before the slate is replaced, so misses
  and calibration can be audited by model version.
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
