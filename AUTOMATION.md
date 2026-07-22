# Daily publishing design

## What is automated now

The production workflow in `.github/workflows/daily-data-refresh.yml` runs on
GitHub Actions each morning at 12:00 UTC and can also be started manually. It
runs `scripts/update_pbp.R`, whose `update_pbp()` function incrementally adds
only final games through the prior completed date. Raw events are stored under
`.private-data/` in the private Actions cache, are ignored by Git, and never
enter `data/` or `docs/`. If that cache expires, the job rebuilds the completed
regular-season history from the MLB Stats API.

The same job refreshes FanGraphs season tables, bullpen availability, the
current schedule, probable starters, posted batting orders, active rosters, and
Open-Meteo park forecasts. Team simulations can run before lineups post. Player
simulations wait for a current complete batting order, and the site displays a
lineup-pending state instead of relabeling yesterday's player board.

After acquisition, the workflow updates the derived-data manifest, regenerates
the Quarto fragments, validates and renders the site, then commits only compact
derived products, fragments, and rendered pages to `main`. A failed acquisition
or validation produces no public commit.

The branch workflow in `.github/workflows/site-preview.yml` regenerates all
static fragments, validates approved data files and checksums, renders Quarto,
validates required pages, and uploads a preview artifact. It has no deployment
permission.

The repository acquisition layer now has executable jobs for the daily MLB schedule,
probable starters, posted batting orders, active rosters, active-roster bullpen
filtering, ballpark weather, Triple-A performance lines, and five-game pitch-mix
change detection. The reusable package source lives in `packages/sabrhoodR`, so
GitHub Actions installs the same functions used during local development.

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
