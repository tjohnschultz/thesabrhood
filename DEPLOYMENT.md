# Deployment state for thesabrhood.com

The Quarto site is live from `main/docs` through GitHub Pages. Daily content is
published by GitHub Actions only after acquisition, analysis, simulation,
freshness, source, and rendered-output checks pass.

## Transition guardrail

This remains the live deployment until a later, explicit cutover. The repository
includes an inert Netlify configuration, a local release-storage contract, and
unapplied Supabase/Google Cloud foundations. No live resource has been created
or redirected by the repository. The active plan first moves changing pipeline
state out of GitHub Actions cache, then lets Netlify publish the already
validated `docs/` artifact so the existing design and research archive are
preserved. Google Cloud is not required for this proof-of-concept phase.

See `infrastructure/ROLLOUT.md` for the phased account, shadow-run, and domain
cutover plan.

## Automated publication layers

1. **Frontend publication:** a push to `main` that changes Quarto pages,
   article sources, site CSS, navigation, or static design assets automatically
   renders and validates the complete site. The workflow commits the resulting
   `docs/` files to `main`, which gives the connected static host a deployable
   site without requiring a local render.
2. **Morning refresh:** completed-game PBP, full PBP analysis, FanGraphs,
   Triple-A, history, editorial products, current slate inputs, simulations,
   graphics, fragments, and rendered pages.
   League off-days still publish current reporting, history, and graphics with
   a clearly labeled no-games simulation state.
3. **Intraday refresh:** approaching game waves are checked every 30 minutes;
   posted orders and probables trigger new simulations and prediction snapshots.
4. **Weekly intelligence refresh:** cumulative FanGraphs award checkpoints,
   race timelines, graphics, and affected pages are rebuilt every Sunday.
5. **Projection feedback:** the first eligible pregame forecast for each game
   and model version is settled against final game and player outcomes.

## How to know a change is live

A source edit is not live merely because it is saved locally. It passes through
four observable states:

1. `quarto render` (or `scripts/build_site.ps1`) updates the local `docs/`
   preview.
2. The source change is committed and merged into `main`.
3. GitHub Actions shows **Render and publish frontend** as successful. Its
   artifact contains the exact rendered site, and `docs/build-info.json` records
   the source commit and render time.
4. The connected GitHub Pages or Netlify deployment for the bot's rendered
   commit reports **Published**. At that point the public site has the change;
   a hard refresh bypasses an older browser copy.

Supabase publication updates data releases. It does not deploy Quarto, CSS, or
article-template changes; those always use the frontend publication workflow.

## Publication safety

- Raw PBP, source snapshots, fitted models, and projection ledgers stay private.
- `data/derived/refresh-health.csv` declares freshness for each public group.
- A failed pull, stale product, checksum mismatch, failed render, or leaked local
  path prevents the bot commit; the prior Pages version remains live.
- Actions cache is replaceable rather than permanent. Before commercial launch,
  raw snapshots and fitted models should move to durable private storage.

## Next applications

The public refresh system is the shared data foundation for two separate builds:

- **Analytics Lab:** a private Shiny research environment for player, pitch,
  team, matchup, trend, table, summary, and downloadable-graphic tools.
- **Broadcast Studio:** a private Shiny packet builder that accepts game and
  editorial inputs and exports reviewed offline HTML and PDF packets.

Both applications should consume `sabrhoodR` and the website's data contracts
rather than duplicating acquisition logic.
