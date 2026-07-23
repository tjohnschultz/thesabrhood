# Deployment state for thesabrhood.com

The Quarto site is live from `main/docs` through GitHub Pages. Daily content is
published by GitHub Actions only after acquisition, analysis, simulation,
freshness, source, and rendered-output checks pass.

## Transition guardrail

This remains the live deployment until a later, explicit cutover. The repository
now includes an inert-by-default Netlify configuration and unapplied Google
Cloud/Supabase infrastructure, but no accounts are required and no cloud
resources have been created. Netlify initially publishes the already validated
`docs/` artifact so the existing design and research archive are preserved.

See `infrastructure/ROLLOUT.md` for the phased account, shadow-run, and domain
cutover plan.

## Automated publication layers

1. **Morning refresh:** completed-game PBP, full PBP analysis, FanGraphs,
   Triple-A, history, editorial products, current slate inputs, simulations,
   graphics, fragments, and rendered pages.
   League off-days still publish current reporting, history, and graphics with
   a clearly labeled no-games simulation state.
2. **Intraday refresh:** approaching game waves are checked every 30 minutes;
   posted orders and probables trigger new simulations and prediction snapshots.
3. **Weekly intelligence refresh:** cumulative FanGraphs award checkpoints,
   race timelines, graphics, and affected pages are rebuilt every Sunday.
4. **Projection feedback:** the first eligible pregame forecast for each game
   and model version is settled against final game and player outcomes.

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
