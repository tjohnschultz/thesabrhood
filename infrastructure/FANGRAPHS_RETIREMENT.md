# FanGraphs retirement plan

FanGraphs is no longer an automated or publication-blocking provider for The
SABRhood. The last validated snapshot remains available only as a dated legacy
reference while its consumers are replaced with first-party transformations of
the existing PBP, Statcast, roster, schedule, lineup, and probable-starter
inputs.

## Active boundary

- The daily MLB/PBP workflow remains the canonical publication workflow.
- Lineups and probable starters come from MLB game feeds through BaseballR's
  `mlb_batting_orders()` and `mlb_probables()` adapters; they do not come from
  FanGraphs.
- `fangraphs_season` and `award_race_history` remain visible in
  `refresh-health.csv` but are classified as `legacy_reference` and do not
  block publication.
- FanGraphs reconciliation and history workflows are manual-only. They must not
  be scheduled or used to satisfy a daily freshness contract.
- A manual run must still pass the existing row-count and aggregate regression
  checks before it can replace the retained snapshot.

## Incremental replacement order

1. Build provider-neutral current-season hitter and pitcher tables from the
   PBP-derived game lines and performance summaries.
2. Move the daily team simulator, player simulator, and matchup models to those
   canonical tables.
3. Replace FanGraphs award, positional-value, and player-market products with
   documented SABRhood metrics derived from batting, pitching, baserunning, and
   fielding run values.
4. Cut the races, projections, newsletter, team, and player page fragments over
   without changing their established layout.
5. Remove the retained FanGraphs snapshot and manual workflows after no public
   or private consumer references them.

## Naming and publication policy

In-house metrics must not be presented as FanGraphs WAR or wRC+. New measures
will use SABRhood-specific names and versioned methods, with components and
validation published alongside the result. MLB/Statcast-derived data remains
outside a commercial serving layer until documented permission exists.
