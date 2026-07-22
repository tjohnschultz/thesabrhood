# The SABRhood site build

The public website is a Quarto site generated from compact derived CSV products.
Raw play-by-play, source snapshots, fitted models, and prediction ledgers remain
under `.private-data/` and are never deployed.

## Production build order

GitHub Actions installs `packages/sabrhoodR` and runs these layers:

1. Update completed-game PBP through the prior day.
2. Settle previously archived pregame projections.
3. Rebuild PBP summaries, form, splits, pitch trends, discipline, manager hooks,
   league trends, unusual awards, run expectancy, and bullpen availability.
4. Refresh FanGraphs, history, editorial, matchup, team, and Triple-A products.
5. Pull the current schedule, probables, orders, active rosters, and weather.
6. Run team and available player simulations and archive eligible predictions.
7. Rebuild the branded graphics feed.
8. Enforce `refresh-health.csv`, update checksums, generate fragments, validate,
   render, restore article assets, and validate the rendered site.
9. Commit only approved public products, graphics, fragments, and `docs`.

`daily-slate-status.csv` makes scheduled-game and league-off-day runs explicit.
Off-days still complete steps 1-4 and 7-9; simulation fragments switch to a
no-games state instead of exposing the retained prior slate.

The weekly workflow separately rebuilds cumulative FanGraphs award checkpoints
and race timelines. The intraday workflow refreshes posted lineups and reruns
pregame projections near first pitch.

## Local validation

From the repository root, with `sabrhoodR`, BaseballR, Lahman, and the declared
R dependencies installed:

```powershell
$rscript = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
& $rscript scripts/build_pbp_products.R
& $rscript scripts/build_history_products.R
& $rscript scripts/build_editorial_products.R
& $rscript scripts/build_graphics_feed.R
& $rscript scripts/build_refresh_health.R
& $rscript scripts/update_derived_manifest.R
& $rscript scripts/build_site_fragments.R
& $rscript scripts/validate_site.R
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render --no-clean
& $rscript scripts/restore_legacy_assets.R
& $rscript scripts/validate_site.R --rendered
```

The rendered homepage is `docs/index.html`. `--no-clean` preserves the imported,
pre-rendered research archive whose older notebooks still depend on one-off
personal analysis environments.

## Public data boundary

The website consumes only `data/derived`, generated HTML fragments, and branded
images. The freshness report declares source date, expected date, cadence, lag,
and status for every time-sensitive public product group. A failed freshness or
validation gate leaves the prior Pages deployment live.
