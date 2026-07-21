# The SABRhood site build

The public website is a Quarto site generated from compact derived CSV products.
Raw play-by-play and private research inputs are not deployed with the site.

## Local build

From `Package Construction`, refresh the representative simulation products and
approved public-data mirror:

```powershell
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_projection_artifacts.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_pitch_trend_artifacts.R
$env:SABRHOOD_DATE = (Get-Date -Format "yyyy-MM-dd")
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/fetch_daily_baseballr_artifacts.R
$env:SABRHOOD_SEASON = (Get-Date -Format "yyyy")
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/fetch_aaa_artifacts.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/fetch_fangraphs_season_source.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_fangraphs_award_artifacts.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_league_trend_award_artifacts.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_graphics_feed_artifacts.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_player_projection_artifacts.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/sync_site_artifacts.R
```

Then, from the `thesabrhood-site` repository root:

```powershell
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/build_site_fragments.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/validate_site.R
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render --no-clean
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/restore_legacy_assets.R
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" scripts/validate_site.R --rendered
```

The rendered homepage is `docs/index.html`.

The `--no-clean` option preserves the already-rendered legacy research articles.
Those article sources still depend on their original R environments and will be
migrated into the new data contract separately.

For a local web server with automatic refresh:

```powershell
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" preview
```

## Safe automation

`.github/workflows/site-preview.yml` rebuilds the fragments, validates the data
contract, renders the site, and uploads the complete `docs` folder as a workflow
artifact. It does not deploy to GitHub Pages or change `thesabrhood.com`.

## Intended daily pipeline

1. Update the private pitch-by-pitch store.
2. Run the package artifact targets.
3. Assemble each game input contract and run the daily score simulations.
4. Build league trend windows and the rotating Insane Baseball Awards candidate pool.
5. Validate data grains, dates, row counts, method versions, and projection
   input completeness.
6. Copy only approved derived CSV products into `data/derived`.
7. Generate static HTML fragments.
8. Render and validate the Quarto site.
9. Publish only after every earlier stage succeeds.

The daily input job uses `baseballr::mlb_game_pks()`, `mlb_probables()`,
`mlb_batting_orders()`, and `mlb_rosters(roster_type = "active")`. Park
coordinates map each venue to an Open-Meteo hourly game window. The Triple-A
job uses `mlb_stats(..., sport_ids = 11)` and labels its output as a performance
watch rather than a scouting or prospect ranking.
