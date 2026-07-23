# Updating The SABRhood locally

The local updater turns the existing R pipeline into one command. It updates
private source data, rebuilds the public derived products, runs the simulations,
regenerates the graphics and HTML fragments, validates the outputs, and renders
the complete Quarto site.

The finished website is in `docs`, not `_site`.

## One-time setup on this computer

Before the first refresh, install the exact local dependencies used by the
pipeline:

```powershell
.\scripts\setup_local_refresh.ps1
```

This installs the required R packages, the pinned `baseballr` version, and the
current local `sabrhoodR` package into the project's private R library. It does
not update or publish the website. Run it once, and again only if a future
pipeline change adds a new dependency.

If Windows blocks a PowerShell script because of the execution policy, use:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\setup_local_refresh.ps1
```

## Normal morning update

Open PowerShell in the `thesabrhood-site` folder and run:

```powershell
.\scripts\local_daily_refresh.ps1 -OpenSite
```

If Windows applies the same execution-policy restriction, use:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\local_daily_refresh.ps1 -OpenSite
```

By default, the updater:

1. Downloads only completed games through yesterday.
2. Grades prior projections and rebuilds all PBP analytics.
3. Refreshes season data, awards, history, editorial products, and Triple-A.
4. Pulls today's schedule, rosters, starters, batting orders, and weather.
5. Runs team simulations and any player simulations supported by posted lineups.
6. Rebuilds the graphics feed, freshness report, manifest, and every site page.
7. Opens `docs/index.html` after all validation checks pass.

For a quicker design preview with fewer simulation trials:

```powershell
.\scripts\local_daily_refresh.ps1 -Fast -OpenSite
```

Use the normal command, without `-Fast`, for the version you intend to publish.

## Update lineups closer to game time

Morning batting orders are often incomplete. Check again later without
redownloading and rebuilding the full historical dataset:

```powershell
.\scripts\local_daily_refresh.ps1 -Mode Lineups -OpenSite
```

This refreshes probable starters and posted batting orders, reruns the daily
team and available player simulations, updates graphics and manifests, and
renders the site again.

## Rebuild after a front-end edit

After changing QMD, CSS, article, or layout files, reuse the last good data:

```powershell
.\scripts\local_daily_refresh.ps1 -Mode Site -OpenSite
```

This is the fastest way to inspect front-end work. It does not contact any data
source or rerun simulations.

## Rebuild a specific date

The defaults are completed PBP through yesterday and a slate for today. To
reproduce another date:

```powershell
.\scripts\local_daily_refresh.ps1 -PbpEnd "2026-07-22" -SlateDate "2026-07-23" -OpenSite
```

Never set `PbpEnd` to a day with games still in progress. The PBP updater
accepts only final games, so yesterday is the safe daily default.

## Publish the result

First inspect what the updater changed:

```powershell
git status --short
git diff --stat
```

The intended public generated paths are `data/derived`, `images/graphics-feed`,
`includes`, and `docs`. Stage and commit them only after the local site looks
correct:

```powershell
git add -- data/derived images/graphics-feed includes docs
git commit -m "data: local daily refresh"
git push -u origin (git branch --show-current)
```

If this is not the `main` branch, merge the branch into `main` before expecting
the live GitHub Pages site to change. The current Pages setup publishes
`main/docs`.

For a manual Netlify deploy, upload or drag the entire `docs` folder. For the
Git-connected Netlify site, pushing and merging the generated `docs` changes is
enough; `netlify.toml` already declares `docs` as the publish directory.

## If a stage fails

The command names the failed stage and stops before reporting success. The
previous live deployment remains untouched. Fix that stage and rerun the same
command. If a data source is temporarily unavailable but you want to continue
front-end work, use `-Mode Site` to render the last validated local data.
