# Publishing a SABRhood article

## Fastest workflow

From the site repository, create a correctly named, team-themed draft:

```powershell
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" `
  scripts/new_article.R `
  "How the cutter changed Boston" `
  "Boston Red Sox" `
  "Pitching"
```

The command creates a dated, URL-safe file in `articles/`. It also selects the
correct team theme from `config/mlb-team-themes.csv`.

Then:

1. Open the new `.qmd` file in `articles/`.
2. Replace the description, deck, callout, and body.
3. Put the article image or generated chart in `images/` and update `image:`.
4. Keep `draft: true` while editing. Change it to `draft: false` when ready.
5. Build only the site:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File `
  .\scripts\local_daily_refresh.ps1 -Mode Site -OpenSite
```

6. Check the article and the Research archive in `docs/`.
7. Commit the source article, image, generated fragments, and `docs/` output on
   the current feature branch. Push that branch and merge it through a pull
   request.

## Manual workflow

Copy `articles/_article-template.qmd` to a short, URL-safe filename such as
`articles/2026-07-24-red-sox-bullpen-shape.qmd`. Update its metadata and change
the wrapper class to `theme-` plus the lowercase team slug, such as
`theme-seattle-mariners`.

The `team` metadata controls the card on the Research page. The wrapper class
controls the article-page palette. Both should describe the same team.

## Archived articles

The original files under `posts/` remain untouched source material. The build
now normalizes their rendered HTML after Quarto finishes, connecting every
archived article to:

- `styles.css`
- `includes/article-team-themes.css`
- a consistent Research archive navigation bar
- the correct team palette where the team is known

Do not hand-edit `docs/posts/*.html`; those changes are generated and can be
replaced by the next build.
