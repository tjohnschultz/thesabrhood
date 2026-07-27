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

The original files under `posts/` remain untouched source material.
`legacy-assets/posts/` holds immutable rendered snapshots so archived articles
do not depend on old R packages, cached sessions, or remote data sources.

Every local and GitHub build calls `scripts/finalize_rendered_site.R` after
Quarto. That single finalization step restores the snapshots and connects every
archived article to:

- `styles.css`
- `includes/article-team-themes.css`
- the current site navbar and footer, with Research marked active
- a consistent Research archive masthead
- a responsive reading frame for prose, figures, code, and wide tables
- the correct team palette where the team is known

The rendered validator compares the article-body text with its canonical
snapshot and fails publication if normalization changes the article content.

Do not hand-edit `docs/posts/*.html`; those files are restored and normalized
again during the next build. Styling changes belong in `styles.css`, while
archive chrome changes belong in `scripts/normalize_legacy_articles.R`.
