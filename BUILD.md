# The SABRhood site build

The public website is a Quarto site generated from compact derived CSV products.
Raw play-by-play and private research inputs are not deployed with the site.

## Local build

From the repository root:

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
3. Validate data grains, dates, row counts, and method versions.
4. Copy only approved derived CSV products into `data/derived`.
5. Generate static HTML fragments.
6. Render and validate the Quarto site.
7. Publish only after every earlier stage succeeds.
