# Deployment gates for thesabrhood.com

The rebuild remains intentionally separated from the live site. The site
foundation and derived-data boundary are complete. Six major gates remain
before a trustworthy public launch.

## 1. Finish product and editorial polish

- Review the information hierarchy, repeated language, card density, and the
  visual relationship between editorial stories and analytical tools.
- Finish mobile and desktop layouts, the retained research archive, and clear
  freshness labels on every time-sensitive product.
- Decide which prototype modules belong in the initial public release.

## 2. Connect and calibrate the daily projection model

- Fit starter, lineup, park, weather, and active-bullpen effects into the score
  model rather than displaying them beside demonstration probabilities.
- Run rolling out-of-time backtests, establish calibration thresholds, archive
  model versions, and review the largest misses.
- Keep betting-adjacent outputs private until these checks pass.

## 3. Harden private data storage

Raw PBP does not live in the public site repository. The scheduled prototype
uses a private GitHub Actions cache and can rebuild from completed MLB games if
that cache expires. Before commercial launch, select a durable private archive
for raw snapshots and fitted model objects, such as private cloud object storage
or a separate private data repository. Add only narrowly scoped credentials as
GitHub Actions secrets.

## 4. Activate and observe the daily analytics refresh

The production job will:

1. fetch the current private PBP snapshot;
2. update only games not already present;
3. rebuild canonical views and derived products with `sabrhoodR`;
4. run data-contract and time-cutoff checks;
5. write the newsletter and site fragments;
6. render Quarto; and
7. retain the prior successful site when any check fails.

Run this on a staging branch long enough to observe late lineups, postponements,
transactions, upstream failures, and recovery behavior.

## 5. Approve a release candidate

- Confirm displayed dates, sample labels, source language, and research caveats.
- Test keyboard navigation, contrast, tables, links, and narrow screens.
- Check page weight, search, analytics, social metadata, and the 404 page.
- Confirm no raw data, credentials, or local filesystem paths are published.
- Complete the editorial, privacy, terms, and business-contact copy needed for
  a professional public product.

## 6. Enable GitHub Pages and the custom domain

After explicit approval, add the deploy job, configure GitHub Pages to use its
artifact, verify `CNAME`, and confirm DNS and HTTPS for `thesabrhood.com`. Keep a
copy of the last known-good `docs` artifact for rollback.

Merging the rebuild pull request updates `main` in the existing repository; it
does not erase the original site's Git history. If GitHub Pages is currently
configured to publish `main/docs`, the rebuilt `docs` directory will become the
live site after the merge. Before that merge, create a `legacy-site-before-rebuild`
branch or tag at the current `main` commit and confirm the Pages source under
**Settings > Pages**. This provides both a readable archive and a one-step
rollback point.

## Current state

The product foundation, repository-contained package, compact derived-data
mirror, scheduled daily refresh, and non-deploying preview workflow are
working. The daily refresh can update the existing `docs` output through
commits to `main`; model effects and calibration are still not approved as a
public betting product. GitHub Pages and custom-domain configuration remain the
explicit final deployment gate.
