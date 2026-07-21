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

## 3. Choose private data storage

The raw PBP cannot live in the public site repository. Select one durable
private source for the scheduled job, such as a private release asset, private
cloud object storage, or a separate private data repository. Add only narrowly
scoped read credentials as GitHub Actions secrets.

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

## Current state

The local product foundation, package boundary, compact derived-data mirror,
and non-deploying preview workflow are working. Gate 1 is active. The daily
input contract needed for Gate 2 is connected, but model effects and
calibration are not complete. No live deployment action has been created.
