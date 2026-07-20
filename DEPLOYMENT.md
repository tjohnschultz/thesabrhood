# Deployment gates for thesabrhood.com

The rebuild is intentionally separated from the live site. Deployment should
happen only after these seven gates are complete.

## 1. Review the local build

- Open `docs/index.html` or run `quarto preview`.
- Review mobile and desktop layouts, language, links, and the retained research
  archive.
- Decide whether the branch is ready to merge.

## 2. Confirm repository settings

- Confirm `tjohnschultz/thesabrhood` is the one production repository.
- Enable Actions with read access to repository contents.
- Keep the preview workflow non-deploying until the data refresh is stable.

## 3. Choose private data storage

The raw PBP cannot live in the public site repository. Select one durable
private source for the scheduled job, such as a private release asset, private
cloud object storage, or a separate private data repository. Add only narrowly
scoped read credentials as GitHub Actions secrets.

## 4. Activate daily analytics refresh

The production job will:

1. fetch the current private PBP snapshot;
2. update only games not already present;
3. rebuild canonical views and derived products with `sabrhoodR`;
4. run data-contract and time-cutoff checks;
5. write the newsletter and site fragments;
6. render Quarto; and
7. retain the prior successful site when any check fails.

## 5. Complete projection calibration

The first manager hook model now has an out-of-time validation report. Before
public probabilities or betting-adjacent features launch, add full-game
backtests, calibration thresholds, park/opponent context, bullpen replacement
logic, and model-version archiving.

## 6. Approve a release candidate

- Confirm the displayed data date and sample labels.
- Test keyboard navigation, contrast, tables, and narrow screens.
- Check page weight, search, analytics, social metadata, and the 404 page.
- Confirm no raw data, credentials, or local filesystem paths are published.

## 7. Enable GitHub Pages and the custom domain

After explicit approval, add the deploy job, configure GitHub Pages to use its
artifact, verify `CNAME`, and confirm DNS and HTTPS for `thesabrhood.com`. Keep a
copy of the last known-good `docs` artifact for rollback.

## Current state

Gates 1–2 have a working local/CI foundation but still require owner review.
The data boundary for Gate 3 is implemented locally, not hosted. Gate 5 is in
progress. No live deployment action has been created.
