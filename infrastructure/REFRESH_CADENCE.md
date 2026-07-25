# Refresh cadence

The proof-of-concept uses three deliberately separate refresh paths. This
keeps final-game PBP acquisition independent from provider publication timing
and prevents intraday lineup checks from repeatedly downloading historical
play-by-play data.

## 6:00 AM America/New_York: completed-game publication

`daily-data-refresh.yml` runs at 6:00 AM in the `America/New_York` timezone.
GitHub applies daylight-saving changes to this schedule.

The workflow:

1. restores the private incremental PBP cache;
2. admits only games whose MLB schedule and game feed both report a final
   state;
3. refuses to replace the cache if any identified final game cannot be
   acquired completely;
4. rebuilds PBP analytics, history, editorial products, the current slate,
   projections, graphics, and the rendered site;
5. publishes and checksum-verifies one immutable Supabase shadow release when
   `SABRHOOD_SCHEDULED_SHADOW_ENABLED` is `true`; and
6. commits approved public derivatives and rendered pages for the current
   GitHub Pages deployment.

The morning build intentionally uses the latest previously validated FanGraphs
season snapshot. It does not claim that FanGraphs has completed its overnight
publication by 6:00 AM.

## 11:45 AM America/New_York: FanGraphs reconciliation

`fangraphs-reconciliation.yml` runs during the MLB season at 11:45 AM. It
retries acquisition, writes the private snapshot transactionally, compares new
public products with the morning baseline, and refuses implausible row or
playing-time regressions. A failed reconciliation does not overwrite or remove
the successful morning publication.

After validation it rebuilds FanGraphs-dependent projections, graphics, health
metadata, and site pages. It saves the private source snapshot for the next
morning and commits only approved public derivatives. It does not acquire PBP
and does not create a second full Supabase release.

`fangraphs-reconciliation-status.csv` records acquisition time, row counts,
aggregate playing-time measures, and whether the provider content advanced
relative to the morning baseline.

## Every 30 minutes: pregame inputs only

`lineup-refresh.yml` remains responsible for posted batting orders, probable
starters, pregame projections, and prediction snapshots. Its four-hour gate
prevents unnecessary builds when no game wave is approaching. It does not
acquire completed-game or live-game PBP.

## Current deployment boundary

GitHub Actions remains the proof-of-concept compute scheduler and GitHub Pages
remains the public deployment. Supabase is the immutable shadow release store
and recovery source. A later Cloud Scheduler/Cloud Run and Netlify cutover can
reuse these provider and release boundaries without changing public research
pages.
