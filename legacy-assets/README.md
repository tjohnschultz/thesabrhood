# Immutable archive assets

`legacy-assets/posts/` contains the canonical rendered snapshots for the
original research archive. These snapshots preserve article copy, figures,
tables, captions, and links without requiring old R package versions or remote
data sources to remain available.

Every site build runs `scripts/finalize_rendered_site.R`, which:

1. restores these snapshots into `docs/posts/`;
2. applies shared navigation, masthead, responsive reading layout, and current
   site asset paths;
3. removes retired generated outputs and writes build metadata; and
4. validates that the normalized article-body text still matches the canonical
   snapshot.

Do not edit article content in this directory as part of a styling change.
Update styling in `styles.css` and page chrome in
`scripts/normalize_legacy_articles.R`.
