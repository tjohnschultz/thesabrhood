# Organized R Function Library

This folder keeps your existing function logic intact but gives the files a clearer order.
The safest workflow is to source `source_all.R` from the folder above this one.

Key principle: original function names are preserved. Cleaner names and a few small counterparts live in `99_clean_names_and_counterparts.R`, which should be sourced last.

Suggested folders later, if you want to split further:
- `core/`: setup, helpers, data loading, summaries
- `analysis/`: expected contact, rolling trends, split comparisons
- `viz/`: themes, pitch-type pack, Visual Revolution plots
- `export/`: social/facet export helpers
