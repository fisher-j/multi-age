# Publication fuel-analysis pipeline

This directory is a publication-only branch of the thesis fuel workflow. It
must not alter raw data, the thesis notebooks, or thesis-era `.rda` objects.

Run the derivation from `analysis/`:

```bat
Rscript scripts/publication/fuel/01-derive-components.R
```

`01-derive-components.R` reads `summarize_collected_data.rda` as a read-only
input and writes only beneath `outputs/publication/fuel/`.

Current analytical decisions:

- Pre-PCT litter and duff are separate, model-derived forest-floor components.
  The script uses Finney and Martin (1993), model 3, with the recorded total
  forest-floor depth and estimated litter percentage. A negative value from a
  shallow extrapolation is bounded at zero and retained in the station-level
  audit output.
- Post-PCT duff and litter loads are deliberately not calculated or reported:
  slash and suspended green foliage make the forest-floor boundary ambiguous.
- Woody and herbaceous vegetation remain separate in both phases.
- The primary PCT contrast is matched-transect pre-PCT minus post-PCT woody
  standing vegetation. It is an inferred potential surface-fuel contribution,
  not a direct slash-load measurement.
- Fuel-bed depth is retained for a later protocol-comparability review and is
  not yet interpreted as slash depth.

No model family, dispersion structure, zero process, or random-effects
selection is specified here. Those decisions follow component derivation and
quality-control review.
