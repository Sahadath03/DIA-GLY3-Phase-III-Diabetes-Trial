# DIA-GLY3 — Review and corrections (9 November 2026)

## Confirmed issue and resolution
SDTM VS was built from `data/raw/efficacy_observed.csv`; WEIGHT ADLB was appended from `data/raw/efficacy_longitudinal.csv` (complete simulated data). Week-26 weight observed in VS: 382/478, versus 478/478 in original ADLB. All 382 shared observed values matched exactly. ADLB WEIGHT records have been rebuilt using `data/adam/adeff.csv` (observed data); the build script `13a_add_weight_to_adlb.R` has been corrected to prevent recurrence.

## Revised observed-case body-weight ANCOVA
N=382 (GLY-301 186, placebo 196); adjusted difference GLY-301 minus placebo = −1.636866 kg, 95% CI (−1.952698, −1.321034), nominal p=1.04e-21. This is **supportive, not a confirmatory fixed-sequence result** because 96 weights are missing and no imputation was performed. A full treatment-policy MAR analysis would require an additional prespecified missing-data method.

## Files changed
- `data/adam/adlb.csv` WEIGHT records corrected to observed values.
- `programs/R/13a_add_weight_to_adlb.R` now derives WEIGHT from observed ADEFF.
- `programs/R/13_weight_analysis.R` now performs observed-case ANCOVA and writes revised tables.
- `outputs/tables/T14_2_4_weight_ancova.csv` and `T14_2_4_weight_descriptives.csv` revised.
- `outputs/diagnostics/weight_ancova_model.rds` removed because the old model is stale; rerun the revised R script to regenerate it.
- `csr/DIA-GLY3_Clinical_Study_Report_reviewed.docx` revised to describe observed-case body-weight results and add T14.3.7.
- Four feasible participant listings added under `outputs/listings/`.

## Remaining limitations and validation status
- The revised R scripts have been inspected but **not executed in R** in this environment. Revised model estimates were independently computed using Python/statsmodels with the same ANCOVA formula.
- Other efficacy and safety models have not been comprehensively rerun in R. The earlier QC pass counts did not cover cross-domain data lineage. Do not call the entire project fully validated.
- No comprehensive protocol deviation listing is possible with the available simulated fields.
- No AE discontinuation records exist (the listing is empty, consistent with the safety summary).
- Safety laboratory coverage is limited to HbA1c, FPG, and body weight.
- Original backup directory was excluded from the reviewed ZIP to avoid stale duplicate data.
- The original CSR was uploaded before the latest manual edits; this reviewed CSR was reconstructed from that uploaded version and should be compared against any more recent local edits.
