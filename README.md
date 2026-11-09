# DIA-GLY3 | Simulated Phase III Diabetes Clinical Trial

**End-to-end biostatistics portfolio project · R · Clinical trial simulation · Statistical analysis · Clinical reporting**

> **Educational simulation:** GLY-301, all participant records, and all results in this repository are fictional. This is not an actual clinical trial, a regulatory submission, or a fully CDISC-compliant implementation.

## Project overview

DIA-GLY3 demonstrates a clinical-trial workflow from study planning and simulated data generation through analysis datasets, statistical programming, tables/listings/figures (TLFs), quality control (QC), and a clinical study report (CSR).

The simulated study compares **GLY-301 plus metformin** with **placebo plus metformin** in adults with type 2 diabetes. Its primary objective is to evaluate change in glycated hemoglobin (HbA1c) from baseline to Week 26.

## Study at a glance

| Design feature | Specification |
| --- | --- |
| Phase | III (simulated) |
| Design | Randomized, double-blind, placebo-controlled, parallel-group |
| Duration | 26 weeks |
| Participants | 478 randomized; 239 per arm |
| Treatment | GLY-301 + metformin vs placebo + metformin |
| Randomization stratification | Baseline HbA1c <9% vs ≥9% |
| Primary endpoint | Change in HbA1c from baseline to Week 26 |
| Main analysis population | Full Analysis Set (478 randomized participants) |
| Primary missing-data approach | Multiple imputation under a missing-at-random (MAR) assumption |

## Statistical methods

- **Study planning:** sample-size/power calculations and simulation specifications.
- **Primary efficacy:** analysis of covariance (ANCOVA) for Week-26 HbA1c change, adjusting for baseline HbA1c and randomization stratum; treatment-specific multiple imputation for missing outcomes, combined using Rubin's rules.
- **Secondary efficacy:** logistic regression for HbA1c response, ANCOVA for fasting plasma glucose (FPG), and a supportive observed-case ANCOVA for body-weight change.
- **Longitudinal efficacy:** mixed model for repeated measures (MMRM) for HbA1c across postbaseline visits.
- **Sensitivity:** missing-not-at-random (MNAR) delta-adjustment/tipping-point analysis.
- **Safety:** treatment-emergent adverse events (TEAEs), serious events, event severity, hypoglycemia, exposure, and adherence.
- **Reporting:** descriptive summaries, treatment contrasts, confidence intervals, TLFs, and CSR interpretation.

## Selected findings from simulated data

| Outcome | GLY-301 vs placebo | Interpretation |
| --- | --- | --- |
| Primary HbA1c change, Week 26 | Adjusted difference **−0.558 percentage points** (95% CI −0.811 to −0.305; p=0.0000197) | Primary MI/ANCOVA analysis favored GLY-301 |
| HbA1c responder | Odds ratio **2.70** (95% CI 1.64 to 4.44; p=0.000117) | Higher response odds in GLY-301 arm |
| FPG change | Adjusted difference **−12.40 mg/dL** (95% CI −21.04 to −3.76; p=0.00510) | FPG reduction favored GLY-301 |
| Body-weight change, Week 26 | Adjusted difference **−1.637 kg** (95% CI −1.953 to −1.321; nominal p<0.0001) | **Supportive observed-case** analysis only; 382/478 observed |
| HbA1c MMRM, Week 26 | Difference **−0.575 percentage points** (95% CI −0.801 to −0.349) | Consistent with primary analysis |

**Missing data and interpretation:** Week-26 HbA1c was missing for **96/478 (20.1%)** participants. The primary analysis used multiple imputation under MAR. The MNAR tipping-point exercise first crossed p≥0.05 at approximately **+1.34 percentage points** under its specified delta scenario; this is a sensitivity result, not a new primary analysis.

**Body-weight correction:** The original ADaM body-weight records inadvertently included complete simulated outcomes that were absent from observed SDTM vital signs. The reviewed ADaM records now use observed data: **186 GLY-301** and **196 placebo** participants had Week-26 weight measurements, with **96 missing** overall. The observed-case ANCOVA is explicitly **supportive rather than confirmatory**, because missing outcomes were not imputed and the analysis is not the originally planned confirmatory approach. See [`REVIEW_NOTES.md`](REVIEW_NOTES.md).

### Selected safety results

- At least one TEAE: **111/239 (46.4%)** in GLY-301 vs **82/239 (34.3%)** in placebo.
- Serious treatment-emergent adverse events: **7/239 (2.9%)** vs **2/239 (0.8%)**.
- Participants with hypoglycemia: **38** in GLY-301 vs **9** in placebo.
- **55 hypoglycemia episodes** were recorded within the safety observation window: **43 during dosing** and **12 after the last dose within the 30-day follow-up**.

These simulated results are descriptive of this fictional dataset and are not evidence about a real medication.

## Repository structure

```text
DIA-GLY3-reviewed/
├── Protocol/              # Study protocol
├── SAP/                   # Statistical analysis plan
├── docs/                  # Study concept, simulation and dataset specifications
├── tlf/                   # TLF shells
├── data/
│   ├── raw/               # Simulated source data
│   ├── sdtm/              # Simplified SDTM-style domains
│   └── adam/              # Simplified ADaM-style analysis datasets
├── programs/R/            # Numbered R programs for simulation, analysis, TLFs and QC
├── outputs/
│   ├── tables/            # Analysis and descriptive CSV tables
│   ├── figures/           # Study figures
│   ├── listings/          # Participant-level listings
│   └── qc/                # QC reports
├── csr/                   # Reviewed clinical study report
├── REVIEW_NOTES.md        # Corrections, validation status and limitations
└── README.md
```

## How to reproduce and validate

**Requirements:** R and the R packages referenced by individual scripts. Package versions and a fully automated clean-environment build are not locked in this repository.

1. Download or clone the repository and open RStudio with the repository root as the working directory.
2. To regenerate the **corrected body-weight ADaM records and analysis**, run:

   ```r
   source("programs/R/13a_add_weight_to_adlb.R")
   source("programs/R/13_weight_analysis.R")
   ```

3. To rerun the available QC reports on the existing datasets and outputs:

   ```r
   source("programs/R/07_validate_adam.R")
   source("programs/R/17_final_qc_audit.R")
   source("programs/R/17b_statistical_consistency_qc.R")
   source("programs/R/17c_hypoglycemia_followup_qc.R")
   source("programs/R/17d_final_efficacy_qc.R")
   ```

4. Review the generated outputs under `outputs/`, the clinical study report under `csr/`, and the caveats in `REVIEW_NOTES.md`.

**Execution-order caution:** `programs/R/05_create_adlb.R` rebuilds ADLB without body-weight records. If you run it, **rerun `13a_add_weight_to_adlb.R` before any weight analysis or ADLB QC**. The numbered programs are not yet a single fully automated, independently validated end-to-end pipeline; do not assume that sourcing all scripts alphabetically will reproduce every deliverable unchanged.

## Quality control and known limitations

The reviewed project was checked in RStudio with the following reported results:

| QC area | Outcome |
| --- | --- |
| ADaM structural checks | 18/18 passed |
| Dataset integrity | 16/16 passed |
| Statistical consistency | 14/15 passed; one check uses an overly narrow dosing-period definition |
| Hypoglycemia safety-window checks | 4/4 passed |
| Efficacy output-file checks | 6/6 passed |
| SDTM vs ADaM Week-26 weight | 382 observed in each; zero missingness or value disagreements |

The statistical-consistency check that flags **12 post-dose hypoglycemia events** is not evidence of out-of-window events: the separate follow-up QC confirms all 55 events occur within the specified safety window. The flagged check is retained and explained rather than silently reported as a pass.

Other limitations include simplified (not fully compliant) SDTM/ADaM conventions, simulated rather than clinical data, incomplete protocol-deviation information, and a body-weight analysis that is supportive rather than confirmatory. Individual scripts and corrected outputs have been checked, but **a full clean-environment rerun of every analysis has not been independently demonstrated**. See [`REVIEW_NOTES.md`](REVIEW_NOTES.md) for the audit trail.

## Deliverables

- **Study protocol and SAP:** `Protocol/` and `SAP/`
- **Analysis code:** `programs/R/`
- **Analysis datasets:** `data/sdtm/` and `data/adam/`
- **TLFs and QC:** `outputs/`
- **Clinical study report:** `csr/DIA-GLY3_Clinical_Study_Report_reviewed.docx`

---

**Portfolio focus:** trial simulation, reproducible statistical programming, missing-data analysis, ANCOVA, logistic regression, MMRM, safety summaries, QC, and clinical reporting in R.
