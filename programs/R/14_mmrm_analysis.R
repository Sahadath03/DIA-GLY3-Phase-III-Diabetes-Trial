

# ============================================================
# DIA-GLY3
# Supportive Efficacy Analysis
# MMRM for HbA1c Change from Baseline
# ============================================================

library(mmrm)

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")

# 2. Prepare post-baseline analysis dataset
dat <- subset(
  adeff,
  WEEK %in% c(4, 8, 12, 18, 26)
)

# Add baseline HbA1c only if it is not already in ADEFF
if (!"HBA1C_BL" %in% names(dat)) {
  dat <- merge(
    dat,
    adsl[, c("USUBJID", "HBA1C_BL")],
    by = "USUBJID",
    all.x = TRUE
  )
}

# 3. Define analysis variables
dat$TRT <- factor(
  dat$TRT01P,
  levels = c("Placebo", "GLY-301")
)

dat$VISIT <- factor(
  dat$WEEK,
  levels = c(4, 8, 12, 18, 26)
)

dat$STRATUM <- factor(
  ifelse(dat$HBA1C_BL < 9, "<9%", ">=9%"),
  levels = c("<9%", ">=9%")
)

dat$CHANGE <- dat$CHG_HBA1C

# Exclude missing outcomes, retaining observed visits
dat <- dat[!is.na(dat$CHANGE), ]

stopifnot(
  !anyDuplicated(dat[c("USUBJID", "VISIT")]),
  !anyNA(dat$TRT),
  !anyNA(dat$HBA1C_BL)
)

cat("\n========== MMRM ANALYSIS DATA ==========\n")
cat("Observed post-baseline records:", nrow(dat), "\n")
cat("Unique participants:", length(unique(dat$USUBJID)), "\n")

# 4. Fit MMRM with unstructured covariance
fit <- mmrm(
  CHANGE ~ TRT * VISIT +
    HBA1C_BL * VISIT + STRATUM +
    us(VISIT | USUBJID),
  data = dat,
  method = "Kenward-Roger"
)

cat("\n========== MMRM MODEL SUMMARY ==========\n")
print(summary(fit))

# 5. Estimate adjusted treatment differences by visit
# emmeans computes contrasts using the fitted MMRM.
if (!requireNamespace("emmeans", quietly = TRUE)) {
  stop("Install emmeans: install.packages('emmeans')")
}

emm <- emmeans::emmeans(
  fit,
  specs = ~ TRT | VISIT
)

contrasts <- emmeans::contrast(
  emm,
  method = list(
    "GLY-301 minus Placebo" = c(-1, 1)
  ),
  by = "VISIT"
)

contrast_results <- as.data.frame(
  summary(contrasts, infer = c(TRUE, TRUE))
)

cat("\n========== TREATMENT DIFFERENCES BY VISIT ==========\n")
print(contrast_results)

# 6. Save outputs
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/diagnostics",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  contrast_results,
  "outputs/tables/T14_2_5_mmrm_by_visit.csv",
  row.names = FALSE
)

saveRDS(
  fit,
  "outputs/diagnostics/mmrm_model.rds"
)

# 7. Week 26 contrast
week26 <- subset(
  contrast_results,
  VISIT == "26"
)

cat("\n========== WEEK 26 MMRM SUMMARY ==========\n")
print(week26)

cat("\nMMRM analysis completed.\n")
print(contrast_results)
