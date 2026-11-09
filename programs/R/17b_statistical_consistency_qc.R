
# ============================================================
# DIA-GLY3 | FINAL QC
# Step 2: Statistical Consistency - Safety
# ============================================================

adsl <- read.csv("data/adam/adsl.csv")
adae <- read.csv("data/adam/adae.csv")
hypo <- read.csv("data/raw/hypoglycemia.csv")

safety <- subset(adsl, SAFFL == "Y")
ae <- subset(adae, TRTEMFL == "Y" & SAFFL == "Y")

# Use actual treatment from ADSL
ae$TRT01A <- safety$TRT01A[
  match(ae$USUBJID, safety$USUBJID)
]

hypo$TRT01A <- safety$TRT01A[
  match(hypo$USUBJID, safety$USUBJID)
]

qc <- data.frame(
  Check = character(),
  Result = character(),
  Details = character(),
  stringsAsFactors = FALSE
)

add_check <- function(name, condition, details) {
  qc[nrow(qc) + 1, ] <<- list(
    name,
    if (isTRUE(condition)) "PASS" else "FAIL",
    as.character(details)
  )
}

count_people <- function(data, treatment) {
  length(unique(data$USUBJID[
    data$TRT01A == treatment
  ]))
}

# 1. Reconcile overall TEAE incidence
add_check(
  "Placebo participants with TEAEs",
  count_people(ae, "Placebo") == 82,
  count_people(ae, "Placebo")
)

add_check(
  "GLY-301 participants with TEAEs",
  count_people(ae, "GLY-301") == 111,
  count_people(ae, "GLY-301")
)

# 2. Reconcile serious TEAEs
sae <- subset(ae, AESER == "Y")

add_check(
  "Placebo participants with serious TEAEs",
  count_people(sae, "Placebo") == 2,
  count_people(sae, "Placebo")
)

add_check(
  "GLY-301 participants with serious TEAEs",
  count_people(sae, "GLY-301") == 7,
  count_people(sae, "GLY-301")
)

# 3. Reconcile hypoglycemia
add_check(
  "Placebo participants with hypoglycemia",
  count_people(hypo, "Placebo") == 9,
  count_people(hypo, "Placebo")
)

add_check(
  "GLY-301 participants with hypoglycemia",
  count_people(hypo, "GLY-301") == 38,
  count_people(hypo, "GLY-301")
)

add_check(
  "Total hypoglycemia episodes",
  nrow(hypo) == 55,
  nrow(hypo)
)

# 4. Reconcile hypoglycemia classifications
add_check(
  "Level 1 hypoglycemia events",
  sum(hypo$HYPOLEVEL == "LEVEL 1") == 43,
  sum(hypo$HYPOLEVEL == "LEVEL 1")
)

add_check(
  "Level 2 hypoglycemia events",
  sum(hypo$HYPOLEVEL == "LEVEL 2") == 10,
  sum(hypo$HYPOLEVEL == "LEVEL 2")
)

add_check(
  "Level 3 hypoglycemia events",
  sum(hypo$HYPOLEVEL == "LEVEL 3") == 2,
  sum(hypo$HYPOLEVEL == "LEVEL 3")
)

# 5. Check Level 3 / external assistance consistency
level3 <- hypo$HYPOLEVEL == "LEVEL 3"
assisted <- hypo$EXTERNAL_ASSISTANCE == "Y"

add_check(
  "Level 3 events require external assistance",
  all(assisted[level3]),
  paste(
    "Level 3:", sum(level3),
    "| Assisted Level 3:", sum(level3 & assisted)
  )
)

add_check(
  "Assisted events are classified Level 3",
  all(level3[assisted]),
  paste(
    "Assisted:", sum(assisted),
    "| Assisted Level 3:", sum(level3 & assisted)
  )
)

# 6. Validate recorded hypoglycemia study days
add_check(
  "Hypoglycemia study days are positive",
  all(hypo$HYPOSTDY >= 1),
  paste(
    "Range:",
    paste(range(hypo$HYPOSTDY), collapse = "-")
  )
)

# Compare events with participant exposure window
hypo$FIRST_DOSE_DAY <- safety$FIRST_DOSE_DAY[
  match(hypo$USUBJID, safety$USUBJID)
]

hypo$LAST_DOSE_DAY <- safety$LAST_DOSE_DAY[
  match(hypo$USUBJID, safety$USUBJID)
]

# Assumes HYPOSTDY and dose days share the same day scale
within_exposure <- with(
  hypo,
  HYPOSTDY >= FIRST_DOSE_DAY &
    HYPOSTDY <= LAST_DOSE_DAY
)

add_check(
  "Hypoglycemia episodes within dosing period",
  all(!is.na(within_exposure) & within_exposure),
  paste(
    "Within dosing period:",
    sum(within_exposure, na.rm = TRUE),
    "of", nrow(hypo)
  )
)

# 7. Validate exposure duration
# Assumes inclusive counting of first and last dose days
calculated_duration <- with(
  safety,
  LAST_DOSE_DAY - FIRST_DOSE_DAY + 1
)

duration_matches <- !is.na(calculated_duration) &
  !is.na(safety$DURATION_DAYS) &
  calculated_duration == safety$DURATION_DAYS

add_check(
  "Treatment duration matches dosing dates",
  all(duration_matches),
  paste(
    "Matching participants:",
    sum(duration_matches),
    "of", nrow(safety)
  )
)

# 8. Print QC
cat("\n========== STATISTICAL CONSISTENCY QC ==========\n")
print(qc, row.names = FALSE, right = FALSE)

cat("\n========== QC SUMMARY ==========\n")
print(table(qc$Result))

# 9. Save
dir.create(
  "outputs/qc",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  qc,
  "outputs/qc/statistical_consistency_qc.csv",
  row.names = FALSE
)

cat("\nQC report saved successfully.\n")



