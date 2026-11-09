
# ============================================================
# DIA-GLY3 | HYPOGLYCEMIA QC INVESTIGATION
# ============================================================

# Reload to ensure consistent objects
adsl <- read.csv("data/adam/adsl.csv")
hypo <- read.csv("data/raw/hypoglycemia.csv")

safety <- subset(adsl, SAFFL == "Y")

# Attach dosing and follow-up information
hypo <- merge(
  hypo,
  safety[, c(
    "USUBJID",
    "TRT01A",
    "FIRST_DOSE_DAY",
    "LAST_DOSE_DAY",
    "SAFETY_FU_DAY"
  )],
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

stopifnot(
  !anyNA(hypo[, c(
    "TRT01A", "FIRST_DOSE_DAY",
    "LAST_DOSE_DAY", "SAFETY_FU_DAY"
  )])
)

# Classify event timing
hypo$TIMING <- ifelse(
  hypo$HYPOSTDY < hypo$FIRST_DOSE_DAY,
  "Before first dose",
  ifelse(
    hypo$HYPOSTDY <= hypo$LAST_DOSE_DAY,
    "During dosing",
    ifelse(
      hypo$HYPOSTDY <= hypo$SAFETY_FU_DAY,
      "After last dose - within follow-up",
      "After safety follow-up"
    )
  )
)

cat("\n=== EVENT TIMING BY TREATMENT ===\n")
print(table(hypo$TRT01A, hypo$TIMING))

cat("\n=== EVENTS OUTSIDE DOSING PERIOD ===\n")
outside <- subset(
  hypo,
  TIMING != "During dosing"
)

print(
  outside[, c(
    "USUBJID", "TRT01A", "HYPOSEQ",
    "HYPOSTDY", "LAST_DOSE_DAY",
    "SAFETY_FU_DAY", "HYPOLEVEL",
    "EXTERNAL_ASSISTANCE", "TIMING"
  )],
  row.names = FALSE
)

cat("\n=== HYPOGLYCEMIA WITHIN SAFETY WINDOW ===\n")
within_safety <- subset(
  hypo,
  HYPOSTDY >= FIRST_DOSE_DAY &
    HYPOSTDY <= SAFETY_FU_DAY
)

print(table(within_safety$TRT01A))

cat("\n=== UNIQUE PARTICIPANTS WITHIN SAFETY WINDOW ===\n")
print(
  tapply(
    within_safety$USUBJID,
    within_safety$TRT01A,
    function(x) length(unique(x))
  )
)

cat("\n=== SAFETY FOLLOW-UP DAY VALIDATION ===\n")
print(summary(safety$SAFETY_FU_DAY))

cat("\n=== FOLLOW-UP AFTER LAST DOSE ===\n")
print(summary(
  safety$SAFETY_FU_DAY - safety$LAST_DOSE_DAY
))

# ============================================================
# DIA-GLY3 | HYPOGLYCEMIA SAFETY WINDOW QC
# ============================================================

qc_followup <- data.frame(
  Check = c(
    "All hypoglycemia episodes within safety window",
    "No episodes before first dose",
    "No episodes after safety follow-up",
    "Safety follow-up lasts 30 days"
  ),
  Result = c(
    all(hypo$HYPOSTDY >= hypo$FIRST_DOSE_DAY &
          hypo$HYPOSTDY <= hypo$SAFETY_FU_DAY),
    sum(hypo$TIMING == "Before first dose") == 0,
    sum(hypo$TIMING == "After safety follow-up") == 0,
    all(safety$SAFETY_FU_DAY -
          safety$LAST_DOSE_DAY == 30)
  )
)

qc_followup$Result <- ifelse(
  qc_followup$Result,
  "PASS",
  "FAIL"
)

cat("\n=== HYPOGLYCEMIA FOLLOW-UP QC ===\n")
print(qc_followup, row.names = FALSE)

dir.create(
  "outputs/qc",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  qc_followup,
  "outputs/qc/hypoglycemia_followup_qc.csv",
  row.names = FALSE
)
