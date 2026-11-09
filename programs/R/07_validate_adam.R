
# ============================================================
# DIA-GLY3 
# Cross-Dataset Validation
# ============================================================

adsl  <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")
adlb  <- read.csv("data/adam/adlb.csv")
adae  <- read.csv("data/adam/adae.csv")

results <- data.frame(
  Check = character(),
  Status = character(),
  stringsAsFactors = FALSE
)

check <- function(name, condition) {
  passed <- isTRUE(condition)
  
  results <<- rbind(
    results,
    data.frame(
      Check = name,
      Status = if (passed) "PASS" else "FAIL"
    )
  )
}

# 1. Participant-level structure
check("ADSL contains 478 participants",
      nrow(adsl) == 478)

check("ADSL identifiers unique",
      !anyDuplicated(adsl$USUBJID))

check("Randomization 239 per arm",
      sum(adsl$TRT01P == "GLY-301") == 239 &&
        sum(adsl$TRT01P == "Placebo") == 239)

# 2. Efficacy structure
check("ADEFF contains 2868 records",
      nrow(adeff) == 2868)

check("ADEFF participant-visit keys unique",
      !anyDuplicated(
        adeff[c("USUBJID", "WEEK")]
      ))

check("All ADEFF subjects in ADSL",
      all(adeff$USUBJID %in% adsl$USUBJID))

check("Week-26 HbA1c missingness = 96",
      sum(
        is.na(adeff$HBA1C[adeff$WEEK == 26])
      ) == 96)

check("Week-0 HbA1c change equals zero",
      all(
        !is.na(adeff$CHG_HBA1C[adeff$WEEK == 0]) &
          abs(adeff$CHG_HBA1C[
            adeff$WEEK == 0
          ]) < 1e-8
      ))

# 3. Treatment consistency
trt_lookup <- adsl$TRT01P[
  match(adeff$USUBJID, adsl$USUBJID)
]

check("ADEFF treatment matches ADSL",
      all(adeff$TRT01P == trt_lookup))

ae_trt_lookup <- adsl$TRT01A[
  match(adae$USUBJID, adsl$USUBJID)
]

check("ADAE treatment matches ADSL",
      all(adae$TRT01A == ae_trt_lookup))

# 4. Laboratory structure
check("ADLB contains 8604 records",
      nrow(adlb) == 8604)

check("ADLB parameter-visit keys unique",
      !anyDuplicated(
        adlb[c("USUBJID", "PARAMCD", "AVISITN")]
      ))

check("All ADLB subjects in ADSL",
      all(adlb$USUBJID %in% adsl$USUBJID))

# 5. Safety
check("All ADAE subjects in ADSL",
      all(adae$USUBJID %in% adsl$USUBJID))

check("ADAE event keys unique",
      !anyDuplicated(
        adae[c("USUBJID", "AESEQ")]
      ))

check("No unmatched drug withdrawals",
      all(
        adae$AEDISFL != "Y" |
          adae$DISCONFL == "Y"
      ))

check("TEAE flags match exposure window",
      all(
        adae$TRTEMFL ==
          ifelse(
            adae$AESTDY >= adae$FIRST_DOSE_DAY &
              adae$AESTDY <= adae$SAFETY_FU_DAY,
            "Y", "N"
          )
      ))

# 6. Cross-dataset baseline consistency
adsl_baseline <- round(
  adsl$HBA1C_BL[
    match(adeff$USUBJID, adsl$USUBJID)
  ], 2
)

check("ADEFF baseline matches ADSL",
      all(abs(
        adeff$HBA1C_BL - adsl_baseline
      ) < 1e-8))

# 7. Output
cat("\n========== ADAM VALIDATION REPORT ==========\n")
print(results, row.names = FALSE)

cat("\nChecks passed:",
    sum(results$Status == "PASS"),
    "of", nrow(results), "\n")

dir.create(
  "docs/validation",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "docs/validation/adam_validation_results.csv",
  row.names = FALSE
)

if (any(results$Status == "FAIL")) {
  warning("Some validation checks failed. Review before proceeding.")
} else {
  cat("\nAll checks passed successfully.\n")
}

