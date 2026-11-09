
# ============================================================
# DIA-GLY3 
# Adverse Event Analysis Dataset (ADaM-style)
# ============================================================

# 1. Verify source files
required_files <- c(
  "data/adam/adsl.csv",
  "data/raw/adverse_events.csv"
)

stopifnot(all(file.exists(required_files)))

# 2. Load source datasets
adsl <- read.csv("data/adam/adsl.csv")
ae <- read.csv("data/raw/adverse_events.csv")

# 3. Attach subject-level analysis variables
adae <- merge(
  ae,
  adsl[, c(
    "USUBJID", "TRT01A", "SAFFL",
    "FIRST_DOSE_DAY", "LAST_DOSE_DAY",
    "SAFETY_FU_DAY", "DISCONFL"
  )],
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

# 4. Derive treatment-emergent status
adae$TRTEMFL <- ifelse(
  adae$AESTDY >= adae$FIRST_DOSE_DAY &
    adae$AESTDY <= adae$SAFETY_FU_DAY,
  "Y", "N"
)

# 5. Serious adverse event flag
adae$SAEFL <- ifelse(
  adae$AESER == "Y", "Y", "N"
)

# 6. Severity categories
adae$AESEVN <- match(
  adae$AESEV,
  c("MILD", "MODERATE", "SEVERE")
)

# 7. Treatment-related AE flag
adae$RELFL <- ifelse(
  adae$AEREL == "RELATED", "Y", "N"
)

# 8. AE leading to treatment discontinuation
adae$AEDISFL <- ifelse(
  adae$AEACN == "DRUG WITHDRAWN", "Y", "N"
)

# 9. Treatment-emergent safety population
adae$ANL01FL <- ifelse(
  adae$TRTEMFL == "Y" &
    adae$SAFFL == "Y",
  "Y", "N"
)

# 10. Sort records
adae <- adae[
  order(adae$USUBJID, adae$AESTDY, adae$AESEQ),
]

rownames(adae) <- NULL

# 11. Validate
stopifnot(
  all(adae$USUBJID %in% adsl$USUBJID),
  all(adae$SAFFL == "Y"),
  all(!is.na(adae$AESEVN)),
  !anyDuplicated(adae[c("USUBJID", "AESEQ")])
)

# 12. Save ADAE
write.csv(
  adae,
  "data/adam/adae.csv",
  row.names = FALSE
)

# 13. Summary
cat("\nADAE created successfully.\n")
cat("Total AE records:", nrow(adae), "\n")
cat("Participants with AEs:",
    length(unique(adae$USUBJID)), "\n")
cat("Treatment-emergent AEs:",
    sum(adae$TRTEMFL == "Y"), "\n")
cat("Serious AEs:",
    sum(adae$SAEFL == "Y"), "\n")
cat("AE records leading to discontinuation:",
    sum(adae$AEDISFL == "Y"), "\n")
