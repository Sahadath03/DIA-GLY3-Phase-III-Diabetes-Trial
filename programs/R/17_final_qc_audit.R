
# ============================================================
# DIA-GLY3 | FINAL QUALITY-CONTROL AUDIT
# Step 1: Dataset Integrity
# ============================================================

# 1. Load datasets
adsl  <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")
adlb  <- read.csv("data/adam/adlb.csv")
adae  <- read.csv("data/adam/adae.csv")
hypo  <- read.csv("data/raw/hypoglycemia.csv")

# 2. Initialize QC log
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

# 3. Participant population
add_check(
  "ADSL contains 478 participants",
  nrow(adsl) == 478,
  paste("Observed:", nrow(adsl))
)

add_check(
  "Unique participant identifiers",
  !anyDuplicated(adsl$USUBJID),
  paste("Duplicates:", sum(duplicated(adsl$USUBJID)))
)

add_check(
  "Equal treatment allocation",
  all(table(adsl$TRT01A) == 239) &&
    length(unique(adsl$TRT01A)) == 2,
  paste(
    names(table(adsl$TRT01A)),
    as.integer(table(adsl$TRT01A)),
    collapse = "; "
  )
)

add_check(
  "Safety population contains 478 participants",
  sum(adsl$SAFFL == "Y", na.rm = TRUE) == 478,
  paste(
    "Observed:",
    sum(adsl$SAFFL == "Y", na.rm = TRUE)
  )
)

# 4. ADEFF integrity
add_check(
  "ADEFF contains 2868 records",
  nrow(adeff) == 2868,
  paste("Observed:", nrow(adeff))
)

add_check(
  "ADEFF participants exist in ADSL",
  all(adeff$USUBJID %in% adsl$USUBJID),
  paste(
    "Unmatched records:",
    sum(!adeff$USUBJID %in% adsl$USUBJID)
  )
)

add_check(
  "No duplicate ADEFF participant visits",
  !anyDuplicated(adeff[, c("USUBJID", "WEEK")]),
  paste(
    "Duplicates:",
    sum(duplicated(adeff[, c("USUBJID", "WEEK")]))
  )
)

w26 <- subset(adeff, WEEK == 26)

add_check(
  "Week 26 contains 478 participant records",
  nrow(w26) == 478,
  paste("Observed:", nrow(w26))
)

add_check(
  "Week 26 HbA1c has 96 missing values",
  sum(is.na(w26$HBA1C)) == 96,
  paste("Observed:", sum(is.na(w26$HBA1C)))
)

# 5. ADLB integrity
add_check(
  "ADLB participants exist in ADSL",
  all(adlb$USUBJID %in% adsl$USUBJID),
  paste("Observed rows:", nrow(adlb))
)

# 6. ADAE integrity
add_check(
  "ADAE contains 251 records",
  nrow(adae) == 251,
  paste("Observed:", nrow(adae))
)

add_check(
  "No duplicate AE sequence identifiers",
  !anyDuplicated(adae[, c("USUBJID", "AESEQ")]),
  paste(
    "Duplicates:",
    sum(duplicated(adae[, c("USUBJID", "AESEQ")]))
  )
)

add_check(
  "All AE participants exist in ADSL",
  all(adae$USUBJID %in% adsl$USUBJID),
  paste(
    "Unmatched records:",
    sum(!adae$USUBJID %in% adsl$USUBJID)
  )
)

# 7. Hypoglycemia integrity
add_check(
  "Hypoglycemia contains 55 event records",
  nrow(hypo) == 55,
  paste("Observed:", nrow(hypo))
)

add_check(
  "Hypoglycemia participant IDs exist in ADSL",
  all(hypo$USUBJID %in% adsl$USUBJID),
  paste(
    "Unmatched records:",
    sum(!hypo$USUBJID %in% adsl$USUBJID)
  )
)

add_check(
  "No duplicate hypoglycemia sequence identifiers",
  !anyDuplicated(hypo[, c("USUBJID", "HYPOSEQ")]),
  paste(
    "Duplicates:",
    sum(duplicated(hypo[, c("USUBJID", "HYPOSEQ")]))
  )
)

# 8. Print QC report
cat("\n========== FINAL QC AUDIT: DATASET INTEGRITY ==========\n")

print(
  qc,
  row.names = FALSE,
  right = FALSE
)

cat("\nQC SUMMARY\n")
print(table(qc$Result))

# 9. Save QC report
dir.create(
  "outputs/qc",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  qc,
  "outputs/qc/final_dataset_integrity_qc.csv",
  row.names = FALSE
)

cat("\nQC report saved successfully.\n")
