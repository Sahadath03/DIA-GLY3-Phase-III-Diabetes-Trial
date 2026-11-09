
# DIA-GLY3
# T14.1.3 - Partial Protocol Compliance Assessment
# Evaluates three available baseline eligibility criteria.
# Does not represent a complete protocol deviation assessment.

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(adsl) == 478,
  !anyDuplicated(adsl$USUBJID),
  all(!is.na(adsl[, c("AGE", "HBA1C_BL", "BMI")])),
  all(adsl$TRT %in% c("GLY-301", "Placebo"))
)

# Potential violations of available eligibility criteria
adsl$AGE_VIOL <- adsl$AGE < 18 | adsl$AGE > 75
adsl$HBA1C_VIOL <- adsl$HBA1C_BL < 7.5 | adsl$HBA1C_BL > 10.5
adsl$BMI_VIOL <- adsl$BMI < 20 | adsl$BMI > 45

adsl$ANY_EVALUATED_VIOL <- (
  adsl$AGE_VIOL |
    adsl$HBA1C_VIOL |
    adsl$BMI_VIOL
)

criteria <- c(
  "Age outside 18-75 years",
  "Baseline HbA1c outside 7.5-10.5%",
  "BMI outside 20-45 kg/m2",
  "At least one evaluated eligibility violation"
)

variables <- c(
  "AGE_VIOL",
  "HBA1C_VIOL",
  "BMI_VIOL",
  "ANY_EVALUATED_VIOL"
)

summarize_arm <- function(arm) {
  subset_data <- adsl[adsl$TRT == arm, ]
  n <- nrow(subset_data)
  
  vapply(variables, function(v) {
    count <- sum(subset_data[[v]])
    sprintf("%d (%.1f%%)", count, 100 * count / n)
  }, character(1))
}

table_14_1_3 <- data.frame(
  Criterion = criteria,
  `GLY-301 (N=239)` = summarize_arm("GLY-301"),
  `Placebo (N=239)` = summarize_arm("Placebo"),
  check.names = FALSE
)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  table_14_1_3,
  "outputs/tables/T14_1_3_partial_protocol_compliance.csv",
  row.names = FALSE
)

print(table_14_1_3, row.names = FALSE)
