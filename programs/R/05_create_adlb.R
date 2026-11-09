
# ============================================================
# DIA-GLY3 
# Laboratory Analysis Dataset (ADaM-style)
# ============================================================

# 1. Check required input files
required_files <- c(
  "data/adam/adsl.csv",
  "data/adam/adeff.csv"
)

stopifnot(all(file.exists(required_files)))

# 2. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")

stopifnot(
  nrow(adsl) == 478,
  nrow(adeff) == 2868,
  !anyDuplicated(adeff[c("USUBJID", "WEEK")])
)

# 3. Create a reusable laboratory transformation
make_lab <- function(data, value_col, base_col,
                     parameter, code, unit) {
  
  out <- data.frame(
    USUBJID = data$USUBJID,
    TRT01P = data$TRT01P,
    FASFL = data$FASFL,
    PARAM = parameter,
    PARAMCD = code,
    AVISIT = data$AVISIT,
    AVISITN = data$AVISITN,
    AVAL = data[[value_col]],
    BASE = data[[base_col]],
    AVALU = unit,
    stringsAsFactors = FALSE
  )
  
  out$CHG <- out$AVAL - out$BASE
  
  # A baseline observation has zero change
  out$CHG[out$AVISITN == 0 &
            !is.na(out$AVAL)] <- 0
  
  out
}

# 4. Transform HbA1c
hba1c <- make_lab(
  adeff,
  value_col = "HBA1C",
  base_col = "HBA1C_BL",
  parameter = "Hemoglobin A1c",
  code = "HBA1C",
  unit = "%"
)

# 5. Transform fasting plasma glucose
fpg <- make_lab(
  adeff,
  value_col = "FPG",
  base_col = "FPG_BL",
  parameter = "Fasting Plasma Glucose",
  code = "FPG",
  unit = "mg/dL"
)

# 6. Combine parameters
adlb <- rbind(hba1c, fpg)

adlb <- adlb[
  order(adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN),
]

rownames(adlb) <- NULL

# 7. Add analysis flags
adlb$ANL01FL <- ifelse(
  adlb$AVISITN > 0 & !is.na(adlb$AVAL),
  "Y", "N"
)

adlb$W26FL <- ifelse(
  adlb$AVISITN == 26, "Y", "N"
)

# 8. Validate
stopifnot(
  nrow(adlb) == 478 * 6 * 2,
  !anyDuplicated(
    adlb[c("USUBJID", "PARAMCD", "AVISITN")]
  ),
  all(adlb$PARAMCD %in% c("HBA1C", "FPG")),
  all(adlb$AVALU[adlb$PARAMCD == "HBA1C"] == "%"),
  all(adlb$AVALU[adlb$PARAMCD == "FPG"] == "mg/dL"),
  all(
    adlb$CHG[
      adlb$AVISITN == 0 & !is.na(adlb$AVAL)
    ] == 0
  )
)

# 9. Save dataset
write.csv(
  adlb,
  "data/adam/adlb.csv",
  row.names = FALSE
)

# 10. Summary
cat("\nADLB created successfully.\n")
cat("Total laboratory records:", nrow(adlb), "\n")
cat("Unique participants:",
    length(unique(adlb$USUBJID)), "\n")

cat("\nRecords by parameter:\n")
print(table(adlb$PARAMCD))

cat("\nMissing Week-26 values:\n")
print(
  with(
    subset(adlb, AVISITN == 26),
    tapply(is.na(AVAL), PARAMCD, sum)
  )
)
