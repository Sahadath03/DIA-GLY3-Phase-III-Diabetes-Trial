
# ============================================================
# DIA-GLY3 
# Efficacy Analysis Dataset (ADaM-style)
# ============================================================

# Load datasets
adsl <- read.csv("data/adam/adsl.csv")
eff <- read.csv("data/raw/efficacy_observed.csv")

# Check source data
stopifnot(
  nrow(adsl) == 478,
  !anyDuplicated(adsl$USUBJID),
  !anyDuplicated(eff[c("USUBJID", "WEEK")]),
  all(eff$USUBJID %in% adsl$USUBJID)
)

# Create long-format analysis dataset
adeff <- merge(
  eff,
  adsl[, c(
    "USUBJID", "TRT01P", "TRT01A",
    "HBA1C_BL", "FPG_BL", "WEIGHT_BL",
    "STRATUM", "FASFL", "RESCUEFL",
    "DISCONFL"
  )],
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

adeff <- adeff[
  order(adeff$USUBJID, adeff$WEEK),
]

# Visit variables
adeff$AVISIT <- ifelse(
  adeff$WEEK == 0,
  "Baseline",
  paste("Week", adeff$WEEK)
)

adeff$AVISITN <- adeff$WEEK

# Efficacy changes from baseline
# Use the observed Week-0 values as analysis baselines
baseline <- adeff[
  adeff$WEEK == 0,
  c("USUBJID", "HBA1C", "FPG", "WEIGHT")
]

names(baseline)[2:4] <- c(
  "BASE_HBA1C", "BASE_FPG", "BASE_WEIGHT"
)

adeff <- merge(
  adeff,
  baseline,
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

adeff <- adeff[
  order(adeff$USUBJID, adeff$WEEK),
]

# Harmonize analysis baselines with recorded visit precision
adeff$HBA1C_BL <- round(adeff$HBA1C_BL, 2)
adeff$FPG_BL <- round(adeff$FPG_BL, 1)
adeff$WEIGHT_BL <- round(adeff$WEIGHT_BL, 1)

# Calculate changes from baseline
adeff$CHG_HBA1C <- adeff$HBA1C - adeff$HBA1C_BL
adeff$CHG_FPG <- adeff$FPG - adeff$FPG_BL
adeff$CHG_WEIGHT <- adeff$WEIGHT - adeff$WEIGHT_BL

# Analysis flags
adeff$ANL01FL <- ifelse(
  adeff$WEEK > 0 & !is.na(adeff$HBA1C),
  "Y", "N"
)

adeff$W26FL <- ifelse(
  adeff$WEEK == 26, "Y", "N"
)

# Post-rescue and post-discontinuation observations
adeff$POSTRESFL <- ifelse(
  adeff$POST_RESCUE, "Y", "N"
)

adeff$POSTDISFL <- ifelse(
  adeff$POST_DISCONT, "Y", "N"
)

# Week-26 HbA1c responder indicator
# NA remains missing for later multiple imputation
adeff$RESP7 <- NA_integer_

idx <- adeff$WEEK == 26 & !is.na(adeff$HBA1C)

adeff$RESP7[idx] <- as.integer(
  adeff$HBA1C[idx] < 7
)

# Validation
stopifnot(
  nrow(adeff) == 478 * 6,
  !anyDuplicated(adeff[c("USUBJID", "WEEK")]),
  sum(adeff$W26FL == "Y") == 478,
  sum(
    is.na(adeff$HBA1C[adeff$WEEK == 26])
  ) == 96,
  all(
    adeff$CHG_HBA1C[adeff$WEEK == 0] == 0
  ),
  all(
    is.na(adeff$WEIGHT[adeff$WEEK == 4])
  )
)

# Save ADEFF
write.csv(
  adeff,
  "data/adam/adeff.csv",
  row.names = FALSE
)

cat("\nADEFF created successfully.\n")
cat("Total records:", nrow(adeff), "\n")
cat("Unique participants:",
    length(unique(adeff$USUBJID)), "\n")
cat("Week-26 records:",
    sum(adeff$W26FL == "Y"), "\n")
cat("Observed Week-26 HbA1c:",
    sum(
      adeff$W26FL == "Y" &
        !is.na(adeff$HBA1C)
    ), "\n")
cat("Missing Week-26 HbA1c:",
    sum(
      adeff$W26FL == "Y" &
        is.na(adeff$HBA1C)
    ), "\n")


