# DIA-GLY3
# Listing L16.1.1 - Participant Disposition

# Read SDTM and ADaM datasets
ds <- read.csv(
  "data/sdtm/ds.csv",
  stringsAsFactors = FALSE
)

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE
)

# Validate participant identifiers
stopifnot(
  nrow(ds) == 478,
  nrow(adsl) == 478,
  !anyDuplicated(ds$USUBJID),
  !anyDuplicated(adsl$USUBJID),
  setequal(ds$USUBJID, adsl$USUBJID)
)

# Validate disposition categories and terms
stopifnot(
  all(ds$DSCAT == "DISPOSITION EVENT"),
  all(ds$DSSCAT == "STUDY TREATMENT"),
  all(ds$DSTERM %in% c(
    "COMPLETED PLANNED TREATMENT",
    "DISCONTINUED STUDY TREATMENT"
  ))
)

# Merge participant disposition and analysis population data
listing <- merge(
  ds[, c("USUBJID", "DSTERM", "DSSTDY")],
  adsl[, c(
    "USUBJID", "SITEID", "TRT",
    "FASFL", "SAFFL", "DISCONT_WEEK"
  )],
  by = "USUBJID",
  all = FALSE
)

# Rename columns
names(listing) <- c(
  "Participant_ID",
  "Disposition",
  "Disposition_Study_Day",
  "Site_ID",
  "Treatment",
  "FAS_Flag",
  "Safety_Flag",
  "Discontinuation_Week"
)

# Sort by treatment and participant
listing <- listing[
  order(listing$Treatment, listing$Participant_ID),
]

row.names(listing) <- NULL

# Validate counts by treatment
print(table(listing$Treatment, listing$Disposition))

stopifnot(
  sum(listing$Disposition ==
        "COMPLETED PLANNED TREATMENT") == 420,
  sum(listing$Disposition ==
        "DISCONTINUED STUDY TREATMENT") == 58,
  sum(listing$Treatment == "GLY-301") == 239,
  sum(listing$Treatment == "Placebo") == 239
)

# Create output directory
dir.create(
  "outputs/listings",
  recursive = TRUE,
  showWarnings = FALSE
)

# Export listing
write.csv(
  listing,
  "outputs/listings/L16_1_1_participant_disposition.csv",
  row.names = FALSE,
  na = ""
)

cat(
  "\nListing saved:",
  "outputs/listings/L16_1_1_participant_disposition.csv\n"
)

cat("Total participants:", nrow(listing), "\n")