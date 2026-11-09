
getwd()
file.exists("data/raw/participants_baseline.csv")
list.files(
  path = ".",
  pattern = "participants_baseline.csv",
  recursive = TRUE,
  full.names = TRUE
)

list.files()
list.files("data", recursive = TRUE, full.names = TRUE)
normalizePath(
  "~/DIA-GLY3-Phase-III-Diabetes-Trial/programs/R/02_simulate_trial.R",
  mustWork = FALSE
)


list.files(
  path = path.expand("~"),
  pattern = "participants_baseline\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Location where Day 7 saved the CSV files
source_folder <- "C:/Users/kadri/OneDrive/Documents/data/raw"

# Correct location inside your project
destination_folder <- file.path(getwd(), "data", "raw")

# Create the destination folder if necessary
dir.create(destination_folder, recursive = TRUE, showWarnings = FALSE)

# Find all CSV files
csv_files <- list.files(
  source_folder,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Copy them into the project
copied <- file.copy(
  csv_files,
  destination_folder,
  overwrite = FALSE
)

cat("Files found:", length(csv_files), "\n")
cat("Files copied successfully:", sum(copied), "\n")
list.files("data/raw")

file.exists("data/raw/participants_baseline.csv")


file.exists("data/adam/adsl.csv")
# ============================================================
# DIA-GLY3 
# Subject-Level Analysis Dataset (ADSL-style)
# ============================================================

# Load raw datasets
participants <- read.csv(
  "data/raw/participants_baseline.csv"
)

events <- read.csv(
  "data/raw/participant_events.csv"
)

exposure <- read.csv(
  "data/raw/treatment_exposure.csv"
)

efficacy <- read.csv(
  "data/raw/efficacy_observed.csv"
)

# Ensure participant-level uniqueness
stopifnot(
  !anyDuplicated(participants$USUBJID),
  !anyDuplicated(events$USUBJID),
  !anyDuplicated(exposure$USUBJID)
)

# Merge subject-level information
adsl <- merge(
  participants,
  events[, c(
    "USUBJID", "RESCUE_WEEK", "DISCONT_WEEK"
  )],
  by = "USUBJID",
  all.x = TRUE
)

adsl <- merge(
  adsl,
  exposure[, c(
    "USUBJID", "FIRST_DOSE_DAY",
    "LAST_DOSE_DAY", "DURATION_DAYS",
    "ADHERENCE_PCT", "ADHERENCE_CATEGORY",
    "SAFETY_FU_DAY"
  )],
  by = "USUBJID",
  all.x = TRUE
)

# Treatment variables
adsl$TRT01P <- adsl$TRT
adsl$TRT01A <- adsl$TRT

# Analysis populations
adsl$FASFL <- "Y"
adsl$SAFFL <- ifelse(
  !is.na(adsl$FIRST_DOSE_DAY), "Y", "N"
)

# Intercurrent-event indicators
adsl$RESCUEFL <- ifelse(
  is.na(adsl$RESCUE_WEEK), "N", "Y"
)

adsl$DISCONFL <- ifelse(
  is.na(adsl$DISCONT_WEEK), "N", "Y"
)

# Week-26 HbA1c availability
week26 <- efficacy[
  efficacy$WEEK == 26,
  c("USUBJID", "HBA1C")
]

names(week26)[2] <- "HBA1C_W26"

adsl <- merge(
  adsl, week26,
  by = "USUBJID",
  all.x = TRUE
)

adsl$W26OBSFL <- ifelse(
  is.na(adsl$HBA1C_W26), "N", "Y"
)

# Validation
stopifnot(
  nrow(adsl) == 478,
  !anyDuplicated(adsl$USUBJID),
  all(adsl$FASFL == "Y"),
  all(adsl$SAFFL == "Y"),
  sum(adsl$TRT01P == "GLY-301") == 239,
  sum(adsl$TRT01P == "Placebo") == 239,
  sum(adsl$W26OBSFL == "N") == 96
)

# Save analysis dataset
dir.create(
  "data/adam",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  adsl,
  "data/adam/adsl.csv",
  row.names = FALSE
)

cat("\nADSL created successfully.\n")
cat("Participants:", nrow(adsl), "\n")
cat("Full Analysis Set:",
    sum(adsl$FASFL == "Y"), "\n")
cat("Safety Set:",
    sum(adsl$SAFFL == "Y"), "\n")
cat("Rescue medication:",
    sum(adsl$RESCUEFL == "Y"), "\n")
cat("Treatment discontinuation:",
    sum(adsl$DISCONFL == "Y"), "\n")
cat("Observed Week-26 HbA1c:",
    sum(adsl$W26OBSFL == "Y"), "\n")


cat("Rows:", nrow(adsl),
    "\nUnique participants:", length(unique(adsl$USUBJID)),
    "\nMissing Week-26 HbA1c:", sum(adsl$W26OBSFL == "N"))
