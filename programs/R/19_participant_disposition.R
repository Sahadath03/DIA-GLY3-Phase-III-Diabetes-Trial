
# DIA-GLY3: T14.1.1 Participant Disposition

adsl <- read.csv("data/adam/adsl.csv", stringsAsFactors = FALSE)
ds <- read.csv("data/sdtm/ds.csv", stringsAsFactors = FALSE)

# Confirm one record per participant
stopifnot(!anyDuplicated(adsl$USUBJID))

# Keep study-treatment disposition events
ds_trt <- subset(
  ds,
  DSCAT == "DISPOSITION EVENT" &
    DSSCAT == "STUDY TREATMENT"
)

# Confirm no duplicate treatment disposition records
stopifnot(!anyDuplicated(ds_trt$USUBJID))

# Match disposition to ADSL
df <- merge(
  adsl[, c("USUBJID", "TRT", "FASFL", "SAFFL")],
  ds_trt[, c("USUBJID", "DSTERM")],
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

# Check for missing disposition information
stopifnot(!anyNA(df$DSTERM))

# Inspect categories before summarizing
print(table(df$TRT, df$DSTERM, useNA = "ifany"))

# Create counts by treatment
treatments <- sort(unique(df$TRT))
stopifnot(length(treatments) == 2)

row_labels <- c(
  "Randomized",
  "Full Analysis Set",
  "Safety Analysis Set",
  "Completed planned treatment",
  "Discontinued study treatment"
)

count_rows <- function(d) {
  c(
    nrow(d),
    sum(d$FASFL == "Y", na.rm = TRUE),
    sum(d$SAFFL == "Y", na.rm = TRUE),
    sum(d$DSTERM == "COMPLETED PLANNED TREATMENT"),
    sum(d$DSTERM == "DISCONTINUED STUDY TREATMENT")
  )
}

counts <- sapply(treatments, function(trt) {
  count_rows(df[df$TRT == trt, ])
})

denominators <- colSums(counts[1, , drop = FALSE])

format_n_pct <- function(n, total) {
  sprintf("%d (%.1f%%)", n, 100 * n / total)
}

formatted <- sapply(seq_along(treatments), function(j) {
  format_n_pct(counts[, j], denominators[j])
})

result <- data.frame(
  Parameter = row_labels,
  formatted,
  check.names = FALSE
)

names(result)[-1] <- treatments

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(
  result,
  "outputs/tables/T14_1_1_participant_disposition.csv",
  row.names = FALSE
)

print(result, row.names = FALSE)

# Reconciliation check
stopifnot(
  all(counts[4, ] + counts[5, ] == counts[1, ])
)

cat("\nT14.1.1 generated successfully.\n")
