
# DIA-GLY3
# T14.1.2 Demographic and Baseline Characteristics

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE
)

# Verify participant-level data
stopifnot(
  !anyDuplicated(adsl$USUBJID),
  nrow(adsl) == 478
)

# Verify treatment groups
print(table(adsl$TRT, useNA = "ifany"))
print(table(adsl$SEX, useNA = "ifany"))
print(table(adsl$STRATUM, useNA = "ifany"))

trts <- c("Placebo", "GLY-301")
stopifnot(all(trts %in% unique(adsl$TRT)))

# Format continuous variables as mean (SD)
summ_cont <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return("NE")
  sprintf("%.2f (%.2f)", mean(x), sd(x))
}

# Format categorical variables as n (%)
summ_cat <- function(x, category, denominator) {
  n <- sum(as.character(x) == category, na.rm = TRUE)
  sprintf("%d (%.1f%%)", n, 100 * n / denominator)
}

# Get treatment-specific data
groups <- lapply(trts, function(t) {
  adsl[adsl$TRT == t, ]
})
names(groups) <- trts

# Determine actual categories
sex_levels <- sort(unique(na.omit(as.character(adsl$SEX))))
stratum_levels <- sort(unique(na.omit(as.character(adsl$STRATUM))))

# Build the table
rows <- list()

add_row <- function(label, values) {
  rows[[length(rows) + 1]] <<- c(label, values)
}

add_row("Participants, n", sapply(groups, nrow))
add_row("Age (years), mean (SD)",
        sapply(groups, function(d) summ_cont(d$AGE)))

for (level in sex_levels) {
  add_row(
    paste0("Sex: ", level, ", n (%)"),
    sapply(groups, function(d) {
      summ_cat(d$SEX, level, nrow(d))
    })
  )
}

add_row("BMI (kg/m2), mean (SD)",
        sapply(groups, function(d) summ_cont(d$BMI)))

add_row("Baseline HbA1c (%), mean (SD)",
        sapply(groups, function(d) summ_cont(d$HBA1C_BL)))

for (level in stratum_levels) {
  add_row(
    paste0("Baseline HbA1c stratum: ", level, ", n (%)"),
    sapply(groups, function(d) {
      summ_cat(d$STRATUM, level, nrow(d))
    })
  )
}

add_row("Baseline FPG (mg/dL), mean (SD)",
        sapply(groups, function(d) summ_cont(d$FPG_BL)))

add_row("Baseline body weight (kg), mean (SD)",
        sapply(groups, function(d) summ_cont(d$WEIGHT_BL)))

# Convert rows into data frame
result <- as.data.frame(
  do.call(rbind, rows),
  stringsAsFactors = FALSE
)

names(result) <- c(
  "Characteristic",
  "Placebo (N=239)",
  "GLY-301 (N=239)"
)

# Save output
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  result,
  "outputs/tables/T14_1_2_baseline_characteristics.csv",
  row.names = FALSE
)

print(result, row.names = FALSE)

cat("\nT14.1.2 generated successfully.\n")
