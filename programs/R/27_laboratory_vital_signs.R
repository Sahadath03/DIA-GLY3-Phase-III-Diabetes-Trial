# DIA-GLY3
# T14.3.6 - Available Laboratory and Vital-Sign Measurements
# Descriptive analysis; observed data only

lb <- read.csv("data/sdtm/lb.csv")
vs <- read.csv("data/sdtm/vs.csv")
adsl <- read.csv("data/adam/adsl.csv")

stopifnot(
  !anyDuplicated(adsl$USUBJID),
  all(c("USUBJID", "TRT") %in% names(adsl))
)

# Standardize laboratory and vital-sign records
lab <- data.frame(
  USUBJID = lb$USUBJID,
  Measurement = lb$LBTEST,
  Unit = lb$LBSTRESU,
  Visit = lb$VISIT,
  Value = lb$LBSTRESN
)

vital <- data.frame(
  USUBJID = vs$USUBJID,
  Measurement = vs$VSTEST,
  Unit = vs$VSSTRESU,
  Visit = vs$VISIT,
  Value = vs$VSSTRESN
)

measurements <- rbind(lab, vital)

# Add treatment assignments
measurements <- merge(
  measurements,
  adsl[, c("USUBJID", "TRT")],
  by = "USUBJID"
)

# Ensure each participant has at most one value
# per measurement and visit
stopifnot(
  !anyDuplicated(
    measurements[, c("USUBJID", "Measurement", "Visit")]
  )
)

# Calculate paired baseline-to-Week-26 changes
baseline <- measurements[
  measurements$Visit == "Baseline",
  c("USUBJID", "TRT", "Measurement", "Value")
]

week26 <- measurements[
  measurements$Visit == "Week 26",
  c("USUBJID", "TRT", "Measurement", "Value")
]

names(baseline)[4] <- "Baseline"
names(week26)[4] <- "Week26"

paired <- merge(
  baseline,
  week26,
  by = c("USUBJID", "TRT", "Measurement")
)

paired$Change <- paired$Week26 - paired$Baseline

# Format observed n, mean, and SD
summarize_values <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return("0; NA")
  }
  
  if (length(x) == 1) {
    return(sprintf("1; %.2f (NA)", mean(x)))
  }
  
  sprintf(
    "%d; %.2f (%.2f)",
    length(x), mean(x), sd(x)
  )
}

results <- list()
k <- 1

for (measurement in unique(paired$Measurement)) {
  
  for (arm in c("GLY-301", "Placebo")) {
    
    d <- paired[
      paired$Measurement == measurement &
        paired$TRT == arm,
    ]
    
    results[[k]] <- data.frame(
      Measurement = measurement,
      Treatment = arm,
      Baseline = summarize_values(d$Baseline),
      Week26 = summarize_values(d$Week26),
      Change_from_Baseline = summarize_values(
        d$Change
      )
    )
    
    k <- k + 1
  }
}

table_14_3_6 <- do.call(rbind, results)

row.names(table_14_3_6) <- NULL

# Save output
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  table_14_3_6,
  "outputs/tables/T14_3_6_laboratory_vital_signs.csv",
  row.names = FALSE
)

print(table_14_3_6, row.names = FALSE)

cat(
  "\nSaved: outputs/tables/",
  "T14_3_6_laboratory_vital_signs.csv\n",
  sep = ""
)