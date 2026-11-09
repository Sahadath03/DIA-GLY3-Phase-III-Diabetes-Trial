
# ============================================================
# DIA-GLY3 | SAFETY ANALYSIS
# Table T14.3.6 - Treatment Exposure and Adherence
# ============================================================

adsl <- read.csv("data/adam/adsl.csv")

safety <- subset(adsl, SAFFL == "Y")
trts <- c("Placebo", "GLY-301")

stopifnot(
  !anyDuplicated(safety$USUBJID),
  all(safety$TRT01A %in% trts),
  all(table(factor(safety$TRT01A, levels = trts)) == 239),
  !anyNA(safety[, c(
    "DURATION_DAYS",
    "ADHERENCE_PCT",
    "ADHERENCE_CATEGORY"
  )])
)

cat("\n=== ADHERENCE CATEGORIES ===\n")
print(table(
  safety$ADHERENCE_CATEGORY,
  safety$TRT01A
))

cat("\n=== EXPOSURE DURATION VALIDATION ===\n")
print(summary(safety$DURATION_DAYS))

# Descriptive statistics
summarize_variable <- function(x) {
  c(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Median = median(x),
    Min = min(x),
    Max = max(x)
  )
}

format_stats <- function(x) {
  s <- summarize_variable(x)
  c(
    sprintf("%d", s["N"]),
    sprintf("%.1f (%.1f)", s["Mean"], s["SD"]),
    sprintf("%.1f", s["Median"]),
    sprintf("%.1f, %.1f", s["Min"], s["Max"])
  )
}

measures <- list(
  "Treatment duration (days)" = "DURATION_DAYS",
  "Treatment adherence (%)" = "ADHERENCE_PCT"
)

rows <- list()

for (measure in names(measures)) {
  variable <- measures[[measure]]
  
  stats <- lapply(trts, function(t) {
    format_stats(safety[
      safety$TRT01A == t,
      variable
    ])
  })
  
  rows[[measure]] <- data.frame(
    Measure = measure,
    Statistic = c(
      "N", "Mean (SD)", "Median", "Min, Max"
    ),
    Placebo = stats[[1]],
    GLY301 = stats[[2]]
  )
}

results <- do.call(rbind, rows)
row.names(results) <- NULL

# Adherence category counts
categories <- sort(unique(safety$ADHERENCE_CATEGORY))

category_rows <- do.call(
  rbind,
  lapply(categories, function(cat) {
    counts <- sapply(trts, function(t) {
      sum(
        safety$TRT01A == t &
          safety$ADHERENCE_CATEGORY == cat
      )
    })
    
    data.frame(
      Measure = "Adherence category",
      Statistic = cat,
      Placebo = sprintf(
        "%d (%.1f%%)",
        counts["Placebo"],
        100 * counts["Placebo"] / 239
      ),
      GLY301 = sprintf(
        "%d (%.1f%%)",
        counts["GLY-301"],
        100 * counts["GLY-301"] / 239
      )
    )
  })
)

results <- rbind(results, category_rows)

names(results)[3:4] <- c(
  "Placebo (N=239)",
  "GLY-301 (N=239)"
)

cat("\n========== TABLE T14.3.6 ==========\n")
print(results, row.names = FALSE, right = FALSE)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_3_6_exposure_adherence.csv",
  row.names = FALSE
)

cat("\nTable T14.3.6 saved successfully.\n")
