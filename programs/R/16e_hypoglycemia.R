
# ============================================================
# DIA-GLY3 | SAFETY ANALYSIS
# Table T14.3.5 - Hypoglycemia
# ============================================================

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
hypo <- read.csv("data/raw/hypoglycemia.csv")

# 2. Define safety population
safety <- subset(adsl, SAFFL == "Y")

stopifnot(
  !anyDuplicated(safety$USUBJID),
  all(hypo$USUBJID %in% safety$USUBJID),
  !anyDuplicated(hypo[, c("USUBJID", "HYPOSEQ")]),
  !anyNA(hypo)
)

# 3. Add actual treatment assignment
hypo$TRT01A <- safety$TRT01A[
  match(hypo$USUBJID, safety$USUBJID)
]

trts <- c("Placebo", "GLY-301")

N <- sapply(
  trts,
  function(t) sum(safety$TRT01A == t)
)

stopifnot(all(N == 239))

# 4. Inspect hypoglycemia classifications
cat("\n=== HYPOGLYCEMIA LEVELS ===\n")
print(table(hypo$HYPOLEVEL, useNA = "ifany"))

cat("\n=== EXTERNAL ASSISTANCE ===\n")
print(table(
  hypo$EXTERNAL_ASSISTANCE,
  useNA = "ifany"
))

cat("\n=== EVENTS BY TREATMENT ===\n")
print(table(hypo$TRT01A))

# 5. Participant-level incidence
format_incidence <- function(data, treatment) {
  
  n <- length(unique(
    data$USUBJID[data$TRT01A == treatment]
  ))
  
  sprintf(
    "%d (%.1f%%)",
    n,
    100 * n / N[treatment]
  )
}

# 6. Define analysis categories
categories <- list(
  "Any hypoglycemia" = rep(TRUE, nrow(hypo)),
  "Level 1 hypoglycemia" =
    hypo$HYPOLEVEL == "LEVEL 1",
  "Level 2 hypoglycemia" =
    hypo$HYPOLEVEL == "LEVEL 2",
  "Level 3 hypoglycemia" =
    hypo$HYPOLEVEL == "LEVEL 3",
  "Required external assistance" =
    hypo$EXTERNAL_ASSISTANCE == "Y"
)

results <- data.frame(
  Category = names(categories),
  stringsAsFactors = FALSE
)

# 7. Count unique participants by category
for (trt in trts) {
  
  results[[trt]] <- vapply(
    categories,
    function(flag) {
      
      selected <- hypo[
        !is.na(flag) & flag,
        ,
        drop = FALSE
      ]
      
      format_incidence(selected, trt)
    },
    character(1)
  )
}

# 8. Count events and participants with recurrence
event_counts <- sapply(
  trts,
  function(t) sum(hypo$TRT01A == t)
)

participant_counts <- sapply(
  trts,
  function(t) length(unique(
    hypo$USUBJID[hypo$TRT01A == t]
  ))
)

recurrent_counts <- sapply(
  trts,
  function(t) {
    ids <- hypo$USUBJID[hypo$TRT01A == t]
    sum(table(ids) >= 2)
  }
)

# 9. Add event-count row
results <- rbind(
  results,
  data.frame(
    Category = "Total hypoglycemia events",
    Placebo = as.character(event_counts["Placebo"]),
    `GLY-301` = as.character(event_counts["GLY-301"]),
    check.names = FALSE
  )
)

names(results)[2:3] <- c(
  "Placebo (N=239)",
  "GLY-301 (N=239)"
)

# 10. Print
cat("\n========== TABLE T14.3.5 ==========\n")
print(results, row.names = FALSE, right = FALSE)

cat("\n=== RECURRENT HYPOGLYCEMIA ===\n")
print(data.frame(
  Treatment = trts,
  Participants_With_Hypoglycemia =
    as.integer(participant_counts[trts]),
  Participants_With_2plus_Events =
    as.integer(recurrent_counts[trts]),
  Total_Events =
    as.integer(event_counts[trts])
))

cat("\n=== GLUCOSE VALUES BY TREATMENT ===\n")
print(
  aggregate(
    GLUCOSE_MGDL ~ TRT01A,
    data = hypo,
    FUN = function(x) c(
      Mean = mean(x),
      Median = median(x),
      Min = min(x),
      Max = max(x)
    )
  )
)

# 11. Save results
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_3_5_hypoglycemia.csv",
  row.names = FALSE
)

cat("\nTable T14.3.5 saved successfully.\n")
