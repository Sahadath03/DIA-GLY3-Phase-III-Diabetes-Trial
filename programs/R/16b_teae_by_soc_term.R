
# ============================================================
# DIA-GLY3 | SAFETY ANALYSIS
# Table T14.3.2 - TEAEs by SOC and Event Term
# ============================================================

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adae <- read.csv("data/adam/adae.csv")

# 2. Define safety population
safety <- subset(
  adsl,
  SAFFL == "Y"
)

# 3. Define treatment-emergent adverse events
ae <- subset(
  adae,
  TRTEMFL == "Y" & SAFFL == "Y"
)

# 4. Validate data
stopifnot(
  !anyDuplicated(safety$USUBJID),
  all(ae$USUBJID %in% safety$USUBJID),
  !anyNA(ae[, c("AESOC", "AETERM", "TRT01A")])
)

trts <- c("Placebo", "GLY-301")

N <- sapply(
  trts,
  function(t) sum(safety$TRT01A == t)
)

# 5. Function to calculate participant incidence
format_incidence <- function(ids, treatment) {
  
  n <- length(unique(ids))
  
  sprintf(
    "%d (%.1f%%)",
    n,
    100 * n / N[treatment]
  )
}

# 6. Summarize events at SOC level
soc_names <- sort(unique(ae$AESOC))

soc_results <- do.call(
  rbind,
  lapply(soc_names, function(soc) {
    
    subset_soc <- ae[ae$AESOC == soc, ]
    
    data.frame(
      SOC = soc,
      Event_Term = "Any event in SOC",
      Level = "SOC",
      Placebo = format_incidence(
        subset_soc$USUBJID[
          subset_soc$TRT01A == "Placebo"
        ],
        "Placebo"
      ),
      GLY301 = format_incidence(
        subset_soc$USUBJID[
          subset_soc$TRT01A == "GLY-301"
        ],
        "GLY-301"
      ),
      stringsAsFactors = FALSE
    )
  })
)

# 7. Summarize events at event-term level
term_groups <- unique(
  ae[, c("AESOC", "AETERM")]
)

term_results <- do.call(
  rbind,
  lapply(seq_len(nrow(term_groups)), function(i) {
    
    soc <- term_groups$AESOC[i]
    term <- term_groups$AETERM[i]
    
    subset_term <- ae[
      ae$AESOC == soc &
        ae$AETERM == term,
    ]
    
    data.frame(
      SOC = soc,
      Event_Term = term,
      Level = "TERM",
      Placebo = format_incidence(
        subset_term$USUBJID[
          subset_term$TRT01A == "Placebo"
        ],
        "Placebo"
      ),
      GLY301 = format_incidence(
        subset_term$USUBJID[
          subset_term$TRT01A == "GLY-301"
        ],
        "GLY-301"
      ),
      stringsAsFactors = FALSE
    )
  })
)

# 8. Combine and sort
results <- rbind(
  soc_results,
  term_results
)

results <- results[
  order(
    results$SOC,
    results$Level != "SOC",
    results$Event_Term
  ),
]

row.names(results) <- NULL

# 9. Rename treatment columns
names(results)[4:5] <- c(
  paste0("Placebo (N=", N["Placebo"], ")"),
  paste0("GLY-301 (N=", N["GLY-301"], ")")
)

# 10. Print
cat("\n========== TABLE T14.3.2 ==========\n")
print(
  results,
  row.names = FALSE,
  right = FALSE
)

# 11. Save
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_3_2_teae_by_soc_term.csv",
  row.names = FALSE
)

cat("\nTable T14.3.2 saved successfully.\n")
