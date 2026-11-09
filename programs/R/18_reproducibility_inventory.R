
# ============================================================
# DIA-GLY3 | REPRODUCIBILITY INVENTORY
# ============================================================

scripts <- list.files(
  "programs/R",
  pattern = "\\.R$",
  full.names = TRUE
)

# Exclude this inventory script
scripts <- scripts[
  basename(scripts) != "18_reproducibility_inventory.R"
]

# Identify common file dependencies and outputs
patterns <- c(
  "read.csv",
  "readRDS",
  "write.csv",
  "saveRDS",
  "source\\(",
  "pdf\\(",
  "ggsave\\("
)

inventory <- do.call(
  rbind,
  lapply(scripts, function(script) {
    
    lines <- readLines(
      script,
      warn = FALSE
    )
    
    # Exclude blank lines and comments
    lines <- trimws(lines)
    lines <- lines[
      nzchar(lines) &
        !grepl("^#", lines)
    ]
    
    matches <- lines[
      grepl(
        paste(patterns, collapse = "|"),
        lines
      )
    ]
    
    if (length(matches) == 0) {
      matches <- NA_character_
    }
    
    data.frame(
      Script = basename(script),
      Statement = matches,
      stringsAsFactors = FALSE
    )
  })
)

dir.create(
  "outputs/qc",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  inventory,
  "outputs/qc/reproducibility_inventory.csv",
  row.names = FALSE
)

cat("\n========== REPRODUCIBILITY INVENTORY ==========\n")

cat("Scripts inspected:", length(scripts), "\n")
cat("Dependency/output statements:", nrow(inventory), "\n")

cat("\n========== SOURCE CALLS ==========\n")

source_calls <- subset(
  inventory,
  grepl("source\\(", Statement)
)

if (nrow(source_calls) == 0) {
  cat("No source() calls detected.\n")
} else {
  print(source_calls, row.names = FALSE)
}

cat("\n========== INVENTORY SAVED ==========\n")
cat("outputs/qc/reproducibility_inventory.csv\n")
