
# ============================================================
# DIA-GLY3 | Figure F14.2.1
# Supportive MMRM: Adjusted HbA1c Treatment Differences
# ============================================================

# Load saved results
results <- read.csv(
  "outputs/tables/T14_2_5_mmrm_by_visit.csv"
)

# Ensure visits are in chronological order
results$WEEK <- as.numeric(as.character(results$VISIT))
results <- results[order(results$WEEK), ]

# Validate input
stopifnot(
  nrow(results) == 5,
  all(c("estimate", "lower.CL", "upper.CL") %in% names(results)),
  !anyNA(results[, c("WEEK", "estimate", "lower.CL", "upper.CL")])
)

# Create output directory
dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save publication-quality PDF
pdf(
  "outputs/figures/F14_2_1_mmrm_hba1c.pdf",
  width = 8,
  height = 5.5
)

par(mar = c(5, 5, 4, 2))

plot(
  results$WEEK,
  results$estimate,
  type = "b",
  pch = 19,
  lwd = 2,
  xlab = "Study Week",
  ylab = "Adjusted HbA1c Difference (percentage points)",
  main = "MMRM: GLY-301 Minus Placebo",
  ylim = range(c(0, results$lower.CL, results$upper.CL)),
  xaxt = "n"
)

axis(1, at = results$WEEK)

# 95% confidence intervals
arrows(
  results$WEEK,
  results$lower.CL,
  results$WEEK,
  results$upper.CL,
  angle = 90,
  code = 3,
  length = 0.07
)

# No-treatment-difference reference
abline(h = 0, lty = 2, col = "gray50")

mtext(
  "Negative values favor GLY-301; simulated trial data",
  side = 1,
  line = 3.7,
  cex = 0.75
)

dev.off()

cat(
  "\nFigure saved: outputs/figures/F14_2_1_mmrm_hba1c.pdf\n"
)
