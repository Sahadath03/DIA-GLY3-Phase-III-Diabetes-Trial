# DIA-GLY3
# Figure F14.2.3 - MNAR Tipping-Point Analysis

# Read previously generated sensitivity results
delta <- read.csv(
  "outputs/tables/T14_2_6_mnar_delta_sensitivity.csv"
)

refined <- read.csv(
  "outputs/tables/T14_2_6_mnar_refined_tipping.csv"
)

# Check required variables
required <- c(
  "Delta", "Estimate", "Lower95", "Upper95", "P_value"
)

stopifnot(
  all(required %in% names(delta)),
  all(required %in% names(refined))
)

# Find first nonsignificant point on the refined grid
refined <- refined[order(refined$Delta), ]

tipping <- refined[
  which(refined$P_value >= 0.05)[1],
]

stopifnot(nrow(tipping) == 1)

cat("First nonsignificant delta:", tipping$Delta, "\n")
cat("P-value at tipping point:", tipping$P_value, "\n")

# Combine results for a smooth curve
combined <- rbind(
  delta[, required],
  refined[, required]
)

combined <- combined[
  order(combined$Delta),
]

combined <- combined[
  !duplicated(combined$Delta),
]

# Create output directory
dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Generate figure
pdf(
  "outputs/figures/F14_2_3_mnar_tipping_point.pdf",
  width = 10,
  height = 7
)

par(mar = c(5, 5, 4, 2))

plot(
  combined$Delta,
  combined$Estimate,
  type = "l",
  lwd = 2,
  col = "blue",
  ylim = range(c(combined$Lower95, combined$Upper95, 0)),
  xlab = "Delta adjustment (HbA1c percentage points)",
  ylab = "Adjusted treatment difference (GLY-301 - Placebo)",
  main = "Figure F14.2.3 - MNAR Tipping-Point Analysis"
)

# Confidence interval boundaries
lines(combined$Delta, combined$Lower95,
      col = "gray40", lty = 2)

lines(combined$Delta, combined$Upper95,
      col = "gray40", lty = 2)

# No-treatment-effect reference line
abline(h = 0, lty = 3, col = "black")

# Tipping-point reference line
abline(v = tipping$Delta, lty = 2, col = "red")

# Highlight tipping point
points(
  tipping$Delta,
  tipping$Estimate,
  pch = 19,
  col = "red",
  cex = 1.3
)

legend(
  "topleft",
  legend = c(
    "Estimated treatment difference",
    "95% confidence interval",
    "No treatment effect",
    paste0("First p >= 0.05: delta = ", tipping$Delta)
  ),
  col = c("blue", "gray40", "black", "red"),
  lty = c(1, 2, 3, 2),
  lwd = c(2, 1, 1, 1),
  bty = "n",
  cex = 0.85
)

dev.off()

cat(
  "Saved: outputs/figures/F14_2_3_mnar_tipping_point.pdf\n"
)