# DIA-GLY3
# Figure F14.3.1 - Treatment-Emergent Adverse Event Incidence

# Read existing safety results
safety <- read.csv(
  "outputs/tables/T14_3_1_overall_teae.csv",
  stringsAsFactors = FALSE
)

# Verify expected columns and categories
stopifnot(
  nrow(safety) == 8,
  all(c(
    "Category",
    "Placebo..N.239.",
    "GLY.301..N.239."
  ) %in% names(safety))
)

# Extract percentage from strings such as "82 (34.3%)"
get_percent <- function(x) {
  as.numeric(
    sub(".*\\(([0-9.]+)%\\).*", "\\1", x)
  )
}

placebo_pct <- get_percent(safety$Placebo..N.239.)
gly_pct <- get_percent(safety$GLY.301..N.239.)

stopifnot(
  all(is.finite(placebo_pct)),
  all(is.finite(gly_pct))
)

# Arrange data for grouped horizontal bar chart
values <- rbind(
  "Placebo" = placebo_pct,
  "GLY-301" = gly_pct
)

# Shorter labels for readability
labels <- c(
  "Any TEAE",
  "Treatment-related TEAE",
  "Mild TEAE",
  "Moderate TEAE",
  "Severe TEAE",
  "Any serious TEAE",
  "TEAE leading discontinuation",
  "TEAE leading dose interruption"
)

# Create output directory
dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Generate PDF
pdf(
  "outputs/figures/F14_3_1_teae_incidence.pdf",
  width = 12,
  height = 8
)

par(mar = c(5, 16, 4, 2))

bar_positions <- barplot(
  values[, 8:1],
  beside = TRUE,
  horiz = TRUE,
  names.arg = labels[8:1],
  col = c("#A8B8CC", "#3478B8"),
  border = NA,
  xlim = c(0, 55),
  las = 1,
  xlab = "Participants (%)",
  main = "Figure F14.3.1 - Treatment-Emergent Adverse Events"
)

# Add percentage labels
text(
  x = as.vector(values[, 8:1]) + 1,
  y = as.vector(bar_positions),
  labels = sprintf("%.1f%%", as.vector(values[, 8:1])),
  cex = 0.8,
  pos = 4
)

legend(
  "bottomright",
  legend = c("Placebo (N=239)", "GLY-301 (N=239)"),
  fill = c("#A8B8CC", "#3478B8"),
  bty = "n"
)

dev.off()

cat("Saved: outputs/figures/F14_3_1_teae_incidence.pdf\n")