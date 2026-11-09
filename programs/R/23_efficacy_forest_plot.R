# DIA-GLY3
# Figure F14.2.2 - Forest Plot of Efficacy Treatment Effects

# 1. Read existing statistical results
primary <- read.csv(
  "outputs/tables/T14_2_1_primary_efficacy.csv"
)

responder <- read.csv(
  "outputs/tables/T14_2_2_logistic_results.csv"
)

fpg <- read.csv(
  "outputs/tables/T14_2_3_fpg_ancova.csv"
)

weight <- read.csv(
  "outputs/tables/T14_2_4_weight_ancova.csv"
)

# 2. Extract primary HbA1c treatment effect
hba1c_est <- as.numeric(
  primary$GLY301[
    primary$Parameter == "Adjusted treatment difference"
  ]
)

hba1c_ci_text <- primary$GLY301[
  primary$Parameter == "95% confidence interval"
]

hba1c_ci <- as.numeric(
  strsplit(gsub("[() ]", "", hba1c_ci_text), ",")[[1]]
)

# 3. Extract responder odds ratio
resp <- responder[
  responder$term == "TRTGLY-301", 
]

resp_est <- exp(resp$estimate)
resp_low <- exp(resp$conf.low)
resp_high <- exp(resp$conf.high)

# 4. Extract FPG treatment effect
fpg_trt <- fpg[
  fpg$term == "TRTGLY-301",
]

# 5. Extract body weight treatment effect
wt_est <- weight$Estimate
wt_low <- weight$CI_Lower
wt_high <- weight$CI_Upper

# 6. Define plotting function
draw_forest <- function(est, low, high, label,
                        null_value, x_limits,
                        log_scale = FALSE) {
  
  plot(
    NA,
    xlim = x_limits,
    ylim = c(0.5, 1.5),
    yaxt = "n",
    ylab = "",
    xlab = label,
    log = if (log_scale) "x" else "",
    bty = "n"
  )
  
  abline(v = null_value, lty = 2, col = "gray50")
  
  segments(low, 1, high, 1, lwd = 2)
  points(est, 1, pch = 19, cex = 1.3)
  points(c(low, high), c(1, 1), pch = "|")
  
  title(
    main = sprintf(
      "Estimate: %.3f (95%% CI: %.3f, %.3f)",
      est, low, high
    ),
    cex.main = 0.85
  )
}

# 7. Create output directory
dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# 8. Generate PDF
pdf(
  "outputs/figures/F14_2_2_efficacy_forest_plot.pdf",
  width = 10,
  height = 9
)

par(
  mfrow = c(4, 1),
  mar = c(4, 5, 3, 2),
  oma = c(0, 0, 3, 0)
)

# Panel 1: HbA1c
draw_forest(
  hba1c_est,
  hba1c_ci[1],
  hba1c_ci[2],
  "HbA1c adjusted difference (%)",
  0,
  c(-1.2, 0.4)
)

# Panel 2: Responder odds ratio
draw_forest(
  resp_est,
  resp_low,
  resp_high,
  "HbA1c responder odds ratio (log scale)",
  1,
  c(0.7, 6),
  log_scale = TRUE
)

# Panel 3: FPG
draw_forest(
  fpg_trt$estimate,
  fpg_trt$conf.low,
  fpg_trt$conf.high,
  "FPG adjusted difference (mg/dL)",
  0,
  c(-25, 5)
)

# Panel 4: Body weight
draw_forest(
  wt_est,
  wt_low,
  wt_high,
  "Body weight adjusted difference (kg)",
  0,
  c(-2.3, 0.4)
)

mtext(
  "Figure F14.2.2 - Efficacy Treatment Effects",
  outer = TRUE,
  side = 3,
  line = 1,
  font = 2,
  cex = 1.2
)

dev.off()

cat("Forest plot successfully generated.\n")