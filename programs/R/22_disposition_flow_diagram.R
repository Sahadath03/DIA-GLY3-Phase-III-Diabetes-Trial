
# DIA-GLY3 - Figure F14.1.1
# Participant disposition by randomized treatment

adsl <- read.csv("data/adam/adsl.csv")
ds <- read.csv("data/sdtm/ds.csv")

stopifnot(
  nrow(adsl) == 478,
  !anyDuplicated(adsl$USUBJID)
)

# Read the previously validated disposition table
disp <- read.csv(
  "outputs/tables/T14_1_1_participant_disposition.csv",
  check.names = FALSE
)

print(disp)

# Use verified counts from T14.1.1
n_randomized <- 478
n_gly <- 239
n_placebo <- 239
gly_completed <- 208
gly_discontinued <- 31
placebo_completed <- 212
placebo_discontinued <- 27

stopifnot(
  n_gly + n_placebo == n_randomized,
  gly_completed + gly_discontinued == n_gly,
  placebo_completed + placebo_discontinued == n_placebo,
  sum(adsl$TRT == "GLY-301") == n_gly,
  sum(adsl$TRT == "Placebo") == n_placebo
)

dir.create("outputs/figures", recursive = TRUE,
           showWarnings = FALSE)

pdf("outputs/figures/F14_1_1_disposition.pdf",
    width = 10, height = 7)

plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))

draw_box <- function(x1, y1, x2, y2, label,
                     fill = "white") {
  rect(x1, y1, x2, y2, col = fill, border = "black")
  text((x1+x2)/2, (y1+y2)/2, label, cex = 0.95)
}

draw_arrow <- function(x1, y1, x2, y2) {
  arrows(x1, y1, x2, y2, length = 0.12)
}

draw_box(3.5, 8.5, 6.5, 9.5,
         "Randomized\nN = 478", "#E8EFF8")

draw_box(1, 5.9, 4, 7.1,
         "GLY-301 + metformin\nN = 239", "#E8EFF8")

draw_box(6, 5.9, 9, 7.1,
         "Placebo + metformin\nN = 239", "#E8EFF8")

segments(5, 8.5, 5, 7.8)
segments(2.5, 7.8, 7.5, 7.8)
draw_arrow(2.5, 7.8, 2.5, 7.1)
draw_arrow(7.5, 7.8, 7.5, 7.1)

draw_box(0.6, 3.1, 4.4, 4.5,
         "Completed planned treatment\nn = 208 (87.0%)")
draw_box(0.6, 1.0, 4.4, 2.4,
         "Discontinued study treatment\nn = 31 (13.0%)")

draw_box(5.6, 3.1, 9.4, 4.5,
         "Completed planned treatment\nn = 212 (88.7%)")
draw_box(5.6, 1.0, 9.4, 2.4,
         "Discontinued study treatment\nn = 27 (11.3%)")

segments(2.5, 5.9, 2.5, 5.2)
segments(2.5, 5.2, 2.5, 4.5)
draw_arrow(2.5, 3.1, 2.5, 2.4)

segments(7.5, 5.9, 7.5, 5.2)
segments(7.5, 5.2, 7.5, 4.5)
draw_arrow(7.5, 3.1, 7.5, 2.4)

title("Figure F14.1.1 — Participant Disposition",
      cex.main = 1.2)

dev.off()

cat("Saved: outputs/figures/F14_1_1_disposition.pdf\n")
