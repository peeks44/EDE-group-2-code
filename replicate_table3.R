# Replicates Table 3 (Dinkelman 2011, AER) - "Assignment to Eskom Project: First Stage OLS Estimates"
# Source data: matched_censusdata.dta (community-level panel, Census 1996/2001)

library(haven)
library(fixest)

d <- read_dta("matched_censusdata.dta")

# Table 3's sample (n = 1,816) excludes the 4 large urban areas.
# largeareas is coded 1 for the 1,816 non-city communities and missing for the 176 city communities.
d <- d[!is.na(d$largeareas), ]
stopifnot(nrow(d) == 1816)

# The paper reports several coefficients scaled to a 10-unit change for readability
# ("Gradient x 10", "Kilometers to grid x 10", "Household density x 10").
d$grad10    <- d$mean_grad_new  / 10
d$kmsubs10  <- d$kms_to_subs0   / 10
d$hhdens10  <- d$baseline_hhdens0 / 10
d$kmsroad10 <- d$kms_to_road0   / 10
d$kmstown10 <- d$kms_to_town0   / 10

controls <- c("base_hhpovrate0", "prop_head_f_a0", "sexratio0_a", "prop_indianwhite0",
              "kmsroad10", "kmstown10", "prop_matric_m0", "prop_matric_f0")

rhs1 <- "grad10"
rhs2 <- paste("grad10", "kmsubs10", "hhdens10", paste(controls, collapse = " + "), sep = " + ")
rhs4 <- paste(rhs2, "d_prop_waterclose", "d_prop_flush", sep = " + ")

m1 <- feols(as.formula(paste("T ~", rhs1)),            data = d, vcov = ~placecode0)
m2 <- feols(as.formula(paste("T ~", rhs2)),             data = d, vcov = ~placecode0)
m3 <- feols(as.formula(paste("T ~", rhs2, "| dccode0")), data = d, vcov = ~placecode0)
m4 <- feols(as.formula(paste("T ~", rhs4, "| dccode0")), data = d, vcov = ~placecode0)

models <- list("(1)" = m1, "(2)" = m2, "(3)" = m3, "(4)" = m4)

dict <- c(grad10 = "Gradient x 10", kmsubs10 = "Kilometers to grid x 10",
          hhdens10 = "Household density x 10", base_hhpovrate0 = "Poverty rate",
          prop_head_f_a0 = "Female-headed HHs", sexratio0_a = "Adult sex ratio",
          prop_indianwhite0 = "Indian, white adults x 10", kmsroad10 = "Kilometers to road x 10",
          kmstown10 = "Kilometers to town x 10", prop_matric_m0 = "Men with high school",
          prop_matric_f0 = "Women with high school", d_prop_waterclose = "Delta water access",
          d_prop_flush = "Delta toilet access", T = "Eskom project = [1 or 0]")

print(etable(models, dict = dict, cluster = ~placecode0,
             order = c("Gradient", "Kilometers to grid", "Household density", "Poverty rate",
                       "Female-headed", "Adult sex ratio", "Indian", "Kilometers to road",
                       "Kilometers to town", "Men with", "Women with", "Delta water", "Delta toilet"),
             fitstat = c("n", "r2"), digits = 3, digits.stats = 2))

cat("\nF-statistic on gradient (Wald test, H0: coefficient on Gradient x 10 = 0):\n")
for (nm in names(models)) {
  w <- wald(models[[nm]], "grad10", print = FALSE)
  cat(sprintf("  Column %s: F = %.2f, Pr > F = %.2f\n", nm, w$stat, w$p))
}

cat("\nMean of outcome variable (Eskom project):", round(mean(d$T), 2), "\n")
cat("District fixed effects included in columns 3-4 (10 districts):",
    length(unique(d$dccode0)), "\n")
