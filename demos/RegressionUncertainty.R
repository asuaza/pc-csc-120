library(tidyverse)
library(openintro)
set.seed(1)

data(hfi)
hfi_2016 <- hfi %>%
  filter(year == 2016)

ggplot(hfi_2016, aes(x = ef_score, y = pf_score)) +
  geom_point()

lmod <- lm(pf_score ~ ef_score, data = hfi_2016)
summary(lmod)

# Write model equation. Uncertainty in estimates vs. uncertainty in prediction.

ggplot(hfi_2016, aes(x = ef_score, y = pf_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# What does uncertainty in estimates mean? Bootstrap:
boot_hfi <- hfi_2016 %>%
  slice_sample(n = nrow(hfi_2016), replace = TRUE)
View(boot_hfi)
boot_lmod <- lm(pf_score ~ ef_score, data = boot_hfi)
summary(boot_lmod)

boot_hfi <- hfi_2016 %>%
  slice_sample(n = nrow(hfi_2016), replace = TRUE)
ggplot(boot_hfi, aes(x = ef_score, y = pf_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  lims(x = range(hfi_2016$ef_score), y = range(hfi_2016$pf_score))

ntrials <- 1000
boot_slopes <- numeric(ntrials)
for (i in 1:ntrials) {
  boot_hfi <- hfi_2016 %>%
    slice_sample(n = nrow(hfi_2016), replace = TRUE)
  boot_lmod <- lm(pf_score ~ ef_score, data = boot_hfi)
  boot_slopes[i] = coef(boot_lmod)[2]
}
hist(boot_slopes)

quantile(boot_slopes, probs = c(.05, .95))

# "Exact" confidence interval:
confint(lmod, parm = "ef_score", level = .9)

# Where do the "standard errors" in the summary come from?
summary(lmod)
1.0549 + .1036 * qnorm(c(.05, .95)) # almost normally distributed...
1.0549 + .1036 * qt(c(.05, .95), df = 160) # actually t distributed

# Can visualize using se = TRUE (which is default):
ggplot(hfi_2016, aes(x = ef_score, y = pf_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.9)

# Difference between uncertainty of estimates and uncertainty of predictions (use plot).

# Can use uncertainty to talk about whether a relationship is "significant." 
ggplot(hfi_2016, aes(x = pf_ss_homicide, y = pf_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Slope is positive; is this a "real" relationship?
ntrials <- 1000
boot_slopes <- numeric(ntrials)
for (i in 1:ntrials) {
  boot_hfi <- hfi_2016 %>%
    slice_sample(n = nrow(hfi_2016), replace = TRUE)
  boot_lmod <- lm(pf_score ~ pf_ss_homicide, data = boot_hfi)
  boot_slopes[i] = coef(boot_lmod)[2]
}
hist(boot_slopes)
quantile(boot_slopes, probs = c(.05, .95))

lmod <- lm(pf_score ~ pf_ss_homicide, data = hfi_2016)
confint(lmod, parm = "pf_ss_homicide", level = .9)

summary(lmod) 
# Significance level related to alpha in hypothesis testing;
#   here the null hypothesis is that the slope is zero.
# Confidence interval interpretation: is zero a plausible value for the "real" slope?

ggplot(hfi_2016, aes(x = pf_ss_homicide, y = pf_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.9)

# Redo with ef_regulation_labor_hours:
ggplot(hfi_2016, aes(x = ef_regulation_labor_hours, y = pf_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.9)

ntrials <- 1000
boot_slopes <- numeric(ntrials)
for (i in 1:ntrials) {
  boot_hfi <- hfi_2016 %>%
    slice_sample(n = nrow(hfi_2016), replace = TRUE)
  boot_lmod <- lm(pf_score ~ ef_regulation_labor_hours, data = boot_hfi)
  boot_slopes[i] = coef(boot_lmod)[2]
}
hist(boot_slopes)

quantile(boot_slopes, c(.05, .95))
lmod <- lm(pf_score ~ ef_regulation_labor_hours, data = hfi_2016)
confint(lmod, parm = "ef_regulation_labor_hours", level = 0.9)

quantile(boot_slopes, c(.1, .9))
confint(lmod, parm = "ef_regulation_labor_hours", level = 0.8)

# Difference between bootstrap intervals and "exact" intervals. Assumption of
#   normal residuals.
lmod_augmented <- broom::augment(lmod)
ggplot(data = lmod_augmented, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals")
ggplot(lmod_augmented, aes(x = .resid)) +
  geom_histogram(binwidth = 0.25) +
  xlab("Residuals")
