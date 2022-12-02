library(openintro)
data(evals)

# Remove unwanted variables.

evals <- evals %>%
  select(
    score:cls_perc_eval,
    cls_students:cls_credits,
    bty_avg:pic_color
  )

# Choose a model using stepwise selection.

lmod_full <- lm(score ~ ., data = evals)
summary(lmod_full)

lmod_bsel <- step(lmod_full)
summary(lmod_bsel)

lmod_intercept <- lm(score ~ 1, data = evals)
summary(lmod_intercept)

lmod_fsel <- step(
  lmod_intercept, scope = formula(lmod_full), direction = "forward"
)
formula(lmod_fsel)
formula(lmod_bsel)

# If forward and backward selection give different models, compare AIC (or
#   adjusted R2).

# Interpret the coefficients!
summary(lmod_bsel)

# (Side note) Don't just look at significance stars! Example:

summary(lmod_bsel)
lmod_bsel_nooutfit <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval +
                           cls_credits + bty_avg + pic_color, data = evals)
summary(lmod_bsel_nooutfit)

# Diagnostics.

bsel_df <- augment(lmod_bsel)

# Normal residuals:
ggplot(bsel_df, aes(x = .resid)) +
  geom_histogram(bins = 20) # Some left skew.

# Linearity:
ggplot(bsel_df, aes(x = .fitted, y = .resid)) +
  geom_point()

# Constant variability:
ggplot(bsel_df, aes(x = bty_avg, y = abs(.resid))) +
  geom_point() +
  geom_smooth(method = "lm")

# Shortcut:
plot(lmod_bsel)
