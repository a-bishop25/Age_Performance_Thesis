# FIXING INTERACTION TERMS WITH MISSED PREVIOUS YEAR

view(sdm2_no_pitch)

model_w_missed <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) + 
    lag_wOBA * missed_prev_year +
    lag_PA * missed_prev_year,
  data = sdm2_no_pitch
)
summary(model_w_missed)
# SCALING PREDICTORS

sdm2_scaled <- sdm2_no_pitch %>%
  mutate(across(
    c(age, lag_PA, lag_G, lag_wOBA, career_PA),
    scale
  ))

fit_scaled <- coxph(
  Surv(start, stop, event) ~ 
    age + lag_PA  + lag_wOBA + career_PA,
  data = sdm2_scaled
)

summary(fit_scaled)


# CHECKING CORRELATION

predictors <- sdm2_no_pitch %>%
  select(age, lag_PA, lag_G, lag_wOBA, career_PA)

cor_matrix <- cor(predictors, use = "complete.obs")
cor_matrix

vif_model <- coxph(
  Surv(start, stop, event) ~ 
    age + lag_PA + lag_G + lag_wOBA + career_PA,
  data = sdm2_no_pitch
)
vif(vif_model)
