# INJURY INCORPORATION 

#COLLAPSE INJURIES TO ONE ROW PER YEAR

injury_num <- injury_classified %>%
  mutate(
    injury_severity = case_when(
      injury_severity == "minor" ~ 1,
      injury_severity == "moderate" ~ 2,
      injury_severity == "severe" ~ 3,
      TRUE ~ NA_real_
    )
  )


injury_summary <- injury_num %>%
  mutate(
    firstname = str_to_lower(firstname),
    lastname  = str_to_lower(lastname),
    injury_year = year(addDate),
    bornyear = year(borndate)
  ) %>%
  group_by(firstname, lastname, bornyear, injury_year) %>%
  summarise(
    total_injuries = n(),
    avg_severity = if (all(is.na(injury_severity))) NA_real_
    else mean(injury_severity, na.rm = TRUE),
    max_severity = if (all(is.na(injury_severity))) NA_real_
    else max(injury_severity, na.rm = TRUE),
    .groups = "drop"
  )

# Clean to join, sets new dataset for simplicity
sdm2_clean <- sdm2_no_pitch %>%
  mutate(
    nameFirst = str_to_lower(nameFirst),
    nameLast  = str_to_lower(nameLast)
  )
head(sdm2_clean)

# Join to sdm2_no_pitch
sdm2_injury <- sdm2_clean %>%
  left_join(
    injury_summary,
    by = c(
      "nameFirst" = "firstname",
      "nameLast"  = "lastname",
      "birthYear" = "bornyear",
      "yearID"    = "injury_year"
    )
  )
view(sdm2_injury)


# NONLINEAR LAGGED VARIABLE MODEL
model_spline_main <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    ns(lag_wOBA, df = 3) +
    ns(lag_PA, df = 3) +
    missed_prev_year,
  data = sdm2_no_pitch
)
summary(model_spline_main)


model_spline_interact <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3),
  data = sdm2_no_pitch
)
summary(model_spline_interact)
dfbeta(model_spline_interact)
boxplot(sdm2_no_pitch$lag_wOBA)
BIC(model_w_missed, model_spline_main, model_spline_interact)


# NEW FOR MEETING

model_injury_main <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3) +
    total_injuries + avg_severity + max_severity,
  data = sdm2_injury
)
summary(model_injury_main)
# CAN I COMPARE TO ORIGINAL BECAUSE OF THE SLIGHTLY DIFFERING DATASETS, THINK I NEED TO DOUBLE CHECK DIFF


# In meeting
library(survival)
library(splines)
library(ggplot2)
library(dplyr)

# ---- Function to create hazard predictions with confidence intervals ----
hazard_predictions <- function(model, var_name, var_seq, fixed_values, df = 3) {
  
  pred_grid <- expand.grid(
    age = fixed_values$age,
    lag_wOBA = if(var_name == "lag_wOBA") var_seq else fixed_values$lag_wOBA,
    lag_PA  = if(var_name == "lag_PA") var_seq else fixed_values$lag_PA,
    missed_prev_year = c(0,1)
  )
  
  # Predict linear predictor + SE
  pred_lp <- predict(model, newdata = pred_grid, type = "lp", se.fit = TRUE)
  
  pred_grid <- pred_grid %>%
    mutate(
      eta = pred_lp$fit,
      se_eta = pred_lp$se.fit,
      # 95% CI on linear predictor
      eta_upper = eta + 1.96 * se_eta,
      eta_lower = eta - 1.96 * se_eta,
      # Convert to hazard
      hazard = exp(eta),
      hazard_upper = exp(eta_upper),
      hazard_lower = exp(eta_lower)
    )
  
  return(pred_grid)
}

median_age <- median(sdm2_no_pitch$age, na.rm = TRUE)
median_PA  <- median(sdm2_no_pitch$lag_PA, na.rm = TRUE)
median_wOBA <- median(sdm2_no_pitch$lag_wOBA, na.rm = TRUE)

# ---- lag_wOBA effect ----
lag_wOBA_seq <- seq(min(sdm2_no_pitch$lag_wOBA, na.rm = TRUE),
                    1.5, length.out = 50)

pred_wOBA <- hazard_predictions(
  model_spline_interact,
  var_name = "lag_wOBA",
  var_seq = lag_wOBA_seq,
  fixed_values = list(age = median_age, lag_PA = median_PA, lag_wOBA = median_wOBA)
)

ggplot(pred_wOBA, aes(x = lag_wOBA, y = hazard, color = factor(missed_prev_year))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = hazard_lower, ymax = hazard_upper, fill = factor(missed_prev_year)),
              alpha = 0.2, color = NA) +
  labs(
    x = "Lag wOBA",
    y = "Predicted Hazard Rate",
    color = "Missed Previous Year",
    fill = "Missed Previous Year",
    title = "Effect of lag_wOBA on Hazard Rate with 95% CI"
  ) +
  theme_minimal()


# ---- lag_PA effect ----
lag_PA_seq <- seq(min(sdm2_no_pitch$lag_PA, na.rm = TRUE),
                  max(sdm2_no_pitch$lag_PA, na.rm = TRUE), length.out = 50)

pred_PA <- hazard_predictions(
  model_spline_interact,
  var_name = "lag_PA",
  var_seq = lag_PA_seq,
  fixed_values = list(age = median_age, lag_PA = median_PA, lag_wOBA = median_wOBA)
)

ggplot(pred_PA, aes(x = lag_PA, y = hazard, color = factor(missed_prev_year))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = hazard_lower, ymax = hazard_upper, fill = factor(missed_prev_year)),
              alpha = 0.2, color = NA) +
  labs(
    x = "Lag PA",
    y = "Predicted Hazard Rate",
    color = "Missed Previous Year",
    fill = "Missed Previous Year",
    title = "Effect of lag_PA on Hazard Rate with 95% CI"
  ) +
  theme_minimal()



# 10 Fold CV
library(survival)
library(dplyr)
library(rsample)
library(purrr)

# --------------------------
# 10-Fold Cross Validation
# --------------------------

# Function to compute C-index for a given training/testing split
cox_cv <- function(model_formula, data, folds = 10, seed = 42) {
  
  set.seed(seed)
  
  # Create folds
  cv_folds <- vfold_cv(data, v = folds, strata = "event") # stratify by event for balance
  
  # Run CV
  results <- map_dbl(cv_folds$splits, function(split) {
    train_data <- analysis(split)
    test_data  <- assessment(split)
    
    fit <- coxph(model_formula, data = train_data)
    
    # Predict linear predictor on test set
    lp <- predict(fit, newdata = test_data, type = "lp")
    
    # Concordance index
    surv_obj <- Surv(test_data$start, test_data$stop, test_data$event)
    concord <- survConcordance(surv_obj ~ lp)$concordance
    return(concord)
  })
  
  return(mean(results))  # average C-index across folds
}

# --------------------------
# Model Formulas
# --------------------------
formula_main <- Surv(start, stop, event) ~ 
  ns(age, df = 3) +
  ns(lag_wOBA, df = 3) +
  ns(lag_PA, df = 3) +
  missed_prev_year

formula_interact <- Surv(start, stop, event) ~ 
  ns(age, df = 3) +
  missed_prev_year * ns(lag_wOBA, df = 3) +
  missed_prev_year * ns(lag_PA, df = 3)

formula_linear <- Surv(start, stop, event) ~ 
  ns(age, df = 3) + 
  lag_wOBA * missed_prev_year +
  lag_PA * missed_prev_year

# --------------------------
# Run CV for each model
# --------------------------
cindex_main      <- cox_cv(formula_main, sdm2_no_pitch)
cindex_interact  <- cox_cv(formula_interact, sdm2_no_pitch)
cindex_linear    <- cox_cv(formula_linear, sdm2_no_pitch)

# --------------------------
# Compare C-index
# --------------------------
tibble(
  Model = c("Spline Main", "Spline Interact", "Linear Interact"),
  CV_Cindex = c(cindex_main, cindex_interact, cindex_linear)
)

