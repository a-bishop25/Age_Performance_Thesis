### Fixing the player filtering to try and isolate specifically the position players in the lahman player batting database


## Initial dataset built for modeling from Lahman
Batting <- Lahman::Batting
People <- Lahman::People
# ================================
# 0. ENHANCE RAW BATTING DATA — KEEP ALL ROWS
# ================================
# --- Join People info and compute age for each season ---
Batting <- Batting %>%
  left_join(
    People %>%
      dplyr::select(playerID, birthYear, birthMonth, birthDay, nameFirst, nameLast, bats, throws),
    by = "playerID"
  ) %>%
  mutate(
    # Estimate player age for that season
    # (approximate, since we don’t have exact game date — assumes mid-season)
    age = yearID - birthYear - ifelse(birthMonth > 7, 1, 0)
  )
# ---- Compute extra metrics ----
Batting <- Batting %>%
  mutate(
    # Plate Appearances
    PA = AB + BB + HBP + SF + SH,
    
    # Singles (1B)
    X1B = H - X2B - X3B - HR,
    
    # Weighted On-Base Average (wOBA)
    wOBA = (
      (0.69 * (BB - ifelse(is.na(IBB), 0, IBB))) +
        (0.72 * HBP) +
        (0.89 * X1B) +
        (1.27 * X2B) +
        (1.62 * X3B) +
        (2.10 * HR)
    ) / (AB + BB - ifelse(is.na(IBB), 0, IBB) + SF + HBP)
  ) %>%
  mutate(
    wOBA = ifelse(is.nan(wOBA) | is.infinite(wOBA), NA, wOBA)
  )

# ---- Build survival model structure ----
survival_data_model2 <- Batting %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    years_in_league = row_number() - 1,  # start counting at 0
    start = years_in_league,
    stop  = years_in_league + 1,
    
    # Lag and cumulative stats
    lag_wOBA = lag(wOBA, 1),
    lag_PA   = lag(PA, 1),
    lag_G    = lag(G, 1),
    
    career_PA = cumsum(lag(PA, default = 0)),
    career_G  = cumsum(lag(G, default = 0)),
    
    # Flag final available year
    event = if_else(row_number() == n(), 1, 0),
    
    # Flag gap years
    missed_prev_year = if_else(yearID - lag(yearID, default = first(yearID)) > 1, 1, 0)
  ) %>%
  ungroup()
view(survival_data_model2)


## Creating pitcher reference data 
pitchers_data <- Lahman::Pitching
view(pitchers_data)

## Pitching summary table
library(dplyr)

pitcher_summary <- Lahman::Pitching %>%
  group_by(playerID) %>%
  summarise(
    pitch_seasons = n_distinct(yearID),
    career_IPouts = sum(IPouts, na.rm = TRUE),
    career_IP     = career_IPouts / 3,
    career_Gp     = sum(G, na.rm = TRUE),
    max_season_IP = max(IPouts / 3, na.rm = TRUE),
    .groups = "drop"
  )

head(pitcher_summary)

## Pitcher exclusion threshold 
pitcher_exclusion <- pitcher_summary %>%
  mutate(
    is_pitcher = career_IP >= 100 |
      career_Gp >= 50 |
      max_season_IP >= 30
  ) %>%
  filter(is_pitcher) %>%
  select(playerID)

head(pitcher_exclusion)

## Anti_join pitcher exclusion from modeling dataset
sdm2_no_pitch <- survival_data_model2 %>%
  anti_join(pitcher_exclusion, by = "playerID")

view(sdm2_no_pitch)

## Sanity Check
n_distinct(survival_data_model2$playerID) -
  n_distinct(survival_data_model2_clean$playerID)


### Updated effects of Predictors 

# ================================
# Cox model + nonlinear effect plots for all predictors
# ================================

# --- 1. Fit Cox model (UNCHANGED)
fit_cox <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) + 
    lag_wOBA + lag_PA + lag_G + career_PA,
  data = scaled_predictors
)

# --- 2. Store predictor names
predictors <- c("age", "lag_wOBA", "lag_PA", "lag_G", "career_PA")

# --- 3. Median values for holding others constant
median_vals <- sdm2_no_pitch %>%
  summarise(
    age = median(age, na.rm = TRUE),
    lag_wOBA = median(lag_wOBA, na.rm = TRUE),
    lag_PA = median(lag_PA, na.rm = TRUE),
    lag_G = median(lag_G, na.rm = TRUE),
    career_PA = median(career_PA, na.rm = TRUE)
  )

# --- 4. Function to generate effect plot for a predictor
plot_effect <- function(var_name) {
  
  # Sequence for focal predictor
  grid_vals <- seq(
    min(sdm2_no_pitch[[var_name]], na.rm = TRUE),
    max(sdm2_no_pitch[[var_name]], na.rm = TRUE),
    length.out = 100
  )
  
  # Construct newdata holding others at median
  newdata <- median_vals[rep(1, 100), ]
  newdata[[var_name]] <- grid_vals
  
  # Predict
  linear_pred <- predict(
    fit_cox,
    newdata = newdata,
    type = "lp",
    se.fit = TRUE
  )
  
  plot_data <- data.frame(
    x = grid_vals,
    HR = exp(linear_pred$fit),
    HR_upper = exp(linear_pred$fit + 1.96 * linear_pred$se.fit),
    HR_lower = exp(linear_pred$fit - 1.96 * linear_pred$se.fit)
  )
  
  # Plot
  ggplot(plot_data, aes(x = x, y = HR)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_ribbon(
      aes(ymin = HR_lower, ymax = HR_upper),
      alpha = 0.2,
      fill = "darkgreen"
    ) +
    labs(
      x = var_name,
      y = "Hazard Ratio",
      title = paste("Effect of", var_name, "on Hazard of Retirement")
    ) +
    theme_minimal()
}

# --- 5. Generate plots for all predictors
plots <- lapply(predictors, plot_effect)

# Print plots
for (p in plots) print(p)



#IN CLASS



sdm2_naomit <- sdm2_no_pitch %>%
  dplyr::select(age, lag_wOBA, lag_PA, career_PA, start, stop, event, lag_G) %>%
  na.omit()

index_vector <- sample(1:nrow(sdm2_naomit),1000)

fit_cox3 <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) + 
    lag_wOBA + lag_PA + lag_G + ns(career_PA, df = 3),
  data = sdm2_naomit, subset = index_vector
)

fit_bic <- stepAIC(fit_cox3, k = log(length(index_vector)))


fit_cox1 <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) + 
    lag_wOBA + lag_PA + lag_G + ns(career_PA, df = 3),
  data = sdm2_naomit, subset = 1:1000,
)

fit_bic <- stepAIC(fit_cox1, k = log(nrow(sdm2_naomit)))
nrow(sdm2_naomit)
samp_player <- sdm2_naomit[1:1000,]

fit_cox2 <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) + 
    lag_wOBA + lag_PA + lag_G + ns(career_PA, df = 3),
  data = samp_player
)
fit_bic <- stepAIC(fit_cox2, k = log(1000))
