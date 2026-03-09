#order: 1)Fix NA values in wOBA calculations 2) Pre-filter handle outlier values 3) performance metric comparison 4) injury characteristic interaction 5) k fold CV

#Fixing NA values in wOBA calculations
# First removing PA == 1 and then threshold and seeing difference
sdm2_no_zero <- sdm2_no_pitch %>%
  filter(AB > 0)

sdm2_twenty <- sdm2_no_pitch %>%
  filter(AB > 20)

model_spline_interact <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3),
  data = sdm2_no_pitch
)
summary(model_spline_interact)

msi_data <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3),
  data = sdm2_no_zero
)
summary(msi_data)

msi_twenty <- msi_data <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3),
  data = sdm2_twenty
)
summary(msi_twenty)

# C-Index function to compare datasets fit
get_cindex <- function(model, data) {
  
  lp <- predict(model, newdata = data, type = "lp")
  surv_obj <- with(data, Surv(start, stop, event))
  
  conc <- concordance(surv_obj ~ lp)
  
  # If C < 0.5, flip it
  c_val <- conc$concordance
  if (c_val < 0.5) c_val <- 1 - c_val
  
  return(c_val)
}

# ---- Compute C-index for each model ----

c_full <- get_cindex(model_spline_interact, sdm2_no_pitch)
c_no_zero <- get_cindex(msi_data, sdm2_no_zero)
c_twenty <- get_cindex(msi_twenty, sdm2_twenty)

# ---- Combine results ----

cindex_results <- data.frame(
  Dataset = c("Full Data", "AB > 0", "AB > 20"),
  C_index = c(c_full, c_no_zero, c_twenty)
)

print(cindex_results)


# Results tell me that keeping the full data with the 0 AB and PA tell me about career survival more than it hurts in terms of performance metrics, so I should most likely keep those observations
# Attempt at a weighted model, dont think it works for this context but its here
sdm2_weighted <- sdm2_no_pitch %>%
  filter(PA >0) %>%
  mutate(PA_weight = pmin(PA, 150))  # cap weight at 150

model_weighted_capped <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3),
  data = sdm2_weighted,
  weights = PA_weight
)
summary(model_weighted_capped)
get_cindex(model_weighted_capped, sdm2_weighted)
# Didn't do much to the c index, basically same as when we removed 0 abs, which we did. Probably smarter to leave model full

# PERFORMANCE METRIC CHECKING
sdm2_stats <- sdm2_no_pitch %>%
  mutate(
    # Batting average
    AVG  = ifelse(AB > 0, H / AB, NA),
    
    # On-base percentage
    OBP  = ifelse(AB + BB + HBP + SF > 0,
                  (H + BB + HBP) / (AB + BB + HBP + SF), NA),
    
    # Singles
    X1B = H - X2B - X3B - HR,
    
    # Slugging
    SLG  = ifelse(AB > 0,
                  (X1B + 2*X2B + 3*X3B + 4*HR) / AB, NA),
    
    # Isolated power
    ISO = SLG - AVG,
    
    # Simplified wOBA (if you want to keep)
    wOBA = ifelse(AB + BB - IBB + SF + HBP > 0,
                  (0.69*(BB-IBB) + 0.72*HBP + 0.89*X1B + 1.27*X2B + 1.62*X3B + 2.10*HR) /
                    (AB + BB - IBB + SF + HBP), NA)
  )

sdm2_scaled_stats <- sdm2_stats %>%
  mutate(across(c(age, lag_PA, lag_G, lag_wOBA, AVG, OBP, SLG, ISO), scale))


metrics <- c("lag_PA", "lag_G", "lag_wOBA", "AVG", "OBP", "SLG", "ISO")
results <- data.frame(metric = metrics, coef = NA, pval = NA, stringsAsFactors = FALSE)

for (m in metrics) {
  f <- as.formula(paste0("Surv(start, stop, event) ~ ns(age,3) + ", m))
  fit <- coxph(f, data = sdm2_scaled_stats)
  summ <- summary(fit)
  results$coef[results$metric == m] <- summ$coef[2, "coef"]  # scaled effect size
  results$pval[results$metric == m] <- summ$coef[2, "Pr(>|z|)"]
}

results <- results %>% arrange(desc(abs(coef)))
results
# Confirmed the three metrics we started with when building the models are the three most significant, those being wOBA, lag_PA, and lag_G

# Injury incorporation into model
# 0's for non injuries
sdm2_injury_zeroes <- sdm2_injury %>%
  mutate(
    total_injuries = ifelse(is.na(total_injuries), 0, total_injuries),
    avg_severity   = ifelse(is.na(avg_severity), 0, avg_severity),
    max_severity   = ifelse(is.na(max_severity), 0, max_severity)
  )

# lag injury stats to match our lagged stats
sdm2_lag_injury <- sdm2_injury_zeroes%>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    lag_total_injuries = lag(total_injuries),
    lag_avg_severity   = lag(avg_severity),
    lag_max_severity   = lag(max_severity)
  ) %>%
  ungroup()

# model with no missed_prev_year
msi_no_missed <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, 3) +
    ns(lag_wOBA, 3) +
    ns(lag_PA, 3) +
    lag_total_injuries,
  data = sdm2_lag_injury
)
summary(msi_no_missed)

# model w missed and injury
msi_with_injury <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) +
    missed_prev_year * ns(lag_wOBA, df = 3) +
    missed_prev_year * ns(lag_PA, df = 3) +
    lag_total_injuries,
  data = sdm2_lag_injury
)
summary(msi_with_injury)

# lag of total injuries not stat sig

# need to try and incorporate injury severity, or possibly that cumulative running total per career

