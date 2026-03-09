### Fixing the player year stats to combine stats from different teams for same players same years


## Initial dataset built for modeling from Lahman
library(dplyr)
library(Lahman)

# ================================
# 1. LOAD RAW DATA
# ================================
Batting <- Lahman::Batting
People  <- Lahman::People

# ================================
# 2. JOIN DEMOGRAPHICS + COMPUTE AGE
# ================================
Batting <- Batting %>%
  left_join(
    People %>%
      dplyr::select(
        playerID, birthYear, birthMonth, birthDay,
        nameFirst, nameLast, bats, throws
      ),
    by = "playerID"
  ) %>%
  mutate(
    # Approximate mid-season age
    age = yearID - birthYear - ifelse(birthMonth > 7, 1, 0)
  )

# ================================
# 3. COMBINE SAME PLAYER–SAME YEAR (ACROSS TEAMS)
# ================================
Batting_combined <- Batting %>%
  group_by(playerID, yearID) %>%
  summarise(
    # Static info
    age       = first(age),
    nameFirst = first(nameFirst),
    nameLast  = first(nameLast),
    bats      = first(bats),
    throws    = first(throws),
    birthYear = first(birthYear),
    
    # Sum counting stats across teams
    G   = sum(G, na.rm = TRUE),
    AB  = sum(AB, na.rm = TRUE),
    H   = sum(H, na.rm = TRUE),
    X2B = sum(X2B, na.rm = TRUE),
    X3B = sum(X3B, na.rm = TRUE),
    HR  = sum(HR, na.rm = TRUE),
    BB  = sum(BB, na.rm = TRUE),
    HBP = sum(HBP, na.rm = TRUE),
    SF  = sum(SF, na.rm = TRUE),
    SH  = sum(SH, na.rm = TRUE),
    IBB = sum(IBB, na.rm = TRUE),
    
    .groups = "drop"
  )

# ================================
# 4. RECOMPUTE PA + wOBA AT SEASON LEVEL
# ================================
Batting_combined <- Batting_combined %>%
  mutate(
    # Plate appearances
    PA = AB + BB + HBP + SF + SH,
    
    # Singles
    X1B = H - X2B - X3B - HR,
    
    # Weighted On-Base Average (season-level)
    wOBA = (
      (0.69 * (BB - IBB)) +
        (0.72 * HBP) +
        (0.89 * X1B) +
        (1.27 * X2B) +
        (1.62 * X3B) +
        (2.10 * HR)
    ) / (AB + BB - IBB + SF + HBP)
  ) %>%
  mutate(
    wOBA = ifelse(is.nan(wOBA) | is.infinite(wOBA), NA, wOBA)
  )

# # ================================
# # 5. OPTIONAL: DROP ZERO-PA SEASONS
# # ================================
# Batting_combined <- Batting_combined %>%
#   filter(PA > 0)

# View result
View(Batting_combined)

#sanity check
Batting_combined %>%
  count(playerID, yearID) %>%
  filter(n > 1)


# ---- Build survival model structure ----
survival_data_model2 <- Batting_combined %>%
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



# Fitting new model without lag_G

fit_cox <- coxph(
  Surv(start, stop, event) ~ 
    ns(age, df = 3) + 
    lag_wOBA + lag_PA  + age*career_PA,
  data = sdm2_no_pitch
)

summary(fit_cox)
cox.zph(fit_cox)  # check proportional hazards



#Creating total career bins:
player_summary <- sdm2_no_pitch %>%
  group_by(playerID) %>%
  summarise(
    career_years = max(stop, na.rm = TRUE),
    has_gap_year = any(missed_prev_year == 1, na.rm = TRUE),
    final_event  = max(event),
    .groups = "drop"
  )

player_summary <- player_summary %>%
  mutate(
    career_group = case_when(
      career_years < 5  ~ "<5 years",
      career_years < 10 ~ "5–10 years",
      TRUE              ~ "10+ years"
    )
  )


#Selecting players:
set.seed(122)

players_by_length <- player_summary %>%
  filter(final_event == 1) %>%   # completed careers only
  group_by(career_group) %>%
  slice_sample(n = 10) %>%       # ~10 per group
  ungroup()

players_with_gaps <- player_summary %>%
  filter(has_gap_year) %>%
  slice_sample(n = 10)

well_known_players <- c(
  "troutmi01", "harpebr03", "judgeaa01", "bettsmo01",
  "alonspe01", "sotoyju01", "freemfr01", "goldspa01",
  "mookie01", "ohtansh01"
)

players_well_known <- player_summary %>%
  filter(playerID %in% well_known_players)


#Combine players for evalset

players_eval <- bind_rows(
  #players_by_length,
  players_with_gaps %>%
    filter(career_years >= 5)
  #players_well_known
) %>%
  distinct(playerID)
players_eval_vec <- players_eval$playerID



#Career Trajectories:
expected_survival_time <- function(surv_obj, max_quantile = 0.95) {
  times <- surv_obj$time
  surv  <- surv_obj$surv
  if(length(times) < 2) return(0)
  max_time <- quantile(times, probs = max_quantile, na.rm = TRUE)
  idx <- which(times <= max_time)
  if(length(idx) < 2) return(0)
  times_sub <- times[idx]
  surv_sub  <- surv[idx]
  dt <- diff(c(0, times_sub))
  auc <- sum(surv_sub * dt)
  return(auc)
}

# ================================
# 4. PREDICT FOR MULTIPLE PLAYERS
# ================================

player_id = "troutmi01"
for (player_id in players_eval_vec) {
  
  cat("\n\n====================\n")
  cat("Generating plots for:", player_id, "\n")
  cat("====================\n")
  
  player_data <- survival_data_model2 %>%
    filter(playerID == player_id) %>%
    arrange(start) %>%
    slice(-1)
  
  if (nrow(player_data) == 0) {
    cat("No data found for", player_id, "\n")
    next
  }
  
  expected_remaining <- sapply(1:nrow(player_data), function(i) {
    newrow <- player_data[i, , drop = FALSE]
    sf <- survfit(fit_cox, newdata = newrow)
    est <- expected_survival_time(sf)
    est
  })
  
  predicted_retirement_age <- player_data$age + expected_remaining
  
  results <- data.frame(
    season_index = 1:nrow(player_data),
    season = player_data$yearID,
    current_age = player_data$age,
    expected_remaining = expected_remaining,
    predicted_retirement_age = predicted_retirement_age
  )
  
  # ================================
  # 5. PLOTS (same as your code)
  # ================================
  
  # --- Predicted retirement age vs. current age ---
  plot(results$current_age, results$predicted_retirement_age,
       type = "b", pch = 19, col = "skyblue",
       xlab = "Current Age",
       ylab = "Predicted Retirement Age",
       main = paste("Predicted Retirement Age vs Current Age for", player_id))
  abline(0, 1, col = "red", lty = 2)
  text(results$current_age, results$predicted_retirement_age + 0.3,
       labels = round(results$expected_remaining, 1), cex = 0.7)
  
  # --- Expected years remaining (barplot) ---
  barplot(results$expected_remaining,
          names.arg = round(results$current_age, 0),
          las = 2,
          col = "skyblue",
          main = paste("Predicted Remaining Career Length by Season for", player_id),
          xlab = "Current Age",
          ylab = "Expected Seasons Remaining")
  abline(h = 0, col = "red", lty = 2)
}

