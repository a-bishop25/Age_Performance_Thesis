### (THESIS Matching inconsistencies, chat prompted here)


#do still have to account for mismatched data
head(model_injury)

#unmatch checking

library(dplyr)

unmatched_injuries <- injury_merge %>%
  anti_join(
    modeling_data,
    by = c(
      "firstname" = "nameFirst",
      "lastname"  = "nameLast",
      "injury_year" = "yearID",
      "bornyear" = "birthYear"
    )
  )

# How many didn't match?
nrow(unmatched_injuries)

# View first few unmatched rows
head(unmatched_injuries)


#FIXING MISMATCHINGS WITH 0 IMPUTATIONS
# Create complete player-year panel
player_year_panel <- survival_data_model2 %>%
  group_by(playerID) %>%
  summarize(
    min_year = min(yearID),
    max_year = max(yearID)
  ) %>%
  rowwise() %>%
  mutate(yearID = list(seq(min_year, max_year))) %>%
  unnest(yearID) %>%
  select(playerID, yearID)

lahman_expanded <- player_year_panel %>%
  left_join(survival_data_model2,
            by = c("playerID", "yearID"))

counting_stats <- c(
  "G","AB","R","H","X1B","X2B","X3B","HR","RBI","SB","CS",
  "BB","SO","HBP","SH","SF","GIDP","PA"
)

lahman_expanded <- lahman_expanded %>%
  mutate(across(all_of(counting_stats), ~ ifelse(is.na(.), 0, .)))
injury_merge_clean <- injury_classified %>%
  mutate(
    firstname = str_to_lower(firstname),
    lastname  = str_to_lower(lastname), 
    injury_year = year(addDate),
    bornyear = year(borndate)
  )

model_injury_expanded <- lahman_expanded %>%
  left_join(injury_merge_clean,
            by = c("nameFirst" = "firstname",
                   "nameLast" = "lastname",
                   "birthYear" = "bornyear",
                   "yearID" = "injury_year"))


