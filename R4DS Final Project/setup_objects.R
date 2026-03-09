library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(tidyverse)
library(ggplot2)
library(lubridate)

injury_data <- read.csv("data/skripnikov_injured_list.csv")
injury_data <- injury_data %>%
  mutate(addDate = as.Date(addDate, format = "%m/%d/%Y")) %>%
  mutate(endDate = as.Date(endDate, format = "%m/%d/%Y")) %>%
  mutate(borndate = as.Date(borndate, format = "%m/%d/%Y"))

head(injury_data)

injury_data_MLB_pos_players <- injury_data %>%
  filter(level == "MLB") %>%
  filter(!posit %in% c("P", "RP", "SP", "p", "RP-SP", "SP-RP", "P "))

injury_data_MLB_pos_players <- injury_data_MLB_pos_players %>%
  mutate(missed_time = as.numeric(endDate - addDate))

injury_data_MLB_pos_players_plot <- injury_data_MLB_pos_players %>%
  filter(endDate != "9999-12-31")

severe_terms <- c(
  "torn", "tear", "acl", "mcl", "pcl", "ulnar", "tommy john",
  "labrum", "fracture", "broken", "rupture", "surgery", 
  "dislocation", "out for season", "season-ending", "dislocated", "subluxation", "turf toe", "plantar fasciitis", "separation", "separated", "herniated disk", "surger", "bone spur", "kidney stone", "fratured", "fractured", "cracked", "herniated discs", "protrusion", "spur", "collapsed lung", "severe", "hematoma", "traumatic", "crack", "clot", "blood clot", "cancer", "hernia", "irregular heartbeat", "appendectomy", "appendicitis", "joint dysfunction", "brokem", "substance", "abuse", "tumor", "spinal fusion", "cartilage damage", "frecture", "bone chips"
)
moderate_terms <- c(
  "strain", "hamstring", "groin", "oblique", "sprain",
  "concussion", "inflammation", "tightness", "tendinitis",
  "spasms", "displaced", "infection", "swelling", "impingement", "laceration", "disorder", "hyperextended", "bulging", "tennis elbow", "cyst", "scratched cornea", "disc", "inflammation", "bursitis", "cellulitis", "lacerated", "tendonitis", "swollen", "hyperextension", "inflamed", "hyper-extended", "infected", "spasm"
  
)
minor_terms <- c(
  "sore", "soreness", "fatigue", "bruised", "contusion",
  "day-to-day", "illness", "flu", "virus", "rest", "issues", "discomfort", "stiffness", "jammed", "irritation", "bruise", "pinched nerve", "twist", "stress reaction", "pain", "vertigo", "migraines", "tenderness", "bunion", "COVID-19", "covid", "pulled", "lesion", "pinched", "pull", "stiff", "aggravated", "shin splints", "stress"
)

#had to look up the regex collapse logic in order to search any across str_detect in my vector
classify_severity <- function(note) {
  
  if (is.na(note)) return("unknown")
  
  if (str_detect(note, regex(str_c(severe_terms, collapse="|"), ignore_case = TRUE))) {
    return("severe")
  }
  
  if (str_detect(note, regex(str_c(moderate_terms, collapse="|"), ignore_case = TRUE))) {
    return("moderate")
  }
  
  if (str_detect(note, regex(str_c(minor_terms, collapse="|"), ignore_case = TRUE))) {
    return("minor")
  }
  
  return("unknown")
}

#had to look up why I couldn't run without vectorizing the output:
classify_severity_vec <- Vectorize(classify_severity)

injury_classified <- injury_data_MLB_pos_players %>%
  mutate(
    injury_severity = classify_severity_vec(notes)
  )

survival_data_model2 <- readRDS("data/survival_data_model2.rds")

injury_merge <- injury_classified %>%
  mutate(
    firstname = str_to_lower(firstname),
    lastname  = str_to_lower(lastname), 
    injury_year = year(addDate),
    bornyear = year(borndate)) %>%
  select(firstname, lastname, injury_year, bornyear, status, notes, addDate, X40man, endDate, minorteam, level, injury_severity)

modeling_data <- survival_data_model2 %>%
  mutate(nameFirst = str_to_lower(nameFirst), 
         nameLast = str_to_lower(nameLast))

model_injury <- modeling_data %>%
  left_join(injury_merge, by = c("nameFirst" = "firstname", "nameLast" = "lastname", "yearID" = "injury_year", "birthYear" = "bornyear"))

model_plot <- model_injury %>%
  filter(!is.na(injury_severity))
