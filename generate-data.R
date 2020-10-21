library(tidyverse)

nc_voter <- read_rds("ncvoter_Statewide_small.rds")  # all districts, Graham doesnt think it's  all voters, one row for each voter 
head(nc_voter)
nc_voter <- nc_voter %>% 
  filter(voter_status_reason_desc != "DECEASED") %>% 
  mutate(voter_reg_num = as.integer(substr(voter_reg_num,6,12)))
#nc_voter$voter_reg_num <- substr(nc_voter$voter_reg_num,6,12)

nc_reg_data <- read_rds("ncvhis_Statewide_small.rds")


voter_data <- left_join(nc_voter,nc_reg_data %>% filter(voted_party_desc != ""),by=c("voter_reg_num", "county_id", "county_desc"))
#left_voter_reg <- right_join(nc_voter,nc_reg_data,by="voter_reg_num")

voter_data <- voter_data %>% 
  mutate(likely_voter = ifelse(is.na(election_lbl), 0, 1),
         age_bin = case_when(
           birth_age <= 29 ~ "18-29",
           birth_age >= 30 & birth_age <= 39 ~ "30-39",
           birth_age >= 40 & birth_age <= 49 ~ "40-49",
           birth_age >= 50 & birth_age <= 64 ~ "50-64",
           birth_age >= 65 ~ "65+"
         ),
         race_ethnicity = case_when(
           ethnic_code == "HL" ~ "Hispanic Any Race",
           ethnic_code == "NL" & race_code == "W" ~ "Non-Hispanic White",
           ethnic_code == "NL" & race_code == "B" ~ "Non-Hispanic Black",
           ethnic_code == "NL" & race_code == "O" ~ "Non-Hispanic Other",
           ethnic_code == "NL" & race_code == "A" ~ "Non-Hispanic Asian",
           ethnic_code == "NL" & race_code == "M" ~ "Non-Hispanic Mixed",
           ethnic_code == "NL" & race_code == "I" ~ "Non-Hispanic American Indian",
           ethnic_code == "NL" & race_code == "P" ~ "Non-Hispanic Pacific Islander",
           ethnic_code == "NL" & race_code == "U" ~ "Undetermined",
           TRUE ~ "Undetermined"
         ),
         voted_party_desc = case_when(
           voted_party_desc == "DEMOCRATIC" ~ "DEMOCRATIC",
           voted_party_desc == "REPUBLICAN" ~ "REPUBLICAN",
           voted_party_desc== "UNAFFILIATED" ~ "UNAFFILIATED",
           voted_party_desc %in% c("LIBERTARIAN", "GREEN", "CONSTITUTION") ~ "THIRD PARTY",
           TRUE ~ as.character(voted_party_desc)
         )
         
  )


grouped_voter_data <- voter_data %>%
  group_by(county_desc, race_ethnicity, #voted_party_desc, 
           age_bin, gender_code) %>%
  summarize(Likely = sum(likely_voter==1), 
            unlikely = sum(likely_voter == 0),
            total = n())


grouped_voter_data %>% write_csv("grouped_voter_data.csv")