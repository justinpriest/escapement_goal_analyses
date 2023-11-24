library(tidyverse)
library(lubridate)
library(readxl)

source("code/1_functions.R")

auketotalrun <- read_csv("data/auke_escapement&harvest_1980-2023.csv") %>%
  dplyr::select(Location, Year, Escapement_Adult, TagsFound_Adult, 
                TagsFound_Jack, RunCaught_WEIR_NOEXP, RunCaught_THETAEXP) %>%
  # NEED TO UPDATE JACK ESC
  mutate(adults_total_tag = TagsFound_Adult + RunCaught_WEIR_NOEXP,
         adults_total_all = Escapement_Adult + RunCaught_THETAEXP) %>%
  rename("returnyear" = Year, 
         "adult_esc_all" = Escapement_Adult,
         "harvestadults" = RunCaught_THETAEXP,
         "adult_esc_tag" = TagsFound_Adult, 
         "jack_esc_tag" = TagsFound_Jack) 
# The *_tag columns exclude untagged coho that go into Auke Creek making the 
#  rather large assumption that those coho are all strays since the weir 
#  must have 100% efficiency. 
# The *_all columns include both tagged and unmarked fish which likely 
#  includes a number of strays. 





# read_excel("data/Auke_Coho_ASL_1980-2022_JRussell20230317.xlsx",
#            sheet = "Smolt Counts")
# read_excel("data/Auke_Coho_ASL_1980-2022_JRussell20230317.xlsx",
#            sheet = "Smolt Samples")



# This is now superceded by the esc and harvest file. But some of these numbers
# slightly conflict with what was provided by NOAA

aukeesc <- read_excel("data/Auke_Coho_ASL_1980-2022_JRussell20230317.xlsx",
           sheet = "Adult Counts", guess_max = 6000) %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(adultesc_all = sum(`Coho Adult Releases`, na.rm = TRUE), 
            jackesc_all = sum(`Coho Jack Releases`, na.rm = TRUE)) 
# This uses escapement of ALL coho into Auke. Prev analyses used only 
#   marked coho with the major assumption that any unmarked fish were 
#   strays.



aukebrood_all <- read_excel("data/Auke_Coho_ASL_1980-2022_JRussell20230317.xlsx",
           sheet = "Adult Samples") %>%
  filter(!is.na(`FW Age` | `SW Age`)) %>%
  mutate(`SW Age` = if_else(Sex == "J" & `SW Age` == 2, 0, `SW Age`), 
         # Fix jacks that were assigned SW age of 2 in 2021, & 2022
         # Note that one fish in 2015 was recorded as a 2.2. Correct?
         age_euro = paste0("Age_", `FW Age`, ".", `SW Age`),
         year = year(Date),
         counttemp = 1) %>%
  dplyr::select(year, age_euro, counttemp) %>%
  group_by(year, age_euro) %>%
  summarise(agecount = sum(counttemp)) %>% 
  #pivot_wider(names_from = age_euro, values_from = agecount) 
  # Can uncomment to see table of ages
  mutate(broodyear = case_when(age_euro == "Age_1.0" ~ year - 2,
                               age_euro == "Age_1.1" ~ year - 3,
                               age_euro == "Age_2.0" ~ year - 3,
                               age_euro == "Age_2.1" ~ year - 4,
                               age_euro == "Age_2.2" ~ year - 5,
                               .default = NA)) %>%
  ungroup()
aukebrood_all

aukebrood_nojacks <- aukebrood_all %>%
  filter(age_euro != "Age_1.0" & age_euro != "Age_2.0") # remove jacks
aukebrood_nojacks


