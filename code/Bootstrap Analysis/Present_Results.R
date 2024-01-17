


pacman::p_load(readr,
               tidyverse,
               labelled,
               gtsummary,
               flextable,
               modelsummary,
               here,
               lme4,
               tictoc,
               xtable)




# Load other scripts ----
source(here("code", "Bootstrap Analysis", "Bootstrap_CI_Helper_Functions.R"))
source(here("code", "Bootstrap Analysis", "Run_Bootstrap_Functions.R"))
source(here("code", "Bootstrap Analysis", "Analyse_Bootstrap_Functions.R"))


load(here("data","CleanDataFile-Trust-Nov102023.RData"))

load(here("code", "Bootstrap Analysis", "Estimated_Effects.RData"))


all_med_effects_data
xtable(all_med_effects_data)
