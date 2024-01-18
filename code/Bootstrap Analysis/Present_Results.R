


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

load(here("code", "Bootstrap Analysis", "Output", "Estimated_Effects.RData"))


# Table of estimated mediation effects ----

med_effs_table_data = all_med_effects_data %>%
  bind_rows(slice_head(.)) %>% slice(-1) %>%                               # Move first row (average) to bottom
  mutate(country = case_match(country,
                              "BR" ~ "Brazil",
                              "CA" ~ "Canada",
                              "EG" ~ "Egypt",
                              "FR" ~ "France",
                              "IN" ~ "India",
                              "KR" ~ "South Korea",
                              "MX" ~ "Mexico",
                              "NG" ~ "Nigeria",
                              "PH" ~ "Philippines",
                              "TH" ~ "Thailand",
                              "TR" ~ "Turkey",
                              "AG" ~ "Average"
                              )) %>%
  mutate(Country = country, Direct = de, Indirect = ie, Total = te, .keep="none")

print(xtable(med_effs_table_data, type="latex", 
       caption = "Estimated mediation effects by country and globally.", label = "tab:med_eff"),
include.rownames=F, hline.after = c(-1, 0, nrow(med_effs_table_data)-1, nrow(med_effs_table_data)))
