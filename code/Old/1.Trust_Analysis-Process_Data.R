######                TRUST STUDY : DATA ANALYSIS                         ######

# Set working directory and load dataset ----

# setwd("C:/Users/rador/OneDrive - Universite de Montreal/UdeM/Labs and project/Trust")
# setwd("C:/Users/Rado Ramasy.RADORAMASY/OneDrive - Universite de Montreal/UdeM/Labs and project/Trust")

pacman::p_load(readr, tidyverse, labelled, gtsummary, flextable,
               modelsummary, here)
set.seed(123)

dat_raw <- read_csv(here("data","RawDataFile-Trust-Jan62023.csv"))





# 1. Prepocessing ----

## Remove unnecessary variables ----

dat <- dat_raw[ , - c(1,4:12,17:23,29:32,43)]


## transform character to factor ----

dat <- dat %>% mutate_if(is.character, as.factor)
dat$wave <- as.factor(dat$wave)

## Set variable labels ----


mylabels <-
  as.list( c(
    age = "Age",
    age_group = "Age group",
    gender = "Gender",
    wave = "Wave",
    q01_community_compliance = "Q01. Community compliance",
    q02_doctors_communication = "Q02. Doctors communication",
    q03_political_communication = "Q03. Political communication",
    q04_self_report_compliance = "Q04. Self report compliance",
    q05_future_compliance = "Q05. Future compliance",
    q06_future_information_source = "Q06. Future information source",
    q07_live_area = "Q07. Live area",
    q08_primary_covid_info_source = "Q08. Primary Covid information source",
    q09_education_completed = "Q09 Education completed"
  ))

dat <- set_variable_labels(dat, .labels = mylabels)



## Reorder levels ----

### q01_community_compliance
dat$q01_community_compliance <- dat$q01_community_compliance %>%
  fct_relevel(
    "yes_almost_everyone","yes_many", "maybe_half",  "no_many",
    "no_almost_no_one"
  )

### q02_doctors_communication
dat$q02_doctors_communication <- dat$q02_doctors_communication %>%
  fct_relevel(
    "yes_completely", "yes_quite_useful", "somewhat_useful", "no_not_useful",
    "no_wrong_misleading"
  )


### q03_political_communication
dat$q03_political_communication <- dat$q03_political_communication %>%
  fct_relevel(
    "yes_completely", "yes_quite_useful",  "somewhat_useful", "no_not_useful",
    "no_wrong_misleading"
  )

### q04_self_report_compliance
dat$q04_self_report_compliance <- dat$q04_self_report_compliance %>%
  fct_relevel(
    "all_guidelines", "some_guidelines", "most_guidelines", "no_guidelines"
  )


### q05_future_compliance
dat$q05_future_compliance <- dat$q05_future_compliance %>%
  fct_relevel(
    "all_guidelines", "most_guidelines", "some_guidelines", "no_guidelines"
  )

### q06_future_information_source
dat$q06_future_information_source <- dat$q06_future_information_source %>%
  fct_relevel(
    "world_health_org", "red_cross_crescent", "doctors_nurses",
    "national_gov_health_minister", "local_gov", "public_health_officials",
    "military_police", "none_useful_believable"
  )


### q08_primary_covid_info_source
dat$q08_primary_covid_info_source <- dat$q08_primary_covid_info_source %>%
  fct_relevel(
    "international_media", "who", "national_media", "gov_briefings",
    "scientific_publications", "tv_news", "radio", "podcasts_blogs",
    "social_media", "social_messengers", "friends_family"
  )

### q09_education_completed
dat$q09_education_completed <- dat$q09_education_completed %>%
  fct_relevel(
    "no_formal_education", "primary_school", "secondary_school",
    "post_secondary", "bachelors_degree", "masters_degree_or_higher"
  )


## Merge levels ----

dat.rec <- dat


### q01_community_compliance
dat.rec$q01_community_compliance <- dat.rec$q01_community_compliance %>%
  fct_collapse(yes = c("yes_almost_everyone","yes_many"),
               no = c( "maybe_half", "no_many","no_almost_no_one"))

### q02_doctors_communication
dat.rec$q02_doctors_communication <- dat.rec$q02_doctors_communication %>%
  fct_collapse(yes = c("yes_completely", "yes_quite_useful"),
               no = c("somewhat_useful", "no_not_useful", "no_wrong_misleading"))

### q03_political_communication
dat.rec$q03_political_communication <- dat.rec$q03_political_communication %>%
  fct_collapse(yes = c("yes_completely", "yes_quite_useful"),
               no = c("somewhat_useful", "no_not_useful", "no_wrong_misleading"))

### q04_self_report_compliance
dat.rec$q04_self_report_compliance <- dat.rec$q04_self_report_compliance %>%
  fct_collapse(
    all_or_most = c("all_guidelines", "most_guidelines"),
    no_or_some = c("some_guidelines","no_guidelines"))


### q05_future_compliance
dat.rec$q05_future_compliance <- dat.rec$q05_future_compliance %>%
  fct_collapse(
    all_or_most = c("all_guidelines", "most_guidelines"),
    no_or_some = c("some_guidelines","no_guidelines"))

### q06_future_information_source
dat.rec$q06_future_information_source <- dat.rec$q06_future_information_source %>%
  fct_collapse(
    NGO = c("world_health_org", "red_cross_crescent"),
    heath.resp = c("doctors_nurses", "public_health_officials"),
    gov = c("national_gov_health_minister", "local_gov","military_police"),
    none = c("none_useful_believable")
  )


### q08_primary_covid_info_source
dat.rec$q08_primary_covid_info_source <- dat.rec$q08_primary_covid_info_source %>%
  fct_collapse(
    sure = c("international_media", "who", "scientific_publications") , #reputable 
    medium = c("national_media", "gov_briefings","tv_news", "radio"),
    low = c("podcasts_blogs", "social_media", "social_messengers", "friends_family"))

### q09_education_completed
dat.rec$q09_education_completed <- dat.rec$q09_education_completed %>%
  fct_collapse(
    primary.or.lower = c("no_formal_education", "primary_school"),
    secondary = c("secondary_school"),
    university = c("post_secondary", "bachelors_degree", "masters_degree_or_higher"))




## Dealing with missing data ----

questionr::freq.na(dat.rec)



dat.cc <- dat.rec[complete.cases(dat.rec),] # data with only complete cases

questionr::freq.na(dat.cc) #no missing data left





## Get shorter names (premaring mediation analysis)

dat.ma <- dat.cc  %>% rename(
  q1.cc = q01_community_compliance,
  q2.dc = q02_doctors_communication,
  q3.pc = q03_political_communication,
  q4.src = q04_self_report_compliance,
  q5.fc = q05_future_compliance,
  q6.fis = q06_future_information_source,
  q7.la = q07_live_area,
  q8.pcis = q08_primary_covid_info_source,
  q9.edu = q09_education_completed
)


dat.ma <- dat.ma %>% mutate(
  q5.fc.num = fct_recode(q5.fc, "1" = "all_or_most", "0" = "no_or_some") %>% 
    as.character() %>% as.numeric(),
  q4.src.num = fct_recode(q4.src, "1" = "all_or_most", "0" = "no_or_some")  %>% 
    as.character() %>% as.numeric(),
  q8.pcis.num = fct_recode(q8.pcis, "2" = "sure", "1" = "medium","0" = "low") %>% 
    as.character() %>% as.numeric())

str(dat.ma)


## Set variables reference
dat.ma$q4.src <- relevel(dat.ma$q4.src, ref = "no_or_some")
dat.ma$q5.fc  <- relevel(dat.ma$q5.fc , ref = "no_or_some")

dat.ma$q8.pcis <- relevel(dat.ma$q8.pcis, ref = "low")


dat.ma$q6.fis <- relevel(dat.ma$q6.fis, ref = "NGO")



## Save final dataset ----
data_file_name = here("data","CleanDataFile-Trust-Nov102023.RData")
save(dat.ma, file=data_file_name)

