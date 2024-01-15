######                TRUST STUDY : DATA ANALYSIS                         ######

# Set working directory and load dataset ----

# setwd("C:/Users/rador/OneDrive - Universite de Montreal/UdeM/Labs and project/Trust")
# setwd("C:/Users/Rado Ramasy.RADORAMASY/OneDrive - Universite de Montreal/UdeM/Labs and project/Trust")

pacman::p_load(readr, tidyverse, labelled, gtsummary, flextable,
               modelsummary)
set.seed(123)

dat_raw <- read_csv("RawDataFile-Trust-Jan62023.csv")

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


#...............................................................................

# 2. Descritpive statistics ----
## Table 1 with missing data -----


tab1 <- dat.rec %>% 
  dplyr::select(-uuid, -country, -date,-city,-region,-q00_live, - weight) %>% 
  tbl_summary(
    by = country_name,
    percent = "column",
    missing_text = "Missing",
    
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p})"),
    
    digits = list(all_continuous() ~ c(1, 1),
                  all_categorical() ~ c(0, 1))) %>%
  
  modify_header(label = "**Characteristics**",
                all_stat_cols(F) ~ "**{level}** \n n = {n}") %>% 
  add_overall()


# export table as a docx file
save_as_docx ( as_flex_table(tab1) , path = "table1_0719.docx") 


## Table 1 with complete cases ----


tab1.cc <- dat.cc %>% 
  dplyr::select(-uuid, -country, -date,-city,-region,-q00_live, - weight) %>% 
  tbl_summary(
    by = country_name,
    percent = "column",
    
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p})"),
    
    digits = list(all_continuous() ~ c(1, 1),
                  all_categorical() ~ c(0, 1))) %>%
  
  modify_header(label = "**Characteristics**",
                all_stat_cols(F) ~ "**{level}** \n n = {n}") %>% 
  add_overall()




# export table as a docx file
save_as_docx ( as_flex_table(tab1.cc) , path = "table1.cc_19.docx") 



#...............................................................................

# 3. Multilevel mediation analysis  ----
## Set variables reference

dat.cc$q05_future_compliance <- relevel(dat.cc$q05_future_compliance,
                                        ref = "all_or_most")

dat.ma$q4.src <- relevel(dat.ma$q4.src, ref = "no_or_some")
dat.ma$q5.fc  <- relevel(dat.ma$q5.fc , ref = "no_or_some")

dat.ma$q8.pcis <- relevel(dat.ma$q8.pcis, ref = "low")


dat.ma$q6.fis <- relevel(dat.ma$q6.fis, ref = "NGO")


## Run the mediation analysis ----
library(lme4)
# M : q4.self reported compliance
# A : q8.primary covid information source
# Y : q5.future compliance

### Multilevel regression for the mediator ----


mod_M  =  glmer(q4.src ~ q8.pcis + q2.dc + q3.pc + 
                     age_group + gender + q7.la +  q9.edu +
                     (q8.pcis |country), 
                   data = dat.ma, family = "binomial",
                   control= glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=2e5)))
mod_M_sum <- summary(mod_M )


### Multilevel regression for the outcome ----


mod_Y = glmer(q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc + 
                        age_group + gender + q7.la  + q9.edu + 
                        (q8.pcis + q4.src |country),
                      data = dat.ma, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))
                 # interaction q8.pcis*q4.src.num is not significant

mod_Y_sum <- summary(mod_Y)



# the random effect for each country in the two models
ranef(mod_M)
ranef(mod_Y)



mediation.fit = mediate(model.m = mod_M,
                   model.y = mod_Y,
                   treat = "pcis",
                   mediator = "src",
                   control.value = "0",
                   treat.value = "1",
                   sims = 10)












### Estimation of the mediation parameters ----

#### Sure pcis vs medium pscis ----

#### overall 
## easier framework : no interaction between exposure and mediator
## logit(P[Y = 1|A, M]) = \theta_O + \theta_1 a + \theta-2 m + \theta_3^T c
## logit(P[M = 1|A]) = \beta_O + \beta_1 a + \beta_2^T c

## DE = exp(\theta_1)
## IE = exp(\beta_1 * \theta_2)
# sure pcis vs medium pcis (primary covid information source)

de = exp(mod_Y_sum$coefficients[2])
ie = exp(mod_M_sum$coefficients[2] * mod_Y_sum$coefficients[4])
te = exp(mod_Y_sum$coefficients[2] + mod_M_sum$coefficients[2]* mod_Y_sum$coefficients[4])

#### for each country 
country_name = as.vector(unique(dat.ma$country_name))

de_country = exp(mod_Y_sum$coefficients[2] + ranef(mod_Y)[[1]][,2])
de_country = cbind(country_name, de_country)

ie_country = exp( (mod_M_sum$coefficients[2] + ranef(mod_Y)[[1]][,2]) * 
                    (mod_Y_sum$coefficients[4] +ranef(mod_Y)[[1]][,4]))
ie_country = cbind(country_name, ie_country)

te_country = exp( mod_Y_sum$coefficients[2] + ranef(mod_Y)[[1]][,2] + 
                    (mod_M_sum$coefficients[2] + ranef(mod_Y)[[1]][,2]) * 
                    (mod_Y_sum$coefficients[4] +ranef(mod_Y)[[1]][,4]))
te_country = cbind(country_name, te_country)


#### Sure pcis vs low pscis ----

#### overall 
## easier framework : no interaction between exposure and mediator
## logit(P[Y = 1|A, M]) = \theta_O + \theta_1 a + \theta-2 m + \theta_3^T c
## logit(P[M = 1|A]) = \beta_O + \beta_1 a + \beta_2^T c

## DE = exp(\theta_1)
## IE = exp(\beta_1 * \theta_2)
# sure pcis vs low pcis (primary covid information source)

de2 = exp(mod_Y_sum$coefficients[3])
ie2 = exp(mod_M_sum$coefficients[3] * mod_Y_sum$coefficients[4])
te2 = exp(mod_Y_sum$coefficients[3] + mod_M_sum$coefficients[3]* mod_Y_sum$coefficients[4])

#### for each country 
country_name = as.vector(unique(dat.ma$country_name))

de_country = exp(mod_Y_sum$coefficients[2] + ranef(mod_Y)[[1]][,2])
de_country = cbind(country_name, de_country)

ie_country = exp( (mod_M_sum$coefficients[2] + ranef(mod_Y)[[1]][,2]) * 
                    (mod_Y_sum$coefficients[4] +ranef(mod_Y)[[1]][,4]))
ie_country = cbind(country_name, ie_country)

te_country = exp( mod_Y_sum$coefficients[2] + ranef(mod_Y)[[1]][,2] + 
                    (mod_M_sum$coefficients[2] + ranef(mod_Y)[[1]][,2]) * 
                    (mod_Y_sum$coefficients[4] +ranef(mod_Y)[[1]][,4]))
te_country = cbind(country_name, te_country)






### Estimation of standard error with bootstrap ----


## Create a vector for each expected output 
### 3 paramaters for overall and for each country
list_output <- list ("de_all", "ie_all", "te_all",
                     "de_CA","de_TH","de_EG","de_IN","de_PH","de_NI","de_FR","de_KO","de_BR","de_ME","de_TU",
                     "ie_CA","ie_TH","ie_EG","ie_IN","ie_PH","ie_NI","ie_FR","ie_KO","ie_BR","ie_ME","ie_TU",
                     "te_CA","te_TH","te_EG","te_IN","te_PH","te_NI","te_FR","te_KO","te_BR","te_ME", "te_TU")


for (object_name in list_output) {
  assign(object_name, numeric(0))
}


## Create the for loop

country_name = as.vector(unique(dat.ma$country_name))
n_boot = 10


for(i in 1:n_boot){
  
  data_sample <- dat.ma[sample(1:nrow(dat.ma), replace = TRUE),]
  # did we have to do the boot for the overall dataset or inside the country group?
  
  
  
  # Fit the mediator model
  tryCatch({
    model_M_b <- update(mod_M, data = data_sample)
  }, error = function(e) {
    cat("Error in mediator model:", conditionMessage(e), "\n")
    next  # Skip to the next iteration if the mediator model fails
  })
  
  
  # Fit the outcome model
  tryCatch({
    model_Y_b <- update(mod_Y, data = data_sample)
  }, error = function(e) {
    cat("Error in outcome model:", conditionMessage(e), "\n")
    next  # Skip to the next iteration if the outcome model fails
  })
  
  
  
  # If both models run successfully, calculate the mediation effect
  if (!inherits(model_Y_b, "try-error") && !inherits(model_M_b, "try-error")) {
    # Calculate the mediation effect 
    
    
    mod_M_sum_b <- summary(model_M_b)
    mod_Y_sum_b <- summary(model_Y_b)
    
    
    ## Estimating the overall effect
    
    de_b = exp( mod_Y_sum_b$coefficients[2])
    ie_b = exp(mod_M_sum_b $coefficients[2] *  mod_Y_sum_b$coefficients[4])
    te_b = exp( mod_Y_sum_b$coefficients[2] + mod_M_sum_b$coefficients[2]*  mod_Y_sum_b$coefficients[4])
    
    ## Store the result on a vector
    de_all[i] = de_b
    ie_all[i] = ie_b
    te_all[i] = te_b
    
    
    ## Estimating the effet by country
    
    de_country_b = exp(mod_Y_sum_b$coefficients[2] + ranef(model_Y_b)[[1]][,2])
    de_country_b = cbind(country_name, de_country_b)
    
    ie_country_b = exp( (mod_M_sum$coefficients[2] + ranef(model_Y_b)[[1]][,2]) * 
                          (mod_Y_sum_b$coefficients[4] +ranef(model_Y_b)[[1]][,4]))
    ie_country_b = cbind(country_name, ie_country_b)
    
    te_country_b = exp( mod_Y_sum_b$coefficients[2] + ranef(model_Y_b)[[1]][,2] + 
                          (mod_M_sum$coefficients[2] + ranef(model_Y_b)[[1]][,2]) * 
                          (mod_Y_sum_b$coefficients[4] +ranef(model_Y_b)[[1]][,4]))
    te_country_b = cbind(country_name, te_country_b)
    
    ## Store the result for each country
    ### for direct effect
    de_CA[i] = de_country_b[country_name == "Canada",2][[1]]
    de_TH[i] = de_country_b[country_name == "Thailand",2][[1]]
    de_EG[i] = de_country_b[country_name == "Egypt",2][[1]]
    de_IN[i] = de_country_b[country_name == "India",2][[1]]
    de_PH[i] = de_country_b[country_name == "Philippines",2][[1]]
    de_NI[i] = de_country_b[country_name == "Nigeria",2][[1]]
    de_FR[i] = de_country_b[country_name == "France",2][[1]]
    de_KO[i] = de_country_b[country_name == "Korea_South",2][[1]]
    de_BR[i] = de_country_b[country_name == "Brazil",2][[1]]
    de_ME[i] = de_country_b[country_name == "Mexico",2][[1]]
    de_TU[i] = de_country_b[country_name == "Turkey",2][[1]]
    
    ### for indirect effect
    
    ie_CA[i] = ie_country_b[country_name == "Canada",2][[1]]
    ie_TH[i] = ie_country_b[country_name == "Thailand",2][[1]]
    ie_EG[i] = ie_country_b[country_name == "Egypt",2][[1]]
    ie_IN[i] = ie_country_b[country_name == "India",2][[1]]
    ie_PH[i] = ie_country_b[country_name == "Philippines",2][[1]]
    ie_NI[i] = ie_country_b[country_name == "Nigeria",2][[1]]
    ie_FR[i] = ie_country_b[country_name == "France",2][[1]]
    ie_KO[i] = ie_country_b[country_name == "Korea_South",2][[1]]
    ie_BR[i] = ie_country_b[country_name == "Brazil",2][[1]]
    ie_ME[i] = ie_country_b[country_name == "Mexico",2][[1]]
    ie_TU[i] = ie_country_b[country_name == "Turkey",2][[1]]
    
    ### for total effect
    
    te_CA[i] = te_country_b[country_name == "Canada",2][[1]]
    te_TH[i] = te_country_b[country_name == "Thailand",2][[1]]
    te_EG[i] = te_country_b[country_name == "Egypt",2][[1]]
    te_IN[i] = te_country_b[country_name == "India",2][[1]]
    te_PH[i] = te_country_b[country_name == "Philippines",2][[1]]
    te_NI[i] = te_country_b[country_name == "Nigeria",2][[1]]
    te_FR[i] = te_country_b[country_name == "France",2][[1]]
    te_KO[i] = te_country_b[country_name == "Korea_South",2][[1]]
    te_BR[i] = te_country_b[country_name == "Brazil",2][[1]]
    te_ME[i] = te_country_b[country_name == "Mexico",2][[1]]
    te_TU[i] = te_country_b[country_name == "Turkey",2][[1]]
    
    
    
    # Display a success message
    cat("Iteration:", i, "Both models ran successfully!\n")
  } else {
    cat("Iteration:", i, "Redoing resampling and models...\n")
    
  }
  
  
  # # the output outside the loop are automatically updated
  # return(de_all, ie_all, te_all,
  #        de_CA, de_TH, de_EG, de_IN, de_PH, de_NI, de_FR, de_KO, de_BR, de_ME, de_TU,
  #        ie_CA, ie_TH, ie_EG, ie_IN, ie_PH, ie_NI, ie_FR, ie_KO, ie_BR, ie_ME, ie_TU,
  #        te_CA, te_TH, te_EG, te_IN, te_PH, te_NI, te_FR, te_KO, te_BR, te_ME, te_TU)
}



# just to get a quick overview, but we can do more fancy things here

output_boot <- cbind.data.frame (de_all, ie_all, te_all,
         de_CA, de_TH, de_EG, de_IN, de_PH, de_NI, de_FR, de_KO, de_BR, de_ME, de_TU,
         ie_CA, ie_TH, ie_EG, ie_IN, ie_PH, ie_NI, ie_FR, ie_KO, ie_BR, ie_ME, ie_TU,
         te_CA, te_TH, te_EG, te_IN, te_PH, te_NI, te_FR, te_KO, te_BR, te_ME, te_TU)
output_boot <- output_boot %>% mutate_if(is.character, as.numeric)

summary(output_boot)



# Save output ----

save.image("trust1102.RData")








