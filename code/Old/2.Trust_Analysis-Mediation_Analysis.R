# Load packages and data ----

pacman::p_load(lme4)

# set.seed(123)

## Load dat.ma (i.e. the cleaned data)
# load(here("data", "CleanDataFile-Trust-Nov102023.RData"))


get_M_coefs = function(dat.ma){
  
  require(lme4)
  
  mod_M  =  glmer(
    q4.src ~ q8.pcis + q2.dc + q3.pc +
      age_group + gender + q7.la +  q9.edu +
      (q8.pcis | country),
    data = dat.ma,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  output = list()
  output$global = summary(mod_M)$coef
  output$country = ranef(mod_M)$country
  return(output)
}

get_Y_coefs = function(dat.ma){
  
  require(lme4)
  
  mod_Y = glmer(
    q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
      age_group + gender + q7.la  + q9.edu +
      (q8.pcis + q4.src | country),
    data = dat.ma,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  output = list()
  output$global = summary(mod_Y)$coef
  output$country = ranef(mod_Y)$country
  return(output)
}

get_mediation_effects = function(Y_coefs, M_coefs){
  # Extract relevant fixed effects
  Y_global = Y_coefs$global
  M_global = M_coefs$global
  X_in_Y_fix = Y_global[3,1]
  M_in_Y_fix = Y_global[4,1]
  X_in_M_fix = M_global[3,1]
  
  # Compute global mediation effects
  de = exp(X_in_Y_fix)
  ie = exp(X_in_M_fix * M_in_Y_fix)
  te = exp(X_in_Y_fix + X_in_M_fix * M_in_Y_fix)
  
  global_effects = c(de, ie, te)
  names(global_effects) = c("de", "ie", "te")
  
  
  
  #### for each country ----
  country_name = as.vector(unique(dat.ma$country_name))
  country_effects = data.frame(country_name = country_name)
  
  # Extract relevant random effects
  Y_country = Y_coefs$country
  M_country = M_coefs$country
  X_in_Y_ran = Y_country[,3]
  M_in_Y_ran = Y_country[,4]
  X_in_M_ran = M_country[,3]
  
  # Compute relevant mixed effects
  X_in_Y_mix = X_in_Y_fix + X_in_Y_ran
  M_in_Y_mix = M_in_Y_fix + M_in_Y_ran
  X_in_M_mix = X_in_M_fix + X_in_M_ran
  
  # Compute mediation effects
  de_country = exp(X_in_Y_mix)
  country_effects = cbind(country_effects, de_country)
  
  ie_country = exp(X_in_M_mix * M_in_Y_mix)
  country_effects = cbind(country_effects, ie_country)
  
  te_country = exp(X_in_Y_mix + X_in_M_mix * M_in_Y_mix)
  country_effects = cbind(country_effects, te_country)
  
  
  
  
  return(list(global_effects = global_effects, country_effects = country_effects))
}




# Run mediation analysis on a single (bootstrap) dataset
# Returns estimated direct, indirect and total effects, both globally and by country.
single_dataset_analysis = function(dat.ma) {
  
  all_Y_effects = get_Y_coefs(dat.ma)
  all_M_effects = get_M_coefs(dat.ma)
  
  all_mediation_effects = get_mediation_effects(all_Y_effects, all_M_effects)
  return(all_mediation_effects)
}
   

# single_dataset_analysis = function(dat.ma){
#   # 3. Multilevel mediation analysis  ----
#   
#   
#   ## Run the mediation analysis ----
#   # M : q4.self reported compliance
#   # A : q8.primary covid information source
#   # Y : q5.future compliance
#   
#   ### Multilevel regression for the mediator ----
#   
#   
#   mod_M  =  glmer(
#     q4.src ~ q8.pcis + q2.dc + q3.pc +
#       age_group + gender + q7.la +  q9.edu +
#       (q8.pcis | country),
#     data = dat.ma,
#     family = "binomial",
#     control = glmerControl(optimizer = "bobyqa",
#                            optCtrl = list(maxfun = 2e5))
#   )
#   mod_M_sum <- summary(mod_M)
#   # mod_M_fixed = glm(q4.src ~ q8.pcis + q2.dc + q3.pc +
#   #                     age_group + gender + q7.la +  q9.edu,
#   #                   data = dat.ma,
#   #                   family = "binomial")
#   # mod_M_empty = glm(q4.src ~ 1, data = dat.ma,
#   #                   family = "binomial")
#   # anova(mod_M_empty, mod_M_fixed, mod_M, test = "LRT")
#   
#   
#   ### Multilevel regression for the outcome ----
#   
#   
#   mod_Y = glmer(
#     q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
#       age_group + gender + q7.la  + q9.edu +
#       (q8.pcis + q4.src | country),
#     data = dat.ma,
#     family = "binomial",
#     control = glmerControl(optimizer = "bobyqa",
#                            optCtrl = list(maxfun = 2e5))
#   )
#   # interaction q8.pcis*q4.src.num is not significant
#   
#   mod_Y_sum <- summary(mod_Y)
#   
#   # mod_Y_ran_A = glmer(
#   #   q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
#   #     age_group + gender + q7.la  + q9.edu +
#   #     (q8.pcis| country),
#   #   data = dat.ma,
#   #   family = "binomial",
#   #   control = glmerControl(optimizer = "bobyqa",
#   #                          optCtrl = list(maxfun = 2e5))
#   # )
#   # 
#   # mod_Y_ran_M = glmer(
#   #   q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
#   #     age_group + gender + q7.la  + q9.edu +
#   #     (q8.pcis | country),
#   #   data = dat.ma,
#   #   family = "binomial",
#   #   control = glmerControl(optimizer = "bobyqa",
#   #                          optCtrl = list(maxfun = 2e5))
#   # )
#   # 
#   # mod_Y_fixed = glm(
#   #   q5.fc ~ q8.pcis + q4.src + q1.cc  + q2.dc + q3.pc +
#   #     age_group + gender + q7.la  + q9.edu,
#   #   data = dat.ma,
#   #   family = "binomial")
#   # 
#   # 
#   # anova(mod_Y_fixed, mod_Y_ran_M, test = "Chisq")
#   # anova(mod_Y_fixed, mod_Y_ran_A, test = "Chisq")
#   # anova(mod_Y_fixed, mod_Y, test="Chisq")
#   
#   # the random effect for each country in the two models
#   ranef(mod_M)
#   ranef(mod_Y)
#   
#   ### Estimation of the mediation parameters ----
#   #### overall ----
#   ## easier framework : no interaction between exposure and mediator
#   ## logit(P[Y = 1|A, M]) = \theta_O + \theta_1 a + \theta-2 m + \theta_3^T c
#   ## logit(P[M = 1|A]) = \beta_O + \beta_1 a + \beta_2^T c
#   
#   ## DE = exp(\theta_1)
#   ## IE = exp(\beta_1 * \theta_2)
#   # sure pcis vs low pcis (primary covid information source)
#   
#   de = exp(mod_Y_sum$coefficients[2])
#   ie = exp(mod_M_sum$coefficients[2] * mod_Y_sum$coefficients[4])
#   te = exp(mod_Y_sum$coefficients[2] + mod_M_sum$coefficients[2] * mod_Y_sum$coefficients[4])
#   
#   global_effects = c(de, ie, te)
#   names(global_effects) = c("de", "ie", "te")
#   
#   #### for each country ----
#   country_name = as.vector(unique(dat.ma$country_name))
#   country_effects = data.frame(country_name = country_name)
#   
#   de_country_rad = exp(mod_Y_sum$coefficients[2] + ranef(mod_Y)[[1]][, 2])
#   country_effects = cbind(country_effects, de_country)
#   
#   ie_country_rad = exp((mod_M_sum$coefficients[2] + ranef(mod_M)[[1]][, 2]) *
#                      (mod_Y_sum$coefficients[4] + ranef(mod_Y)[[1]][, 4]))
#   country_effects = cbind(country_effects, ie_country)
#   
#   te_country_rad = exp(
#     mod_Y_sum$coefficients[2] + ranef(mod_Y)[[1]][, 2] +
#       (mod_M_sum$coefficients[2] + ranef(mod_Y)[[1]][, 2]) *
#       (mod_Y_sum$coefficients[4] + ranef(mod_Y)[[1]][, 4])
#   )
#   country_effects = cbind(country_effects, te_country)
#   
#   
#   
#   
#   return(list(global_effects = global_effects, country_effects = country_effects))
#   
#   
# }
