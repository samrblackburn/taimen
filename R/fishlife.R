

fishlife_targets <- list(
  
  # species of interest
  tar_target(species_list, c("Hucho taimen", "Parahucho perryi")),
  
  # get mean values of fishlife parameters for species of interest, drop a few parameters we don't need
  tar_target(fishlife_means, FishBase_and_Morphometrics$beta_gv %>% 
               as_tibble(rownames = "species", .name_repair = "universal") %>% 
               filter(species %in% species_list) %>% 
               rename_with(~ str_replace(., "\\.+$", "")) %>% # drop trailing periods
               rename_with(~ str_c(., "_mean"), .cols = where(is.numeric)) %>%
               select(-c(contains("base"), contains("spawning"), contains("habitat"), contains("feeding"), contains("shape")))),
  
  # function to get variance estimates from fishlife for specific species
  tar_target(get_var, function(species_name){
    diag(FishBase_and_Morphometrics$Cov_gvv[species_name,,]) %>% 
      as_tibble_row(.name_repair = "universal") %>% 
      rename_with(~ str_replace(., "\\.+$", "")) %>% # drop trailing periods
      rename_with(~ str_c(., "_var")) %>% 
      mutate(species = species_name)
  }),
  
  # get variance values of fishlife parameters for species of interest, drop a few parameters we don't need
  tar_target(fishlife_vars, map(species_list, get_var) %>% 
               list_rbind() %>%  
               select(-c(contains("base"), contains("spawning"), contains("habitat"), contains("feeding"), contains("shape"))) %>% 
               relocate(species)),
  
  # combine means and variance values, format, remove unneeded variables
  tar_target(fishlife_values, full_join(fishlife_means, fishlife_vars, by = join_by(species))  %>% 
               mutate(across(contains("var"),
                             ~ sqrt(.x),
                             .names = "{str_replace(.col, 'var', 'sd')}"
               )) %>% 
               select(-contains("var")) %>% 
               mutate(across(where(is.numeric), signif)) %>% #trim off extra digits
               select(species, log.linf_mean = log.length_infinity_mean, log.m_mean = log.natural_mortality_mean, log.k_mean = log.growth_coefficient_mean, 
                      log.linf_sd = log.length_infinity_sd, log.m_sd = log.natural_mortality_sd, log.k_sd = log.growth_coefficient_sd)) 
  
  
  
  ## normally distributed mean and variance
  # μ = mean of log(X)
  # σ = var of log(X)
  # mean(X) = exp(μ + σ/2)
  # SD(X) =  sqrt(exp(σ) – 1) * mean(X)
  # tar_target(unlog_values, full_join(means, vars, by = join_by(species)) %>%
  #              # get mean based on log mean and log var
  #              mutate(across(contains("log.") & contains("_mean"),
  #                            ~ exp(.x + (cur_data()[[str_replace(cur_column(), "_mean", "_var")]] / 2)),
  #                            .names = "{str_replace(.col, 'log.', '')}")) %>%
  #              # get SD based on mean and log var
  #              mutate(across(contains("log.") & contains("_var"),
  #                            ~ sqrt(exp(.x) - 1) * cur_data()[[str_replace(str_replace(cur_column(), 'log.', ''), '_var', '_mean')]],
  #                            .names = "{str_replace(str_replace(.col, 'log.', ''), '_var', '_sd')}")) %>%
  #              select(-contains("log")) %>%
  #              mutate(across(contains("var"),
  #                            ~ sqrt(.x),
  #                            .names = "{str_replace(.col, 'var', 'sd')}"
  #              )) %>%
  #              select(-contains("var")) %>%
  #              mutate(across(where(is.numeric), signif))) #trim off extra digits
  
  
)

