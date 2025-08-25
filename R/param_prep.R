param_prep_targets <- list(
  
  # fish length data for each populations
  # files
  tar_target(delger_length_file, "data/DelgerLengthAssessmentWorkshop_final.csv", format = "file"),
  tar_target(onon_length_file, "data/OnonLengthAssessmentWorkshop_final.csv", format = "file"),
  tar_target(eguur_length_file, "data/EgUurLengthAssessmentWorkshop_final.csv", format = "file"),
  tar_target(tugur_length_file, "data/TugurLengthAssessmentWorkshop_final.csv", format = "file"),
  tar_target(karibetsu_length_file, "data/KaribetsuLengthAssessmentWorkshop_final.csv", format = "file"),
  tar_target(koppi_length_file, "data/KoppiLengthAssessmentWorkshop_final.csv", format = "file"),
  
  # read in data
  tar_target(rivers, list(
    delger = as.matrix(read_csv(delger_length_file, show_col_types = FALSE)),
    onon = as.matrix(read_csv(onon_length_file, show_col_types = FALSE)),
    eguur = as.matrix(read_csv(eguur_length_file, show_col_types = FALSE)),
    tugur = as.matrix(read_csv(tugur_length_file, show_col_types = FALSE)),
    karibetsu = as.matrix(read_csv(karibetsu_length_file, show_col_types = FALSE)),
    koppi = as.matrix(read_csv(koppi_length_file, show_col_types = FALSE))
  )),
  
  
  # get n random draws of LInf and K to use in place of VB fit posteriors for Delger and Onon populations
  # using values from Jensen et al. 2009 doi:10.1139/F09-109
  # Linf: mean=146, SD=14.9
  # K: mean=0.09, SD=0.0065
  tar_target(delger_onon_params, {
    linf_mean <- log_mean(146, 14.9)
    linf_sd <- log_sd(146, 14.9)
    
    k_mean <- log_mean(0.09, 0.0065)
    k_sd <- log_sd(0.09, 0.0065)
    
    rows <- nrow(tugur_post)
    tibble(linf = rlnorm(rows, linf_mean, linf_sd),
                                          k = rlnorm(rows, k_mean, k_sd))
    }),
  
  # get n random draws of M from Fishlife distributions to attach to LInf and K posteriors/draws
  tar_target(itou_m, {
    perryi_m_mean <- fishlife_values %>% 
      filter(species == "Parahucho perryi") %>% 
      pull(log.m_mean)
    perryi_m_sd <- fishlife_values %>% 
      filter(species == "Parahucho perryi") %>% 
      pull(log.m_sd)
    rows <- nrow(itou_post)
    tibble(m = rlnorm(rows, perryi_m_mean, perryi_m_sd))
    }),
  tar_target(taimen_m, {
    taimen_m_mean <- fishlife_values %>% 
      filter(species == "Hucho taimen") %>% 
      pull(log.m_mean)
    taimen_m_sd <- fishlife_values %>% 
      filter(species == "Hucho taimen") %>% 
      pull(log.m_sd)
    rows <- nrow(tugur_post)
    tibble(m = rlnorm(rows, taimen_m_mean, taimen_m_sd))
    }),
  
  # combine LInf, K, and M for each population
  tar_target(delger_params, delger_onon_params %>% 
               bind_cols(taimen_m) %>% 
               mutate(pop = "delger",
                      species = "Hucho taimen")),
  tar_target(onon_params, delger_onon_params %>% 
               bind_cols(taimen_m) %>% 
               mutate(pop = "onon",
                      species = "Hucho taimen")),
  tar_target(tugur_params, tugur_post %>% 
               bind_cols(taimen_m) %>% 
               mutate(pop = "tugur",
                      species = "Hucho taimen")),
  tar_target(eguur_params, eguur_post %>% 
               bind_cols(taimen_m) %>% 
               mutate(pop = "eguur",
                      species = "Hucho taimen")),
  tar_target(koppi_params, itou_post %>% 
               bind_cols(itou_m) %>% 
               mutate(pop = "koppi",
                      species = "Parahucho perryi")),
  tar_target(karibetsu_params, itou_post %>% 
               bind_cols(itou_m) %>% 
               mutate(pop = "karibetsu",
                      species = "Parahucho perryi")),
  
  # combine all params, used in MonteCarlo simulation
  tar_target(params_mc, bind_rows(tugur_params, koppi_params, karibetsu_params, eguur_params, delger_params, onon_params)),
  
  # get list of params to use in single LBSPR model fit for each population
  # can take the mean of the draws/posteriors of each param so that we don't have to build a new data frame with values from scratch
  # these have been double checked with original values in Fishlife and Jensen et al. 2009
  tar_target(lbspr_params, params_mc %>% 
               summarise(across(c(linf, k, m), list(mean = ~signif(mean(.), 3), sd = ~signif(sd(.), 3))), .by = pop) %>% 
               rename(river = pop) %>% 
               mutate(river = str_to_title(river),
                      river = if_else(river == "Eguur", "Eg-Uur", river),
                      # set order of rivers
                      river = factor(river, levels = c("Tugur", "Eg-Uur", "Delger", "Onon", "Karibetsu", "Koppi")))),
  
  # prep TPars object for LBSPR model with specified m, k, LInf
  tar_target(create_tpars, function(species, m, k, linf){
    TPars <- new("LB_pars", verbose = FALSE)
    
    # Cosmetic
    TPars@Species <- species
    TPars@L_units <- "cm" # units for fish length measure
    
    # Biology
    TPars@Linf <- linf  # von Bertalanffy asymptotic length
    TPars@CVLinf <- 0.2
    TPars@L50 <- 50 # Length @ 50% maturity - 50, approx from growth curve at age 4
    TPars@L95 <- 75 # Length @ 95% maturity - 75, approx from growth curve at age 7
    TPars@MK <- m / k
    TPars@Walpha <- 0.0047 # a from vB weight equation - Jensen et al. 2009
    TPars@Wbeta <- 3.1 # b from vB weight equation - Jensen et al. 2009
    
    TPars
  }),
  
  # create lb_lengths from a data matrix
  # lbspr package initialization for lb_length object claims to handle matrices, but doesn't
  # this uses the code from the initialization that was supposed to handle matrices
  # we can't be reading in files every time, way too slow
  tar_target(create_lvec, function(LB_pars, dat){
    lvec = new("LB_lengths", verbose = FALSE)
    dat <- as.data.frame(dat)
    lvec@Years <- as.numeric(names(dat[1:ncol(dat)]))
    lvec@NYears <- ncol(dat)
    LB_pars@BinMax <- ceiling((max(LB_pars@Linf * 1.1, 1.1 * max(dat, na.rm=TRUE)))/0.5)*0.5
    LB_pars@BinMin <- floor((min(dat, na.rm=TRUE) * 0.9)/0.5)*0.5
    LB_pars@BinWidth <- floor((1/20 * LB_pars@BinMax)/5)*5
    LBins <- seq(from=LB_pars@BinMin, by=LB_pars@BinWidth, to=LB_pars@BinMax)
    lvec@LData <- sapply(1:ncol(dat), function(x) hist(dat[,x], breaks=LBins, plot=FALSE)$counts)
    lvec@LMids <- hist(dat[,1], breaks=LBins, plot=FALSE)$mids
    if (length(LB_pars@L_units) > 0) lvec@L_units <- LB_pars@L_units
    return(lvec)
  })
  
)



