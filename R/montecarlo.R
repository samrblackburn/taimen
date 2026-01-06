montecarlo_targets <- list(
  # run lbspr model with specified values
  # format result with key values
  tar_target(run_lbspr_mc, function(name, species, m, k, linf, data) {
    name_filt <- if_else(name == "eguur", "Eg-Uur", str_to_title(name))
    linf_cv <- lbspr_params %>%
      filter(river == name_filt) %>%
      pull(linf_cv)

    tpars <- create_tpars(species, m, k, linf, linf_cv)

    Lvec <- create_lvec(tpars, data)

    SPR_est <- LBSPRfit(tpars, Lvec, verbose = FALSE)

    tibble(
      river = name,
      year = SPR_est@Years,
      MK = SPR_est@MK,
      LInf = SPR_est@Linf,
      SL50 = SPR_est@SL50,
      SL95 = SPR_est@SL95,
      FM = SPR_est@FM,
      SPR = SPR_est@SPR,
      SL50.Var = SPR_est@Vars[, "SL50"],
      SL95.Var = SPR_est@Vars[, "SL95"],
      FM.Var = SPR_est@Vars[, "FM"],
      SPR.Var = SPR_est@Vars[, "SPR"]
    )
  }),

  # run model a specified number of times and bind together results
  # pulls in pre-filled df with random pulls of params and drops rows after n
  tar_target(mc_lbspr, function(data, name, n = 100) {
    # get relevant params for populations
    pars <- params_mc %>%
      filter(pop == name) %>%
      head(n) %>%
      select(name = pop, species, m, k, linf)

    # iterate over M, K, LInf values and fit model, then bind together
    # setting seed for furrr because LBSPR generates random numbers during model fit
    future_pmap(
      pars,
      run_lbspr_mc,
      data,
      .options = furrr_options(seed = TRUE)
    ) %>%
      list_rbind()
  }),

  #run simulation for all rivers and return output bound together
  tar_target(bind_mc_lbspr, function(rivers, n = 100) {
    #iterate over each river , run simulation, and bind together output
    imap(rivers, ~ mc_lbspr(.x, .y, n)) %>%
      list_rbind()
  }),

  # actually run the simulation - change the number to run more simulations
  # 100 simulations takes about less than a minute on my computer
  # can't be more than number of bayesian iterations; currently 6,000
  # may spit out warnings about package availability, this seems to be a bug: https://github.com/Calvagone/campsis/issues/157
  tar_target(mc_output, bind_mc_lbspr(rivers, 100))
)
