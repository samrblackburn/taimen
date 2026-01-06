mc_plot_targets <- list(
  # pulls n vals from distributions of F/M and SPR from MC simulation output
  tar_target(
    pull_output_dist,
    function(river, year, log.FM, log.FM.SD, log.SPR, log.SPR.SD, n) {
      tibble(
        river = rep(river, n),
        year = rep(year, n),
        FM = rlnorm(n, log.FM, log.FM.SD),
        SPR = rlnorm(n, log.SPR, log.SPR.SD)
      )
    }
  ),

  # wrapper to convert MC output distributions to be lognormal distributed
  # then pull values from distributions and combine
  tar_target(expand_output, function(df, n = 30) {
    df_log <- df %>%
      mutate(
        FM = if_else(FM == 0, 1e-3, FM), #make F/M estimated to be zero instead very very small to that log is possible
        log.FM = log_mean(FM, sqrt(FM.Var)),
        log.FM.SD = log_sd(FM, sqrt(FM.Var)),
        log.SPR = log_mean(SPR, sqrt(SPR.Var)),
        log.SPR.SD = log_sd(SPR, sqrt(SPR.Var))
      ) %>%
      select(river, year, log.FM, log.FM.SD, log.SPR, log.SPR.SD)

    pmap(df_log, pull_output_dist, n) %>%
      list_rbind()
  }),

  # format mc simulation output for plotting
  tar_target(
    mc_plot_df,
    expand_output(mc_output) %>%
      mutate(
        year = as_factor(year),
        river = str_to_title(river),
        river = if_else(river == "Eguur", "Eg-Uur", river),
        # set order of rivers
        river = factor(
          river,
          levels = c(
            "Tugur",
            "Eg-Uur",
            "Delgermoron",
            "Onon",
            "Karibetsu",
            "Koppi"
          )
        )
      ) %>%
      mutate(log.SPR = log(SPR), log.FM = log(FM)) %>%
      group_by(river, year) %>%
      summarise(
        SPR_estimate = mean(SPR, na.rm = TRUE),
        SPR_sd = sd(SPR, na.rm = TRUE),
        FM_estimate = mean(FM, na.rm = TRUE),
        FM_sd = sd(FM, na.rm = TRUE),
        log.SPR_estimate = mean(log.SPR, na.rm = TRUE),
        log.SPR_sd = sd(log.SPR, na.rm = TRUE),
        log.FM_estimate = mean(log.FM, na.rm = TRUE),
        log.FM_sd = sd(log.FM, na.rm = TRUE),
        n = n()
      ) %>%
      mutate(
        SPR_lower = SPR_estimate - 1.96 * SPR_sd,
        SPR_upper = SPR_estimate + 1.96 * SPR_sd,
        SPR_upper = if_else(SPR_upper > 1, 1, SPR_upper), # don't allow SPR values over 1
        FM_lower = FM_estimate - 1.96 * FM_sd,
        FM_upper = FM_estimate + 1.96 * FM_sd,
        log.SPR_lower = exp(log.SPR_estimate - 1.96 * log.SPR_sd),
        log.SPR_upper = exp(log.SPR_estimate + 1.96 * log.SPR_sd),
        log.SPR_upper = if_else(log.SPR_upper > 1, 1, log.SPR_upper),
        log.FM_lower = exp(log.FM_estimate - 1.96 * log.FM_sd),
        log.FM_upper = exp(log.FM_estimate + 1.96 * log.FM_sd)
      ) %>%
      ungroup() %>%
      mutate(
        species = if_else(
          river %in% c("Karibetsu", "Koppi"),
          "Parahucho perryi",
          "Hucho taimen"
        )
      )
  ),

  # plot of Spawning Potential Ration and F/M for mc simulation output
  # save out with
  # ggsave("MC SPR.png", tar_read(mc_spr_plot), width = 10, height = 7, units = "in")
  # ggsave("MC FM.png", tar_read(mc_fm_plot), width = 10, height = 7, units = "in")
  tar_target(
    mc_spr_plot,
    ggplot(data = mc_plot_df, aes(x = year)) +
      facet_wrap(~river) +
      geom_hline(yintercept = 0.4, linetype = "dashed", alpha = 0.5) +
      geom_hline(yintercept = 0.2, linetype = "dashed", alpha = 0.5) +
      geom_segment(aes(
        y = log.SPR_lower,
        yend = log.SPR_upper,
        color = species
      )) +
      geom_point(aes(y = SPR_estimate, color = species)) +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = "Spawning Potential Ratio") +
      scale_color_manual(name = NULL, values = c("#228833", "#aa3377")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = -45, hjust = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  ),
  tar_target(
    mc_fm_plot,
    ggplot(data = mc_plot_df, aes(x = year)) +
      facet_wrap(~river, scales = "free_y") +
      geom_hline(yintercept = 0.87, linetype = "dashed", alpha = 0.5) +
      geom_segment(aes(
        y = log.FM_lower,
        yend = log.FM_upper,
        color = species
      )) +
      geom_point(aes(y = FM_estimate, color = species)) +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = "F/M Ratio") +
      scale_color_manual(name = NULL, values = c("#228833", "#aa3377")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = -45, hjust = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  )
)
