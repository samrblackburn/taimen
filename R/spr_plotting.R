

lbspr_plot_targets <- list(
  
  # format and order river names
  tar_target(rivers_renamed, rivers %>% 
               map(as.data.frame) %>% 
               list_rbind(names_to = "river") %>% 
               mutate(river = str_to_title(river),
                      river = if_else(river == "Eguur", "Eg-Uur", river),
                      # set order of rivers
                      river = factor(river, levels = c("Tugur", "Eg-Uur", "Delger", "Onon", "Karibetsu", "Koppi")))),
  
  # get count of number of fish per year per population
  tar_target(river_counts, rivers_renamed %>%
               pivot_longer(where(is.numeric), names_to = "year", values_to = "length") %>%
               filter(!is.na(length)) %>%
               summarise(n = n(), .by = c(river, year)) %>% 
               rename(Year = year)),
  
  
  #run LBSPR model for a given population
  tar_target(run_lbspr, function(name) {
    data <- filter(rivers_renamed, river == name) %>%
      select(-c(river, where(~all(is.na(.)))))
    species <-  if_else(name %in% c("karibetsu", "koppi"), "Parahucho perryi", "Hucho taimen")
    pars <- filter(lbspr_params, river == name)
    m <- pars$m[1]
    k <- pars$k[1]
    linf <- pars$linf[1]
    tpars <- create_tpars(species, m, k, linf)
    Lvec <- create_lvec(tpars, data)
    LBSPRfit(tpars, Lvec, verbose = FALSE)
  }),
  
  # actually run LBSPR for each population
  tar_target(tugur_fit, run_lbspr("Tugur")),
  tar_target(eguur_fit, run_lbspr("Eg-Uur")),
  tar_target(onon_fit, run_lbspr("Onon")),
  tar_target(delger_fit, run_lbspr("Delger")),
  tar_target(koppi_fit, run_lbspr("Koppi")),
  tar_target(karibetsu_fit, run_lbspr("Karibetsu")),
  
  # plot LBSPR model fits for each population
  tar_target(tugur_lbspr_plot, plotSize(tugur_fit, counts = filter(river_counts, river == "Tugur"))),
  tar_target(eguur_lbspr_plot, plotSize(eguur_fit, counts = filter(river_counts, river == "Eg-Uur"))),
  tar_target(onon_lbspr_plot, plotSize(onon_fit, counts = filter(river_counts, river == "Onon"))),
  tar_target(delger_lbspr_plot, plotSize(delger_fit, counts = filter(river_counts, river == "Delger"))),
  tar_target(koppi_lbspr_plot, plotSize(koppi_fit, counts = filter(river_counts, river == "Koppi"))),
  tar_target(karibetsu_lbspr_plot, plotSize(karibetsu_fit, counts = filter(river_counts, river == "Karibetsu"))),
  
  # plot maturity and selectivity curves for each population
  tar_target(tugur_selmat_plot, plotMat(tugur_fit) +
    labs(tag = "A") +
    theme(plot.tag = element_text(face = "bold", size = 14),
          plot.tag.position = c(0.18, 0.95))),
  tar_target(eguur_selmat_plot, plotMat(eguur_fit) +
    labs(tag = "B") +
    theme(plot.tag = element_text(face = "bold", size = 14),
          plot.tag.position = c(0.18, 0.95))),
  tar_target(delger_selmat_plot, plotMat(delger_fit) +
    labs(tag = "C") +
    theme(plot.tag = element_text(face = "bold", size = 14),
          plot.tag.position = c(0.18, 0.95))),
  tar_target(onon_selmat_plot, plotMat(onon_fit) +
    labs(tag = "D") +
    theme(plot.tag = element_text(face = "bold", size = 14),
          plot.tag.position = c(0.18, 0.95))),
  tar_target(koppi_selmat_plot, plotMat(koppi_fit) +
    labs(tag = "E") +
    theme(plot.tag = element_text(face = "bold", size = 14),
          plot.tag.position = c(0.18, 0.95))),
  tar_target(kari_selmat_plot, plotMat(karibetsu_fit) +
    labs(tag = "F") +
    theme(plot.tag = element_text(face = "bold", size = 14),
          plot.tag.position = c(0.18, 0.95))),
  
  # combine mat figures
  tar_target(selmat_plot, grid.arrange(tugur_selmat_plot, eguur_selmat_plot, delger_selmat_plot, onon_selmat_plot, koppi_selmat_plot, kari_selmat_plot, ncol = 3))
  
  
)


