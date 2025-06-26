

vb_fit_targets <- list(
  
  #data files
  tar_target(itou_age_length_file, "data/itou_age_length.csv", format = "file"),
  tar_target(eguur_age_length_file, "data/eguur_age_length.csv", format = "file"),
  tar_target(tugur_age_length_08_file, "data/tugur_age_length_2008.csv", format = "file"),
  tar_target(tugur_age_length_17_file, "data/tugur_age_length_2017.csv", format = "file"),
  
  # read in data files
  tar_target(itou_age_length, read_csv(itou_age_length_file)),
  tar_target(eguur_age_length, read_csv(eguur_age_length_file)),
  tar_target(tugur_age_length, bind_rows(read_csv(tugur_age_length_08_file), read_csv(tugur_age_length_17_file))),
  
  # priors for itou - uninformative
  tar_target(itou_priors, prior(normal(0,500), nlpar="Linf", lb=0) +
               prior(normal(0,10), nlpar="K", lb=0) +
               prior(normal( 0,10), nlpar="t0") +
               prior(student_t(3,0,40), class=sigma)),
  
  # priors for Tugur and Eguur, form Jensen et al. 2009 doi:10.1139/F09-109 
  tar_target(taimen_priors, prior(normal(146,14.9), nlpar="Linf", lb=0) +
               prior(normal(0.09,0.0065), nlpar="K", lb=0) +
               prior(normal( 0,10), nlpar="t0") +
               prior(student_t(3,0,40), class=sigma)),
  
  # initial values for modeling
  tar_target(inits, function() list(
    Linf=runif(1, 50, 500),
    K=runif(1, 0.01, 2.00),
    t0=rnorm(1, 0, 0.5)
  )),
  
  # function wrapper to run model
  tar_target(vb_fit, function(data, priors) {
    brm(brmsformula(length ~ Linf * (1 - exp(-K * (age - t0))),
                    Linf ~ 1, K ~ 1, t0 ~ 1, nl=TRUE),            
        family = gaussian(),  
        data = data,         
        prior = priors,      
        init = inits,     
        chains = 3,       
        cores = 3,             
        iter = 3000,          
        warmup = 1000,        
        control = list(adapt_delta=0.80, max_treedepth=15))
  }),
  
  ## fit model to populations
  tar_target(itou_vb_fit, vb_fit(itou_age_length, itou_priors)),
  tar_target(eguur_vb_fit, vb_fit(eguur_age_length, taimen_priors)),
  tar_target(tugur_vb_fit, vb_fit(tugur_age_length, taimen_priors)),
  
  ## get posteriors from each fit - only interested in LInf and K
  tar_target(itou_post, as_draws_df(itou_vb_fit) %>% 
               select(linf = b_Linf_Intercept, k = b_K_Intercept)),
  tar_target(eguur_post, as_draws_df(eguur_vb_fit) %>% 
               select(linf = b_Linf_Intercept, k = b_K_Intercept)),
  tar_target(tugur_post, as_draws_df(tugur_vb_fit) %>% 
               select(linf = b_Linf_Intercept, k = b_K_Intercept)),
  
  ## plots of model fit
  tar_target(itou_vb_fit_plot, itou_age_length %>%
               add_predicted_draws(itou_vb_fit) %>%  # adding the posterior distribution with tidybayes
               ggplot(aes(x=age)) +  
               stat_lineribbon(aes(y=.prediction), .width=c(.95, .80, .50),  # regression line and CI
                               alpha=0.5, colour="black") +
               geom_point(aes(y = length), color="darkblue", size=2) +   # raw data
               scale_fill_brewer(name = "CI", palette="Greys") +
               scale_y_continuous(name = "Length (cm)", limits = c(0, 120), breaks = c(0,30,60,90,120)) + 
               scale_x_continuous(name = "", limits = c(0, 30)) +
               labs(tag = "A") +
               # ggtitle("Koppi and Karibetsu") +
               theme_bw() +
               theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.position = "inside",
                     legend.position.inside = c(0.85, 0.25),
                     plot.tag = element_text(face = "bold", size = 14),
                     plot.tag.position = c(0.2, 0.9))),
  tar_target(eguur_vb_fit_plot, eguur_age_length %>%
               add_predicted_draws(eguur_vb_fit) %>%  # adding the posterior distribution with tidybayes
               ggplot(aes(x=age)) +  
               stat_lineribbon(aes(y=.prediction), .width=c(.95, .80, .50),  # regression line and CI
                               alpha=0.5, colour="black") +
               geom_point(aes(y = length), color="darkblue", size=2) +   # raw data
               scale_fill_brewer(name = "CI", palette="Greys") +
               scale_y_continuous(name = NULL, limits = c(0, 160), breaks = c(0,30,60,90,120,150)) + 
               scale_x_continuous(name = "Age (years)", limits = c(0, 30)) +
               labs(tag = "B") +
               # ggtitle("Eg-Uur") +
               theme_bw() +
               theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.position = "none",
                     #legend.position.inside = c(0.85, 0.25),
                     plot.tag = element_text(face = "bold", size = 14),
                     plot.tag.position = c(0.2, 0.9))),
  tar_target(tugur_vb_fit_plot, tugur_age_length %>%
               add_predicted_draws(tugur_vb_fit) %>%  # adding the posterior distribution with tidybayes
               ggplot(aes(x=age)) +  
               stat_lineribbon(aes(y=.prediction), .width=c(.95, .80, .50),  # regression line and CI
                               alpha=0.5, colour="black") +
               geom_point(aes(y = length), color="darkblue", size=2) +   # raw data
               scale_fill_brewer(name = "CI", palette="Greys") +
               scale_y_continuous(name = NULL, limits = c(0, 160), breaks = c(0,30,60,90,120,150)) + 
               scale_x_continuous(name = "", limits = c(0, 30)) +
               labs(tag = "C") +
               # ggtitle("Tugur") +
               theme_bw() +
               theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.position = "none",
                     # legend.position.inside = c(0.85, 0.25),
                     plot.tag = element_text(face = "bold", size = 14),
                     plot.tag.position = c(0.2, 0.9))),
  
  # combined plot figure
  # save out with ggsave("VB fits.png", tar_read(vb_fit_plots), width = 10, height = 3, units = "in")
  tar_target(vb_fit_plots, grid.arrange(itou_vb_fit_plot, eguur_vb_fit_plot, tugur_vb_fit_plot, ncol = 3))
  
)


