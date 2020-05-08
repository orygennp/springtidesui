sp_data_sf <- alt_results_ls[[1]]
basic_cols_chr_vec <- names(sp_data_sf)[names(sp_data_sf) %>% startsWith(c("pop"))|
                                          names(sp_data_sf) %>% startsWith(c("t0_2"))|
                                          names(sp_data_sf) %>% startsWith(c("ti_"))|
                                          names(sp_data_sf) %>% startsWith(c("tx_2"))|
                                          names(sp_data_sf) %>% startsWith(c("delta_2"))|
                                          names(sp_data_sf) %>% startsWith(c("SA2_MAIN16"))]
sp_data_sf <- sp_data_sf %>%
  dplyr::select(tidyselect::all_of(basic_cols_chr_vec))
par_str_tb <- pa_x_params_ls$input_ls$env_str_par_tb
shock_var_chr_vec <- c(paste0("RR_shock_",
                              2011:2031,
                              "0701",
                              "_") %>%
                         paste0(c("00_11",
                                  "12_25",
                                  "26_99")))
shock_rr_dbl_vec <- rep(1.25, length(shock_var_chr_vec))
par_str_tb <- par_str_tb %>%
  dplyr::add_row(param_name = shock_var_chr_vec,
                 deter_val = shock_rr_dbl_vec,
                 distribution = rep("pert", times = length(shock_var_chr_vec)),
                 dist_param_1 = shock_rr_dbl_vec,
                 dist_param_2 = shock_rr_dbl_vec*0.75,
                 dist_param_3 = shock_rr_dbl_vec*1.50,
                 dist_param_4 = rep(3, times = length(shock_var_chr_vec)),
                 use_in = "base")
bgd_var_chr_vec <- c(paste0("RR_bgd_",
                              2011:2031,
                            "0701",
                              "_") %>%
                         paste0(c("00_11",
                                  "12_25",
                                  "26_99")))
bgd_rr_dbl_vec <- rep(1, length(bgd_var_chr_vec))
par_str_tb <- par_str_tb %>%
  dplyr::add_row(param_name = bgd_var_chr_vec,
                 deter_val = bgd_rr_dbl_vec,
                 distribution = rep("pert", times = length(bgd_rr_dbl_vec)),
                 dist_param_1 = bgd_rr_dbl_vec,
                 dist_param_2 = bgd_rr_dbl_vec*0.95,
                 dist_param_3 = bgd_rr_dbl_vec*1.1,
                 dist_param_4 = rep(3, times = length(bgd_rr_dbl_vec)),
                 use_in = "base")

par_str_list <- instantiate_env_struc_par_all(par_str_tb)
par_vals_tb  <- purrr::map_dfr(1:length(par_str_list),
                                ~ genValueFromDist(par_str_list[[.x]], 10))
param_tb <- par_vals_tb
#pa_x_params_ls$sim_data_r4@st_envir@par_vals


