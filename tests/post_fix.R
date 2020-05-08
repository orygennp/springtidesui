basic_cols_chr_vec <- names(sp_data_sf)[names(sp_data_sf) %>% startsWith(c("pop"))|
                                          names(sp_data_sf) %>% startsWith(c("t0_2"))|
                                          names(sp_data_sf) %>% startsWith(c("ti_"))|
                                          names(sp_data_sf) %>% startsWith(c("tx_2"))|
                                          names(sp_data_sf) %>% startsWith(c("delta_2"))|
                                          names(sp_data_sf) %>% startsWith(c("SA2_MAIN16"))]
sp_data_sf <- alt_results_ls[[1]]
sp_data_sf <- sp_data_sf %>%
  dplyr::select(tidyselect::all_of(basic_cols_chr_vec))
