add_shock_RR_tb <- function(res_tb,
                            RR_tb,
                            starts_with_chr_vec){
  res_tb %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(starts_with_chr_vec[1])), #t0_prev
                     .funs = list(s1_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scenario 1_RR`)*.,
                                  s2_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scenario 2_RR`) *.,
                                  s1_i1_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scen. 1, int. 1_RR`) *.,
                                  s1_i2_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scen. 1, int. 2_RR`) *.,
                                  s1_i3_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scen. 1, int. 3_RR`) *.,
                                  s1_i4_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scen. 1, int. 4_RR`) *.,
                                  s1_i5_excess = ~ RR_tb %>%
                                    dplyr::slice(1) %>% dplyr::pull(`Scen. 1, int. 5_RR`) *.)

    ) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(starts_with_chr_vec[2])), #tx_prev
                     .funs = list(s1_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scenario 1_RR`)*.,
                                  s2_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scenario 2_RR`) *.,
                                  s1_i1_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scen. 1, int. 1_RR`) *.,
                                  s1_i2_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scen. 1, int. 2_RR`) *.,
                                  s1_i3_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scen. 1, int. 3_RR`) *.,
                                  s1_i4_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scen. 1, int. 4_RR`) *.,
                                  s1_i5_excess = ~ RR_tb %>%
                                    dplyr::slice(2) %>% dplyr::pull(`Scen. 1, int. 5_RR`) *.)

    ) %>% dplyr::rename_at(
      dplyr::vars(ends_with("_excess")), function(x) { toupper(x) }
    )
  }

## SPECIFY DATA PATHS
r_data_path_chr <- "C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"
project_data_path_chr <- r_data_path_chr %>% stringr::str_replace("R_Format","Project/COVID19")
path_to_K10_data_chr <- paste0(project_data_path_chr,"/Copy of Psychological Distress - North Coast outputs.xlsx")
pa_dir_chr <- "Victoria_STE"
## IMPORT SYDNEY DATA
start_date_dtm <- lubridate::ymd_hms("2011-01-01 00:00:00",
                                     tz = "Australia/Melbourne")
distress_tbs_ls <- import_model_output_tb(path_to_K10_data_chr = path_to_K10_data_chr,
                                          range_ls = range_ls,
                                          start_date_dtm = start_date_dtm)
scenarios_tb <- readxl::read_excel(path_to_K10_data_chr,
                                   range = "W3:X9",
                                   col_names = F) %>%
  dplyr::rename(Scenario = ...1,
                Description = ...2)

## IMPORT PRE-COVID SPRINGTIDES RUNS
pa_x_params_ls_path_chr <- paste0(project_data_path_chr,
                                  "/",
                                  pa_dir_chr,
                                  "/output_params_ls.rds")
output_params_ls <- readRDS(pa_x_params_ls_path_chr)
## SPECIFY DATES OF SPRINGTIDES EPI SOURCES
epi_data_dtm_ls <- list(age_12_17 = lubridate::ymd_hms("2014-01-01 00:00:00",
                                                       tz = "Australia/Melbourne"),
                        age_18_25 = lubridate::ymd_hms("2011-01-01 00:00:00", # 2007 not available
                                                       tz = "Australia/Melbourne"))
## GET BASE CASE RESULTS
sp_data_sf <- output_params_ls$sim_results_ls[[1]]
basic_cols_chr_vec <- names(sp_data_sf)[names(sp_data_sf) %>% startsWith(c("pop"))|
                                          names(sp_data_sf) %>% startsWith(c("t0_2"))|
                                          names(sp_data_sf) %>% startsWith(c("ti_"))|
                                          names(sp_data_sf) %>% startsWith(c("tx_2"))|
                                          names(sp_data_sf) %>% startsWith(c("delta_2"))|
                                          names(sp_data_sf) %>% startsWith(c("SA2_MAIN16"))]
sp_data_sf <- sp_data_sf %>%
  dplyr::select(tidyselect::all_of(basic_cols_chr_vec))
### UPDATE_PARAMS_LS
pa_x_params_ls <- output_params_ls
distress_tb <- distress_tbs_ls$youth_distress_tb
prev_params_dbl_vec <- purrr::map_dbl(epi_data_dtm_ls,
                                      ~ which.min(abs(distress_tb$week_starting_dtm-.x)))

prev_data_dbl_vec <- distress_tb %>%
  dplyr::slice(prev_params_dbl_vec) %>%
  dplyr::pull(Baseline)
#ti_col_chr_vec <- names(pa_x_params_ls$sim_results_ls[[1]])[names(pa_x_params_ls$sim_results_ls[[1]]) %>% startsWith("ti_")] %>% stringr::str_sub(end=-6) %>% unique()
dates_nms_chr_vec <- get_var_pfx_chr_vec(sp_data_sf = sp_data_sf,
                                         stat = "popl") %>%
  stringr::str_sub(end = -4) %>%
  unique() %>%
  paste0("_RR")
dtm_vec <- tf_vars_to_dtm_vec(dates_nms_chr_vec,
                                  tz = "Australia/Melbourne")
dates_idx_dbl_vec <- purrr::map_dbl(dtm_vec,
                                    ~ which.min(abs(distress_tb$week_starting_dtm-.x)))
## Add Interval Times
RR_tb <- distress_tb %>% dplyr::slice(dates_idx_dbl_vec)
bgd_RR_lup <- make_base_case_lup(pa_x_params_ls = pa_x_params_ls,
                                           dates_idx_dbl_vec = dates_idx_dbl_vec,
                                           dates_nms_chr_vec = dates_nms_chr_vec,
                                           RR_tb = RR_tb,
                                           prev_data_dbl_vec = prev_data_dbl_vec)

alt_results_ls <- pa_x_params_ls$sim_results_ls %>%
  purrr::map(~ {
    .x %>%
      add_shock_RR_tb(RR_tb = RR_tb,
                      starts_with_chr_vec = c("t0_prev","tx_prev")) %>%
      add_alt_model_RR_tb(col_pfx_chr = "t0_prev",
                          time_RR_chr = "t0_RR",
                          RR_lup = bgd_RR_lup)  %>%
      add_alt_model_RR_tb(col_pfx_chr = "tx_prev",
                          time_RR_chr = "tx_RR",
                          RR_lup = bgd_RR_lup) %>%
      add_shock_RR_tb(RR_tb = RR_tb,
                      starts_with_chr_vec = c("SYD_BC_t0_prev","SYD_BC_tx_prev"))

  }
  )


####


shock_var_chr_vec <- c(paste0("RR_shock_",
                              2011:2031,
                              "0701",
                              "_") %>%
                         paste0(c("00_11",
                                  "12_25",
                                  "26_99")))
shock_rr_dbl_vec <- rep(1.25, length(shock_var_chr_vec))
par_str_tb <- pa_x_params_ls$input_ls$env_str_par_tb
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


