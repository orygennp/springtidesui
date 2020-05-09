make_shock_RR_tb <- function(RR_tb,
                             var_chr,
                             dates_nms_chr_vec){
  tb <- purrr::map2_dfc(1:nrow(RR_tb),
                        dates_nms_chr_vec,
                        ~ tibble::tibble(!!rlang::sym(.y) := (RR_tb %>%
                                                                dplyr::slice(.x) %>%
                                                                dplyr::pull(!!rlang::sym(var_chr)))
                        )) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Dtm")
  tb$Dtm <- tf_vars_to_dtm_vec(tb$Dtm,
                                starts_dbl = 4,
                                tz = "Australia/Melbourne")
  tb
}
make_bgd_RR_lup <- function(pa_x_params_ls,
                            dates_idx_dbl_vec, # Named
                            dates_nms_chr_vec,
                            RR_tb,
                            prev_data_dbl_vec,
                            age_range_chr_vec){

  bgd_RR_lup <- purrr::reduce(1:length(dates_idx_dbl_vec),
                .init = tibble::tibble(age_range_chr = age_range_chr_vec),
                ~ {
                  row_idx_dbl = .y
                  dplyr::mutate(.x,!!rlang::sym(dates_nms_chr_vec[.y]) := purrr::map_dbl(age_range_chr,
                                                                                         ~ {
                                                                                           idx_dbl <- which(age_range_chr_vec==.x)
                                                                                           1+(RR_tb %>%
                                                                                                       dplyr::slice(row_idx_dbl) %>%
                                                                                                dplyr::pull(Baseline)-prev_data_dbl_vec[idx_dbl])/prev_data_dbl_vec[idx_dbl]

                                                                                         })
                  )
                })
  purrr::map_dfr(1:nrow(bgd_RR_lup),
                 ~bgd_RR_lup[.x,2:ncol(bgd_RR_lup)] %>%
                   t() %>%
                   as.data.frame() %>%
                   tibble::rownames_to_column("date_and_age_chr") %>%
                   tibble::as_tibble() %>%
                   dplyr::mutate(date_and_age_chr = paste0(date_and_age_chr %>% stringr::str_sub(end=-4),
                                                           "_",
                                                           bgd_RR_lup[.x,1] %>% stringr::str_sub(start=-5))))
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
distress_ages_ls <- list(c("12_25"),
                         c("00_11","26_99"))
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
epi_data_dtm_ls <- list(age_04_17 = lubridate::ymd_hms("2014-01-01 00:00:00",
                                                       tz = "Australia/Melbourne"),
                        age_18_99 = lubridate::ymd_hms("2011-01-01 00:00:00", # 2007 not available
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
## TEMPORAL DATA
dates_nms_chr_vec <- get_var_pfx_chr_vec(sp_data_sf = sp_data_sf,
                                         stat = "popl") %>%
  stringr::str_sub(end = -4) %>%
  unique() %>%
  paste0("_RR")
dtm_vec <- tf_vars_to_dtm_vec(dates_nms_chr_vec,
                                  tz = "Australia/Melbourne")

## Structural parameters
par_str_tb <- purrr::reduce(1:length(distress_tbs_ls),
                            .init = pa_x_params_ls$input_ls$env_str_par_tb,
                            ~ {
                              par_str_tb <- .x
                              age_range_chr_vec <- distress_ages_ls[[.y]]
                              distress_tb <- distress_tbs_ls[[.y]]
                              prev_params_dbl_vec <- purrr::map_dbl(epi_data_dtm_ls,
                                                                    ~ which.min(abs(distress_tb$week_starting_dtm-.x)))
                              prev_data_dbl_vec <- distress_tb %>%
                                dplyr::slice(prev_params_dbl_vec) %>%
                                dplyr::pull(Baseline)
                              dates_idx_dbl_vec <- purrr::map_dbl(dtm_vec,
                                                                  ~ which.min(abs(distress_tb$week_starting_dtm-.x)))
                              RR_tb <- distress_tb %>% dplyr::slice(dates_idx_dbl_vec)
                              names_chr_vec <- purrr::map(age_range_chr_vec,
                                                              ~ {
                                                                age_range_chr <- .x
                                                                names_chr_vec <- names(epi_data_dtm_ls) %>%
                                                                  purrr::map_chr(~ paste0(.x %>% stringr::str_sub(end=4),
                                                                                          .x %>% stringr::str_sub(start=5,end=6) %>%
                                                                                            as.numeric() %>% max(age_range_chr %>%
                                                                                                                   stringr::str_sub(end=2) %>%
                                                                                                                   as.numeric()) %>%
                                                                                            paste0(ifelse(.<10,"0",""),.),
                                                                                          "_",
                                                                                          .x %>% stringr::str_sub(start=8,end=9) %>%
                                                                                            as.numeric() %>% min(age_range_chr %>%
                                                                                                                   stringr::str_sub(start=4) %>%
                                                                                                                   as.numeric()) %>%
                                                                                            paste0(ifelse(.<10,"0",""),.)))
                                                                names_chr_vec[purrr::map_lgl(names_chr_vec,
                                                                                             ~ stringr::str_sub(.x,start=5,end=6)< stringr::str_sub(.x,start=8,end=9))]
                                                              }) %>%
                                unlist()
                              ### HERE
                              ### HERE
                              bgd_RR_lup <- make_bgd_RR_lup(pa_x_params_ls = pa_x_params_ls,
                                                            dates_idx_dbl_vec = dates_idx_dbl_vec,
                                                            dates_nms_chr_vec = dates_nms_chr_vec,
                                                            RR_tb = RR_tb,
                                                            prev_data_dbl_vec = prev_data_dbl_vec,
                                                            age_range_chr_vec = names_chr_vec)
                              shock_RR_lup <- make_shock_RR_tb(RR_tb = RR_tb,
                                                               var_chr ="Scenario 1_RR",
                                                               dates_nms_chr_vec = dates_nms_chr_vec)
                              purrr::reduce(age_range_chr_vec,
                                                          .init = par_str_tb,
                                                          ~ .x %>%
                                                            dplyr::add_row(param_name = shock_RR_lup$Dtm %>%
                                                                             as.character() %>%
                                                                             stringr::str_replace_all("-","") %>%
                                                                             paste0("RR_shock_",.,"_",.y),
                                                                           deter_val = shock_RR_lup$V1,
                                                                           distribution = rep("pert", times = length(shock_RR_lup$V1)),
                                                                           dist_param_1 = shock_RR_lup$V1,
                                                                           dist_param_2 = shock_RR_lup$V1*0.75,
                                                                           dist_param_3 = shock_RR_lup$V1*1.50,
                                                                           dist_param_4 = rep(3, times = length(shock_RR_lup$V1)),
                                                                           use_in = "base")) %>%
                                dplyr::add_row(param_name = bgd_RR_lup$date_and_age_chr %>%
                                                 stringr::str_sub(start=4) %>%
                                                 paste0("RR_bgd_",.),
                                               deter_val = bgd_RR_lup$V1,
                                               distribution = rep("pert", times = length(bgd_RR_lup$V1)),
                                               dist_param_1 = bgd_RR_lup$V1,
                                               dist_param_2 = bgd_RR_lup$V1*0.95,
                                               dist_param_3 = bgd_RR_lup$V1*1.1,
                                               dist_param_4 = rep(3, times = length(bgd_RR_lup$V1)),
                                               use_in = "base")

                            })
## Paramater values
new_it_nbr <- 20
par_str_list <- instantiate_env_struc_par_all(par_str_tb)
par_vals_tb  <- purrr::map_dfr(1:length(par_str_list),
                                ~ genValueFromDist(par_str_list[[.x]], new_it_nbr))
param_tb <- par_vals_tb
#pa_x_params_ls$sim_data_r4@st_envir@par_vals


