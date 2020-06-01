# library(ready4sim)
# library(ready4space)
library(springtides)
#saveRDS(output_params_ls,paste0(project_data_path_chr,"/output_params_ls.RDS"))
# import_model_output_tb <- function(path_to_K10_data_chr,
#                                    range_ls,
#                                    start_date_dtm){
#   purrr::map(range_ls,
#              ~ {
#                tb <- readxl::read_xlsx(path_to_K10_data_chr,
#                                        range = .x)
#                tb <- tb %>%
#                  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('Scen')), .funs = list(RR = ~(1+(.-Baseline)/Baseline))) %>%
#                  dplyr::mutate(week_starting_dtm = purrr::map_dbl(t,
#                                                                   ~ start_date_dtm + lubridate::weeks(.x)))
#                attributes(tb$week_starting_dtm) <- attributes(start_date_dtm)
#                tb
#              }) %>%
#     stats::setNames(names(range_ls))
#
# }
# make_shock_RR_tb <- function(RR_tb,
#                              var_chr,
#                              dates_nms_chr_vec,
#                              shock_rr_is_one_lgl){
#   tb <- purrr::map2_dfc(1:nrow(RR_tb),
#                         dates_nms_chr_vec,
#                         ~ tibble::tibble(!!rlang::sym(.y) := (RR_tb %>%
#                                                                 dplyr::slice(.x) %>%
#                                                                 dplyr::pull(!!rlang::sym(var_chr)))
#                         )) %>%
#     t() %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("Dtm")
#   tb$Dtm <- tf_vars_to_dtm_vec(tb$Dtm,
#                                 starts_dbl = 4,
#                                 tz = "Australia/Melbourne")
#   if(shock_rr_is_one_lgl)
#     tb$V1 <- 1
#   tb
# }
# make_bgd_RR_lup <- function(pa_x_params_ls,
#                             dates_idx_dbl_vec, # Named
#                             dates_nms_chr_vec,
#                             RR_tb,
#                             prev_data_dbl_vec,
#                             age_range_chr_vec,
#                             bgd_rr_is_one_lgl){
#
#   bgd_RR_lup <- purrr::reduce(1:length(dates_idx_dbl_vec),
#                 .init = tibble::tibble(age_range_chr = age_range_chr_vec),
#                 ~ {
#                   row_idx_dbl = .y
#                   dplyr::mutate(.x,!!rlang::sym(dates_nms_chr_vec[.y]) := purrr::map_dbl(age_range_chr,
#                                                                                          ~ {
#                                                                                            idx_dbl <- which(age_range_chr_vec==.x)
#                                                                                            1+(RR_tb %>%
#                                                                                                        dplyr::slice(row_idx_dbl) %>%
#                                                                                                 dplyr::pull(Baseline)-prev_data_dbl_vec[idx_dbl])/prev_data_dbl_vec[idx_dbl]
#
#                                                                                          })
#                   )
#                 })
#   bgd_RR_lup <- purrr::map_dfr(1:nrow(bgd_RR_lup),
#                  ~bgd_RR_lup[.x,2:ncol(bgd_RR_lup)] %>%
#                    t() %>%
#                    as.data.frame() %>%
#                    tibble::rownames_to_column("date_and_age_chr") %>%
#                    tibble::as_tibble() %>%
#                    dplyr::mutate(date_and_age_chr = paste0(date_and_age_chr %>% stringr::str_sub(end=-4),
#                                                            "_",
#                                                            bgd_RR_lup[.x,1] %>% stringr::str_sub(start=-5))))
#   if(bgd_rr_is_one_lgl)
#     bgd_RR_lup$V1 <- 1
#   bgd_RR_lup
# }
# update_par_vals_tb <- function(sp_data_sf,
#                                distress_tbs_ls,
#                                distress_ages_ls,
#                                pa_x_params_ls,
#                                epi_data_dtm_ls,
#                                bgd_rr_is_one_lgl = T,
#                                shock_rr_is_one_lgl = F){
#   dates_nms_chr_vec <- get_var_pfx_chr_vec(sp_data_sf = sp_data_sf,
#                                            stat = "popl") %>%
#     stringr::str_sub(end = -2) %>%
#     unique() %>%
#     paste0("_RR")
#   dtm_vec <- tf_vars_to_dtm_vec(dates_nms_chr_vec,
#                                 tz = "Australia/Melbourne")
#   ## Structural parameters
#   ## NEED TO :
#   ## A) SET SEED
#
#   purrr::reduce(1:length(distress_tbs_ls),
#                 .init = pa_x_params_ls$input_ls$env_str_par_tb,
#                 ~ {
#                   par_str_tb <- .x
#                   age_range_chr_vec <- distress_ages_ls[[.y]]
#                   distress_tb <- distress_tbs_ls[[.y]]
#                   prev_params_dbl_vec <- purrr::map_dbl(epi_data_dtm_ls,
#                                                         ~ which.min(abs(distress_tb$week_starting_dtm-.x)))
#                   prev_data_dbl_vec <- distress_tb %>%
#                     dplyr::slice(prev_params_dbl_vec) %>%
#                     dplyr::pull(Baseline)
#                   dates_idx_dbl_vec <- purrr::map_dbl(dtm_vec,
#                                                       ~ which.min(abs(distress_tb$week_starting_dtm-.x)))
#                   RR_tb <- distress_tb %>% dplyr::slice(dates_idx_dbl_vec)
#                   names_chr_vec <- purrr::map(age_range_chr_vec,
#                                               ~ {
#                                                 age_range_chr <- .x
#                                                 names_chr_vec <- names(epi_data_dtm_ls) %>%
#                                                   purrr::map_chr(~ paste0(.x %>% stringr::str_sub(end=4),
#                                                                           .x %>% stringr::str_sub(start=5,end=6) %>%
#                                                                             as.numeric() %>% max(age_range_chr %>%
#                                                                                                    stringr::str_sub(end=2) %>%
#                                                                                                    as.numeric()) %>%
#                                                                             paste0(ifelse(.<10,"0",""),.),
#                                                                           "_",
#                                                                           .x %>% stringr::str_sub(start=8,end=9) %>%
#                                                                             as.numeric() %>% min(age_range_chr %>%
#                                                                                                    stringr::str_sub(start=4) %>%
#                                                                                                    as.numeric()) %>%
#                                                                             paste0(ifelse(.<10,"0",""),.)))
#                                                 names_chr_vec[purrr::map_lgl(names_chr_vec,
#                                                                              ~ stringr::str_sub(.x,start=5,end=6)< stringr::str_sub(.x,start=8,end=9))]
#                                               }) %>%
#                     unlist()
#                   bgd_RR_lup <- make_bgd_RR_lup(pa_x_params_ls = pa_x_params_ls,
#                                                 dates_idx_dbl_vec = dates_idx_dbl_vec,
#                                                 dates_nms_chr_vec = dates_nms_chr_vec,
#                                                 RR_tb = RR_tb,
#                                                 prev_data_dbl_vec = prev_data_dbl_vec,
#                                                 age_range_chr_vec = names_chr_vec,
#                                                 bgd_rr_is_one_lgl= bgd_rr_is_one_lgl)
#                   shock_RR_lup <- make_shock_RR_tb(RR_tb = RR_tb,
#                                                    var_chr ="Scenario 1_RR",
#                                                    dates_nms_chr_vec = dates_nms_chr_vec,
#                                                    shock_rr_is_one_lgl= shock_rr_is_one_lgl)
#                   purrr::reduce(age_range_chr_vec,
#                                 .init = par_str_tb,
#                                 ~ .x %>%
#                                   dplyr::add_row(param_name = shock_RR_lup$Dtm %>%
#                                                    as.character() %>%
#                                                    stringr::str_replace_all("-","") %>%
#                                                    paste0("RR_shock_",.,"_",.y),
#                                                  deter_val = shock_RR_lup$V1,
#                                                  distribution = rep("pert", times = length(shock_RR_lup$V1)),
#                                                  dist_param_1 = shock_RR_lup$V1,
#                                                  dist_param_2 = shock_RR_lup$V1*0.75,
#                                                  dist_param_3 = shock_RR_lup$V1*1.25,
#                                                  dist_param_4 = rep(3, times = length(shock_RR_lup$V1)),
#                                                  use_in = "base")) %>%
#                     dplyr::add_row(param_name = bgd_RR_lup$date_and_age_chr %>%
#                                      stringr::str_sub(start=4) %>%
#                                      paste0("RR_bgd_",.),
#                                    deter_val = bgd_RR_lup$V1,
#                                    distribution = rep("pert", times = length(bgd_RR_lup$V1)),
#                                    dist_param_1 = bgd_RR_lup$V1,
#                                    dist_param_2 = bgd_RR_lup$V1*0.95,
#                                    dist_param_3 = bgd_RR_lup$V1*1.05,
#                                    dist_param_4 = rep(3, times = length(bgd_RR_lup$V1)),
#                                    use_in = "base")
#
#                 })
# }
# run_scenarios <- function(sp_data_sf,
#                           distress_tbs_ls,
#                           distress_ages_ls,
#                           pa_x_params_ls,
#                           epi_data_dtm_ls,
#                           scenarios_ls,
#                           n_its_dbl,
#                           save_path_chr)
#   purrr::walk(1:length(scenarios_ls),
#               ~ {
#                 par_str_tb <- update_par_vals_tb(sp_data_sf = sp_data_sf,
#                                                  distress_tbs_ls = distress_tbs_ls,
#                                                  distress_ages_ls = distress_ages_ls,
#                                                  pa_x_params_ls = pa_x_params_ls,
#                                                  epi_data_dtm_ls = epi_data_dtm_ls,
#                                                  bgd_rr_is_one_lgl = scenarios_ls[[.x]][1],
#                                                  shock_rr_is_one_lgl = scenarios_ls[[.x]][2])
#                 ## Paramater values
#                 n_its_dbl <- n_its_dbl
#                 par_str_list <- ready4sim::instantiate_env_struc_par_all(par_str_tb)
#                 set.seed(1987)
#                 par_vals_tb  <-  make_par_vals_tb(par_str_ls = par_str_list, ## ready4sim::
#                                                   n_its_dbl = n_its_dbl)
#                 new_sim_data_r4 <- pa_x_params_ls$sim_data_r4
#                 new_sim_data_r4@st_envir@par_vals <- par_vals_tb
#                 new_sim_results <- ready4sim::runSimulation(x = new_sim_data_r4,
#                                                  n_its_int = n_its_dbl,
#                                                  group_by = pa_x_params_ls$input_ls$grouping_for_sim,
#                                                  inc_ti_lgl = T)
#                 ## Save results
#                 saveRDS(new_sim_results,paste0(save_path_chr,"/sim_res_",names(scenarios_ls)[.x],".RDS"))
#                 saveRDS(par_vals_tb,paste0(save_path_chr,"/par_vals_tb_",names(scenarios_ls)[.x],".RDS"))
#
#               })

## SPECIFY DATA PATHS
r_data_path_chr <- "C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"
project_data_path_chr <- r_data_path_chr %>% stringr::str_replace("R_Format","Project/COVID19")
path_to_K10_data_chr <- paste0(project_data_path_chr,"/Copy of Psychological Distress - North Coast outputs.xlsx")
pa_dir_chr <- "Victoria_04_84"
## IMPORT SYDNEY DATA
start_date_dtm <- lubridate::ymd_hms("2011-01-01 00:00:00",
                                     tz = "Australia/Melbourne")
distress_tbs_ls <- import_model_output_tb(path_to_K10_data_chr = path_to_K10_data_chr,
                                          range_ls = list(youth_distress_tb = "L3:U734",
                                                          all_distress_tb = "A3:J734"),
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
sp_data_sf <- output_params_ls$sim_results_ls[[1]][[1]]
basic_cols_chr_vec <- names(sp_data_sf)[names(sp_data_sf) %>% startsWith(c("pop"))|
                                          names(sp_data_sf) %>% startsWith(c("t0_2"))|
                                          names(sp_data_sf) %>% startsWith(c("ti_"))|
                                          names(sp_data_sf) %>% startsWith(c("tx_2"))|
                                          names(sp_data_sf) %>% startsWith(c("delta_2"))|
                                          names(sp_data_sf) %>% startsWith(c("SA2_MAIN16"))]
sp_data_sf <- sp_data_sf %>%
  dplyr::select(tidyselect::all_of(basic_cols_chr_vec))
### UPDATE_PARAMS_LS
#pa_x_params_ls <- output_params_ls
## MAIN RUN
n_its_dbl <- 1
pa_x_params_ls$input_ls$env_str_par_tb
run_scenarios_ls(sp_data_sf = sp_data_sf,
              distress_tbs_ls = distress_tbs_ls,
              distress_ages_ls = distress_ages_ls,
              pa_x_params_ls = output_params_ls,
              epi_data_dtm_ls = epi_data_dtm_ls,
              scenarios_ls = list(bc = c(T,T),
                                  shock_only = c(T,F)
                                  #,
                                  #bgd_only = c(F,T),
                                  #both_cfs = c(F,F)
                                  ),
              n_its_dbl = n_its_dbl,
              save_path_chr = project_data_path_chr
                #"I://Research//Partnership Grant Study//4. AIM 3 ( Economic Analyses and Fidelity)//COVID//Victoria_STE"
              )
input_ls <- output_params_ls$input_ls
input_ls$n_its_int <- n_its_dbl
sim_data_r4 <- output_params_ls$sim_data_r4
saveRDS(input_ls,paste0(project_data_path_chr,"/input_ls.RDS"))
saveRDS(sim_data_r4,paste0(project_data_path_chr,"/sim_data_r4.RDS"))
