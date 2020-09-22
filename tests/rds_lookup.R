r_data_dir_chr = normalizePath("C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format")
input <- list(pa_type_chr = "Predefined boundary")
data_pckg_chr <- "springtides" # Abstract
pa_r4_chr <- "aus_pa_r4"
data(list = pa_r4_chr,
     package = data_pckg_chr,
     envir = environment())
eval(parse(text = paste0("pa_r4<-",pa_r4_chr)))
meso2_choices = pa_r4@lookup_tb@sp_data_pack_lup %>%
  dplyr::filter(main_feature == "Boundary") %>%
  dplyr::filter(!area_type %in% c(#"AUS",
    "HSS",#"SA1","SA2",
    "XX1")) %>% ## ABSTRACT
  dplyr::pull(area_type) %>%
  unique() %>%
  purrr::map_chr(~ready4fun::get_from_lup_obj(pa_r4@lookup_tb@sp_abbreviations_lup,
                                          match_var_nm_chr = "short_name",
                                          match_value_xx = .x,
                                          target_var_nm_chr = "long_name",
                                          evaluate_lgl = F)) %>%
  sort()
input$meso2_type_chr <- meso2_choices[5]
meso2_type_tb <- pa_r4@lookup_tb@sp_data_pack_lup %>%
  dplyr::filter(main_feature == "Boundary") %>%
  dplyr::filter(area_type == input$meso2_type_chr %>%
                  ready4fun::get_from_lup_obj(pa_r4@lookup_tb@sp_abbreviations_lup,
                                          match_var_nm_chr = "long_name",
                                          match_value_xx = .,
                                          target_var_nm_chr = "short_name",
                                          evaluate_lgl = F))
meso2_bound_yr_chr_vec <- meso2_type_tb %>%
  dplyr::pull(area_bound_yr) %>%
  unique() %>%
  sort()
meso2_uid_tb <- pa_r4@lookup_tb@sp_uid_lup %>%
  dplyr::filter(spatial_unit == input$meso2_type_chr %>%
                  ready4fun::get_from_lup_obj(pa_r4@lookup_tb@sp_abbreviations_lup,
                                          match_var_nm_chr = "long_name",
                                          match_value_xx = .,
                                          target_var_nm_chr = "short_name",
                                          evaluate_lgl = F))
reactive_ls <- list(meso2_choices_ls = list(meso2_type_tb = meso2_type_tb,
                                     meso2_bound_yr_chr_vec = meso2_bound_yr_chr_vec,
                                     meso2_uid_tb = meso2_uid_tb))
input$meso2_bound_yr <- reactive_ls$meso2_choices_ls$meso2_bound_yr_chr_vec[1]
getSpUnitVarChr <-  function(){
  pa_r4@lookup_tb@sp_uid_lup %>%
    ready4space::get_data(value_chr = pa_r4@lookup_tb@sp_abbreviations_lup %>%
                            ready4space::get_data(col_chr = "long_name",
                                                  value_chr = input$meso2_type_chr), #"PHN",
                          area_bound_yr = input$meso2_bound_yr)
}
getPolysSf <- function(){
  filtered_dp_lup <- pa_r4@lookup_tb@sp_data_pack_lup %>%
    dplyr::filter(area_bound_yr == input$meso2_bound_yr) %>%
    dplyr::filter(data_type == "Geometry")
  ready4space::get_data(filtered_dp_lup,
                        r_data_dir_chr = r_data_dir_chr,
                        col_chr = "area_type",
                        value_chr = pa_r4@lookup_tb@sp_abbreviations_lup %>%
                          ready4space::get_data(col_chr = "long_name",
                                                value_chr = input$meso2_type_chr)
  )
}

if(input$pa_type_chr !="HSS"){
  sf <- getPolysSf()
  sp_unit_var_chr <- getSpUnitVarChr()
}else{
  sf <- getPointsSf()
  sp_unit_var_chr <- "service_name"
}
if(is.null(input$map_bounds)) {
  sf
} else {
  bounds <- input$map_bounds
  filter_by_map_view_sf(sf,
                        bounds,
                        col_chr = sp_unit_var_chr)
}
####
uid_chr <- meso2_uid_tb$var_name


# saveRDS(sf, paste0(r_data_dir_chr,"/aus_sos_nat_bnd_2016_sf.rds"))
