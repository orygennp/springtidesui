test_ls <-list(col_names_chr_vec = col_names_chr_vec,
     plot_legend_chr_vec = plot_legend_chr_vec,
     make_density_lgl_vec = make_density_lgl_vec,
     make_discrete_lgl_vec = make_discrete_lgl_vec,
     diverge_lgl_vec = diverge_lgl_vec)
index_dbl <- 3
add_feature_to_land(land_pl = land_pl,
                      features_sf = features_sf,
                      feature_var_chr = paste0(test_ls[[1]][index_dbl],ifelse(test_ls[[3]][index_dbl],"_dns","")),
                      zoom_chr = "feature",
                      scale_title_chr = test_ls[[2]][index_dbl],
                      make_discrete_lgl = test_ls[[4]][index_dbl],
                      diverge_lgl = test_ls[[5]][index_dbl],
                      input_ls = input_ls)
