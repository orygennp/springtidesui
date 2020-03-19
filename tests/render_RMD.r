devtools::load_all()
template_path_chr <- system.file("Inputs_And_Outputs.rmd", package = "springtidesui")
temp_dir_chr <- tempdir()
file.copy(template_path_chr, paste0(temp_dir_chr,'/report.Rmd'), overwrite = TRUE)
r_data_path_chr <- "C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"
project_data_path_chr <- r_data_path_chr %>% stringr::str_replace("R_Format","Project")
input_ls_path_chr <- paste0(project_data_path_chr,"/input_ls_4.rds")
sim_data_r4_path_chr <- paste0(project_data_path_chr,"/sim_data_r4_4.rds")
sim_results_ls_path_chr <- paste0(project_data_path_chr,"/sim_results_ls_4.rds")
params_ls <- readRDS(paste0(project_data_path_chr,"/params_4.rds"))
params_ls$pa_r4_chr <- "aus_pa_r4"
params_ls$r_data_dir_chr <- r_data_path_chr
params_ls$input_ls_path_chr <- input_ls_path_chr
params_ls$sim_data_r4_path_chr <- sim_data_r4_path_chr
params_ls$sim_results_ls_path_chr <- sim_results_ls_path_chr

out <- rmarkdown::render(paste0(temp_dir_chr,'/report.Rmd'),
                        rmarkdown::pdf_document(),
                        params = params_ls,
                        envir = new.env()
                        )
# owd <- setwd(temp_dir_chr)
# setwd(owd)


