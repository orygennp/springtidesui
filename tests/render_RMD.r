library(magrittr)
template_path_chr <- system.file("report.Rmd", package = "springtidesui")
temp_dir_chr <- tempdir()
file.copy(template_path_chr, paste0(temp_dir_chr,'/report.Rmd'), overwrite = TRUE)
r_data_path_chr <- "C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"
project_data_path_chr <- r_data_path_chr %>% stringr::str_replace("R_Format","Project")
input <-list(report_format_chr = "HTML")
params_ls <- readRDS(paste0(project_data_path_chr,"/params_4.rds"))
params_ls$add_title_lgl <- ifelse(input$report_format_chr=="HTML",T,F)
params_ls$data_pckg_chr <- "springtides"
params_ls$disorder_chr <- "Any_Affective_Disorder"
params_ls$meso2_bound_yr <- 2011
params_ls$meso2_chr <- "0800"
params_ls$meso2_name_chr <- "0800 Postal Area"
params_ls$n_its_int <- 10
params_ls$pa_r4_chr <- "aus_pa_r4"
params_ls$r_data_dir_chr <- r_data_path_chr
params_ls$input_ls_path_chr <- paste0(project_data_path_chr,"/input_ls_4.rds")
params_ls$sim_data_r4_path_chr <- paste0(project_data_path_chr,"/sim_data_r4_4.rds")
params_ls$sim_results_ls_path_chr <- paste0(project_data_path_chr,"/sim_results_ls_4.rds")
params_ls$stat_chr <- "Prevalence - Annual"
params_ls$title_chr <- "Springtides report"
input <-list(report_format_chr = "HTML")
params_ls$pdf_output_lgl <- ifelse(input$report_format_chr=="PDF",T,F)
out <- rmarkdown::render(paste0(temp_dir_chr,'/report.Rmd'),
                         switch(input$report_format_chr,
                                PDF = rmarkdown::pdf_document(),
                                HTML = knitrBootstrap::bootstrap_document(title = params_ls$title_chr,
                                                                          theme = "journal",
                                                                          menu = F),
                                Word = rmarkdown::word_document()),
                        params = params_ls,
                        envir = new.env())


