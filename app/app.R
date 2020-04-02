library(shiny)
library(springtidesui)
options(shiny.maxRequestSize=100*1024^2)
# launch_app(r_data_dir_chr = normalizePath("C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"),
#            credentials_tb = data.frame(
#              user = c("user_1", "user_2", "user_3", "user_4","user_5"),
#              password = c("password_1", "password_2", "password_3", "password_4","password_5"),
#              stringsAsFactors = FALSE),
#            shinyio_lgl = F)
launch_basic_app(r_data_dir_chr = normalizePath("C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"),
           # credentials_tb = data.frame(
           #   user = c("user_1", "user_2", "user_3", "user_4","user_5"),
           #   password = c("password_1", "password_2", "password_3", "password_4","password_5"),
           #   stringsAsFactors = FALSE),
           shinyio_lgl = F)
# r_data_dir_chr <- normalizePath("C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format")
# ui <- fluidPage(theme = shinythemes::shinytheme("journal"),
#                 tags$style(".btn {
#                      background-color: orange;
#                                 }"),
#                 shiny::headerPanel('Springtides'),
#
#   # App title ----
#   # titlePanel("Tabsets"),
#
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       shiny::uiOutput("statisticControls"),
#       shiny::uiOutput("disorderControls"),
#       shiny::uiOutput("ageRangeControls"),
#       shiny::uiOutput("when_controls"),
#       shiny::sliderInput("n_its_int", "Number of simulation iterations", 1, 100, 10),
#       shiny::sliderInput("uncertainty_int", "Uncertainty Interval",
#                          min = 0.01, max = 1, value = c(0.025,0.975), step = 0.005),
#       shiny::textInput("user_name_chr", "Your name / your organisation's name", value = "Anonymous User"),
#       shiny::uiOutput("areaNameControls"),
#       shiny::p("Note: It will take between 5 and 20 minutes to generate your report."),
#       shiny::radioButtons("report_format_chr","Report format:",
#                           c("PDF" = "PDF",
#                             "HTML" = "HTML",
#                             "Word" = "Word"), inline=T),
#       shiny::downloadButton("report", "Generate a report")
#     ),
#
#     # Main panel for displaying outputs ----
#     mainPanel(
#
#       # Output: Tabset w/ plot, summary, and table ----
#       tabsetPanel(type = "tabs",
#                   # tabPanel("Plot", plotOutput("plot")),
#                   # tabPanel("Summary", verbatimTextOutput("summary")),
#                   tabPanel("Area to profile",
#                            shiny::selectInput("pa_type_chr", h3("Type of geometry"),
#                                               choices = list("PHN Boundary" = "Predefined boundary",
#                                                              "Poximity to Headspace Centres" = "HSS"
#                                                              # ,
#                                                              # "Base on proximity to custom coordinates" = "Custom"
#                                               ),
#                                               selected = "Predefined boundary"),
#                            shiny::conditionalPanel(
#                              condition = "input.pa_type_chr == \"Predefined boundary\"",
#                              shiny::uiOutput("areaControls")
#                            ),
#                            shiny::conditionalPanel(
#                              condition = "input.pa_type_chr == \"HSS\"",
#                              shiny::uiOutput("headspaceControls"),
#                              # shiny::selectInput("pa_type_chr", h3("Geometry"),
#                              #                    choices = list("Select from a menu of existing options" = "Predefined boundary",
#                              #                                   "Generate your own" = "HSS"
#                              #                                   # ,
#                              #                                   # "Base on proximity to custom coordinates" = "Custom"
#                              #                    ),
#                              #                    selected = "Predefined boundary"),
#                              shiny::conditionalPanel(
#                                condition = "input.pa_type_chr != \"Predefined boundary\"",
#                                shiny::selectInput("gdist_ttime_chr", h3("Proximity measure"),
#                                                   choices = list("Maximium geometric distance" = "Geometric distance",
#                                                                  "Maximum drive time" = "Travel time"),
#                                                   selected = "Geometric distance")
#                              ),
#                              shiny::conditionalPanel(
#                                condition = "input.pa_type_chr != \"Predefined boundary\" & input.gdist_ttime_chr == \"Geometric distance\"",
#                                shiny::sliderInput("gdist_dbl", "Geometric distance in Kilometres",
#                                                   min = 10, max = 50, value = 20)
#                              ),
#                              shiny::conditionalPanel(
#                                condition = "input.pa_type_chr != \"Predefined boundary\" & input.gdist_ttime_chr == \"Travel time\"",
#                                shiny::sliderInput("ttime_dbl", "Drive time in minutes",
#                                                   min = 15, max = 60, value = 20)
#                              )
#                            )
#                            )
#
#       )
#
#     )
#   )
# )
# server <- function(input, output) {
#
#   data_pckg_chr <- "springtides"
#   pa_r4_chr <- "aus_pa_r4"
#   data(list = pa_r4_chr,
#        package = data_pckg_chr,
#        envir = environment())
#   eval(parse(text = paste0("pa_r4<-",pa_r4_chr)))
#   reactive_ls <- shiny::reactiveValues()
#   shiny::observe({
#     if(is.null(input$stat_chr)){
#       reactive_ls$disorder_choices_vec <- NULL
#     }else{
#       reactive_ls$disorder_choices_vec <- springtides::get_input_vec(fn = springtides::get_disorder_choices_chr_vec,
#                                                                      args = list(lup_r4 = pa_r4@lookup_tb,
#                                                                                  path_to_data_chr = r_data_dir_chr,
#                                                                                  stat_chr = input$stat_chr,
#                                                                                  sex_chr_vec = c("Female","Male")),
#                                                                      n = Inf) %>% stringr::str_replace_all("_"," ") %>%
#         sort()
#     }
#   })
#
#   output$ageRangeControls <- shiny::renderUI({
#     # if(is.null(input$disorder_chr))
#     #   return()
#     age_vec <- springtides::get_age_range_choices_int_vec(lup_r4 = pa_r4@lookup_tb,
#                                                           path_to_data_chr = r_data_dir_chr,
#                                                           stat_chr = input$stat_chr,
#                                                           disorder_chr = input$disorder_chr %>%
#                                                             stringr::str_replace_all(" ","_"))
#     shiny::tagList(
#       shiny::sliderInput("age_range_int_vec",
#                          shiny::h3("Age range"),
#                          min = min(age_vec), max = max(age_vec), value = age_vec))
#   })
#   output$areaControls <- shiny::renderUI({
#     if(input$pa_type_chr !="Predefined boundary" #| is.null(reactive_ls$meso2_chr_choices_vec)
#        )
#       return()
#     meso2_uid_tb <- pa_r4@lookup_tb@sp_uid_lup %>%
#       dplyr::filter(spatial_unit == "PHN")
#     meso2_type_tb <- pa_r4@lookup_tb@sp_data_pack_lup %>%
#       dplyr::filter(main_feature == "Boundary") %>%
#       dplyr::filter(area_type == "PHN")
#     bnd_sf <- readRDS(meso2_type_tb %>%
#                         dplyr::filter(area_bound_yr == 2017) %>%
#                         dplyr::pull(source_reference) %>%
#                         paste0(r_data_dir_chr,
#                                "/",
#                                .,".rds"
#                         ))
#
#     meso2_chr_choices_vec <- bnd_sf  %>%
#       dplyr::pull(!!rlang::sym(meso2_uid_tb %>%
#                                  dplyr::filter(year == 2017) %>%
#                                  dplyr::pull(var_name))) %>%
#       sort()
#     shiny::tagList(
#       shiny::selectInput("meso2_chr", h3("Feature"),
#                          choices = meso2_chr_choices_vec)
#
#     )
#   })
#   output$areaNameControls <- shiny::renderUI({
#     gdist_dbl <- ifelse(input$pa_type_chr=="Predefined boundary",
#                         NA_real_,
#                         ifelse(is.null(input$gdist_dbl),
#                                NA_real_,
#                                input$gdist_dbl))
#     # ttime_dbl <- ifelse(input$pa_type_chr=="Predefined boundary",
#     #                     NA_real_,
#     #                     ifelse(is.null(input$ttime_dbl), NA_real_,input$ttime_dbl))
#
#     gdist_ttime_chr <- ifelse(input$pa_type_chr=="Predefined boundary",
#                               NA_character_,
#                               "Geometric distance"#ifelse(is.null(input$gdist_ttime_chr), NA_character_,input$gdist_ttime_chr)
#                               )
#     if(is.null(input$micro_chr_vec)){
#       points_chr <- "" # Replace with numeric count of points for custom coordinates and >5 services
#     }else{
#       points_chr <- input$micro_chr_vec %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed( ',', ' and')
#     }
#     default_chr <- paste0("Area within ",
#                           ifelse(is.na(gdist_ttime_chr),
#                                  "",
#                                  ifelse(gdist_ttime_chr=="Geometric distance",
#                                         paste0(gdist_dbl, " Km "),
#                                         paste0(ttime_dbl, " Minutes Drive "))),
#                           "of ",
#                           points_chr,
#                           ifelse(T,
#                                  paste0(" Headspace Centre",ifelse(length(input$micro_chr_vec)>1,"s","")),
#                                  " Custom Coordinates")) # Replace with condition logic for custom coordinates
#     shiny::conditionalPanel(
#       condition = "input.pa_type_chr != \"Predefined boundary\"",
#       shiny::textInput("area_name_chr", "Name of the custom geometry that you are profiling", value = default_chr)
#     )
#   })
#   output$disorderControls <- shiny::renderUI({
#     shiny::tagList(shiny::selectInput("disorder_chr",
#                                       shiny::h3("Disorder or behaviour"),
#                                       choices = reactive_ls$disorder_choices_vec))
#   })
#   output$headspaceControls <- shiny::renderUI({
#     if(input$pa_type_chr !="HSS")
#       return()
#     shiny::tagList(
#       shiny::checkboxGroupInput("micro_chr_vec", "Headspace Centres",
#                                 choices = pa_r4@lookup_tb@sp_site_coord_lup %>%
#                                   dplyr::pull(service_name) %>% unique() %>% sort(),
#                                 inline = T)
#     )
#   })
#   output$report <- shiny::downloadHandler(
#     filename = function() {
#       paste('Springtides_Report', sep = '.', switch(
#         input$report_format_chr, PDF = 'pdf', HTML = 'html', Word = 'docx'
#       ))
#     },
#     content = function(file) {
#       withProgress(message = 'Rendering, please wait!', {
#         path_to_template_chr <- system.file("report.Rmd", package = "springtidesui")#"report.Rmd"#
#         temp_dir_chr <- tempdir()
#         file.copy(path_to_template_chr, paste0(temp_dir_chr,'/report.Rmd'), overwrite = TRUE)
#         if(input$pa_type_chr!="Predefined boundary"){
#           meso2_type_chr <- NA_character_
#         }else{
#           meso2_type_chr <- ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
#                                                   lookup_variable = "long_name",
#                                                   lookup_reference = "Primary Health Network",
#                                                   target_variable = "short_name",
#                                                   evaluate = F)
#         }
#         if(is.null(input$meso2_chr)){
#           meso2_chr <- NA_character_
#         }else{
#           meso2_chr <- input$meso2_chr
#         }
#         if(is.null(input$micro_chr_vec)){
#           micro_chr_vec <- NA_character_
#         }else{
#           micro_chr_vec <- input$micro_chr_vec
#         }
#         params_ls <- list(age_lower = input$age_range_int_vec[1],
#                           age_upper = input$age_range_int_vec[2],
#                           authorship_1_chr =  "Report generated by the Orygen Springtides App",
#                           authorship_2_chr =  paste0("Input parameters selected by ",input$user_name_chr),
#                           data_pckg_chr = data_pckg_chr,
#                           disorder_chr = input$disorder_chr %>%
#                             stringr::str_replace_all(" ","_"),
#                           format_chr = input$report_format_chr,
#                           gdist_dbl = ifelse(input$pa_type_chr=="Predefined boundary",
#                                              NA_real_,
#                                              ifelse(is.null(input$gdist_dbl), NA_real_,input$gdist_dbl)),
#                           gdist_ttime_chr = ifelse(input$pa_type_chr=="Predefined boundary",
#                                                    NA_character_
#                                                    ,ifelse(is.null(input$gdist_ttime_chr), NA_character_,input$gdist_ttime_chr)),
#                           meso2_bound_yr = ifelse(input$pa_type_chr=="Predefined boundary",
#                                                   as.integer(input$meso2_bound_yr),
#                                                   NA_real_),
#                           meso2_chr = meso2_chr,
#                           meso2_name_chr = springtides::make_area_name_chr(pa_r4 = pa_r4,
#                                                                            pa_type_chr = input$pa_type_chr,
#                                                                            area_type_chr = meso2_type_chr,
#                                                                            feature_chr = meso2_chr,
#                                                                            area_name_chr = input$area_name_chr),
#                           meso2_type_chr = meso2_type_chr,
#                           micro_chr_vec = micro_chr_vec,
#                           model_end_date = min(input$dateRange[2] %>% lubridate::as_datetime(tz="Australia/Melbourne"), pa_r4@temporal_max),
#                           model_start_date = max(input$dateRange[1] %>% lubridate::as_datetime(tz="Australia/Melbourne"), pa_r4@temporal_min),
#                           n_its_int = input$n_its_int,
#                           pa_r4_chr = pa_r4_chr,
#                           pa_type_chr = input$pa_type_chr,
#                           pdf_output_lgl = switch(input$report_format_chr,
#                                                   PDF = T,
#                                                   HTML = F,
#                                                   Word = T),
#                           r_data_dir_chr = r_data_dir_chr,
#                           rendered_by_shiny_lgl = T,
#                           stat_chr = input$stat_chr,
#                           ttime_dbl = ifelse(input$pa_type_chr=="Predefined boundary",
#                                              NA_real_,
#                                              ifelse(is.null(input$ttime_dbl), NA_real_,input$ttime_dbl)),
#                           uncertainty_1_int = input$uncertainty_int[1],
#                           uncertainty_2_int = input$uncertainty_int[2],
#                           user_name_chr = input$user_name_chr)
#         params_ls$title_chr <- paste0("Predicted ",
#                                       springtides::tf_stat_chr(stat_chr = params_ls$stat_chr %>% tolower(),
#                                                                disorder_chr = params_ls$disorder_chr),
#                                       " in young people aged ",
#                                       params_ls$age_lower,
#                                       " to ",
#                                       params_ls$age_upper,
#                                       " for ",
#                                       params_ls$meso2_name_chr,
#                                       " between ",
#                                       params_ls$model_start_date %>% format("%d %B %Y"),
#                                       " and ",
#                                       params_ls$model_end_date %>% format("%d %B %Y"))
#         out <- rmarkdown::render(paste0(temp_dir_chr,'/report.Rmd'),
#                                  switch(input$report_format_chr,
#                                         PDF = rmarkdown::pdf_document(),
#                                         HTML = rmarkdown::html_document(toc=T,
#                                                                         toc_float = T,
#                                                                         number_sections = T,
#                                                                         theme = "journal"),
#                                         Word = rmarkdown::word_document()),
#                                  params = params_ls,
#                                  envir = new.env())
#         file.rename(out, file)
#       })
#     }
#   )
#   output$statisticControls <- shiny::renderUI({
#     shiny::tagList(
#       shiny::selectInput("stat_chr", h3("Statistic"),
#                          choices = springtides::get_input_ls(fn = springtides::make_stat_choices_ls,
#                                                              args = NULL,
#                                                              n = Inf) %>% purrr::flatten_chr() %>% sort())
#     )
#   })
#   output$when_controls <- shiny::renderUI({
#     if(input$pa_type_chr=="HSS" & length(input$micro_chr_vec) == 0)
#       return()
#     shiny::tagList(
#       shiny::dateRangeInput('dateRange',
#                             label = 'Start and end dates (yyyy-mm-dd)',
#                             start = Sys.Date(),
#                             end = (Sys.Date() + lubridate::years(5)) %>% min(pa_r4@temporal_max),
#                             min = pa_r4@temporal_min,
#                             max = pa_r4@temporal_max
#       )
#     )
#   })
# }
# shiny::shinyApp(ui=ui,server = server)
