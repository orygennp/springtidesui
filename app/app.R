library(shiny)
library(springtidesui)
# library(shinymanager)
# library(shinyjs)
# library(kableExtra)
#shiny::shinyOptions(
credentials_tb <- data.frame(
  user = c("1", "orygenNP", "orygenHSR", "OrygenPol","RFWN"),
  password = c("000", "Melton", "FirstBounce", "Electorate","Framework"),
  stringsAsFactors = FALSE)
r_data_dir_chr <- normalizePath("C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format")
#aus_pa_r4 <- springtides::aus_pa_r4
#path_to_report_chr <- system.file("test_2.Rmd", package = "springtides")
sec_server <- function(input,
                          output,
                          session){
  result_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials_tb))
  server_chr_vec <- deparse(springtidesui::server)
  eval(parse(text = server_chr_vec[3:(length(server_chr_vec)-1)]))
  output$res_auth <- shiny::renderPrint({
    shiny::reactiveValuesToList(result_auth)
  })
}
shiny::shinyApp(ui = shinymanager::secure_app(head_auth = tags$script(springtidesui::make_inactivity_script_chr()),
                                              ui = springtidesui::make_app_ui()),
                server = sec_server)
# shiny::shinyApp(ui = make_app_ui(),
#                 server = sec_server)
