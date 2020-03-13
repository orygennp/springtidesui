#' @title launch_app
#' @description FUNCTION_DESCRIPTION
#' @param r_data_dir_chr A character string
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{getShinyOption}},\code{\link[shiny]{shinyApp}}
#' @rdname launch_app
#' @export
#' @importFrom shiny shinyOptions shinyApp
launch_app <- function(r_data_dir_chr){
  require(shiny)
  shiny::shinyOptions(r_data_dir_chr = r_data_dir_chr#,
               # pa_r4 = pa_r4,
               # path_to_report_template_chr = path_to_report_template_chr#,secure_lgl = F
               )
  shiny::shinyApp(ui = make_app_ui(),
                  server = server)
}
