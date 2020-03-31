#' @title launch_app
#' @description FUNCTION_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @param credentials_tb PARAM_DESCRIPTION, Default: NULL
#' @param shinyio_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{shinyApp}}
#' @rdname launch_app
#' @export
#' @import V8
#' @importFrom shiny shinyApp
launch_app <- function(r_data_dir_chr,
                       credentials_tb = NULL,
                       shinyio_lgl = F) {
  if(shinyio_lgl){
    library(V8)
  }
  shiny::shinyApp(
    make_ui_fn(secure_lgl = !is.null(credentials_tb)),
    make_server_fn(r_data_dir_chr = r_data_dir_chr,
                   credentials_tb = credentials_tb)
  )
}

