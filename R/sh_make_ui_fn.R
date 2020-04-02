#' @title make_ui_body_fn
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{fluidPage}},\code{\link[shiny]{headerPanel}},\code{\link[shiny]{sidebarLayout}},\code{\link[shiny]{builder}},\code{\link[shiny]{htmlOutput}},\code{\link[shiny]{tabsetPanel}},\code{\link[shiny]{tabPanel}},\code{\link[shiny]{selectInput}},\code{\link[shiny]{actionButton}},\code{\link[shiny]{conditionalPanel}},\code{\link[shiny]{sliderInput}},\code{\link[shiny]{textInput}},\code{\link[shiny]{downloadButton}}
#'  \code{\link[shinythemes]{shinytheme}}
#'  \code{\link[shinyjs]{useShinyjs}}
#'  \code{\link[purrr]{flatten}}
#' @rdname make_ui_body_fn
#' @export
#' @importFrom shiny fluidPage headerPanel sidebarPanel p uiOutput mainPanel tabsetPanel tabPanel selectInput actionButton conditionalPanel sliderInput h1 textInput downloadButton
#' @importFrom shinythemes shinytheme
#' @importFrom shinyjs useShinyjs
#' @importFrom purrr flatten_chr
make_ui_body_fn <- function(){
  shiny::fluidPage(theme = shinythemes::shinytheme("journal"),
                   tags$head(tags$style(HTML("#tabs li a[data-value = 'ft_type'], #tabs li a[data-value = 'bound_yr'], #tabs li a[data-value = 'ft_value'], #tabs li a[data-value = 'pred_yr'], #tabs li a[data-value = 'stat_type'], #tabs li a[data-value = 'population'], #tabs li a[data-value = 'review'], #tabs li a[data-value = 'make_rpt'] {
                             display: none;
                                                     }"))),
                   shinyjs::useShinyjs(),
                   tags$style(".btn {
                     background-color: orange;
                                }"),
                   shiny::headerPanel('Springtides'),
                   shiny::sidebarPanel(
                     shiny::p(""),
                     ## Who
                     shiny::uiOutput("who_heading_chr"),
                     shiny::uiOutput("who_instr_chr"),
                     shiny::uiOutput("who_selected_chr"),
                     ## What
                     shiny::uiOutput("what_heading_chr"),
                     shiny::uiOutput("what_instr_chr"),
                     shiny::uiOutput("what_selected_chr"),
                     ## When
                     shiny::uiOutput("when_heading_chr"),
                     shiny::uiOutput("when_instr_chr"),
                     shiny::uiOutput("when_selected_chr"),
                     ## Where
                     shiny::uiOutput("welcome_chr"),
                     shiny::uiOutput("selected_pa_type_chr"),
                     shiny::uiOutput("ft_select_chr"),
                     #uiOutput("ft_select_hss_chr"),
                     shiny::uiOutput("selected_ft_type_chr"),
                     # uiOutput("selected_ft_type_hss_chr"),
                     shiny::uiOutput("bound_yr_select_chr"),
                     shiny::uiOutput("selected_bound_yr_chr"),
                     shiny::uiOutput("ft_value_select_chr"),
                     shiny::uiOutput("selected_ft_values_chr"),
                     shiny::uiOutput("about_chr")
                   ),
                   shiny::mainPanel(
                     shiny::tabsetPanel(type = "tabs",
                                        id = "tabs",
                                        shiny::tabPanel("Select type of geometry",
                                                        value = "geom_type",
                                                        shiny::selectInput("pa_type_chr", h3("Geometry"),
                                                                           choices = list("Select from a menu of existing options" = "Predefined boundary",
                                                                                          "Generate your own" = "HSS"
                                                                                          # ,
                                                                                          # "Base on proximity to custom coordinates" = "Custom"
                                                                           ),
                                                                           selected = "Predefined boundary"),
                                                        shiny::actionButton("confirmWhere1", "Confirm the type of geometry to use -->>")
                                        ),
                                        shiny::tabPanel("Select area feature type",
                                                        value = "ft_type",
                                                        shiny::p(""),
                                                        shiny::actionButton("returnToWhere1", "<<-- Go back to change type of geometry to use"),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr == \"Predefined boundary\"",
                                                          shiny::uiOutput("predefinedControls")
                                                        ),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr == \"HSS\"",
                                                          shiny::uiOutput("headspaceControls")
                                                        ),
                                                        shiny::uiOutput("conditionalGeomFt1Nav")
                                        ),
                                        shiny::tabPanel("Select boundary year",
                                                        value = "bound_yr",
                                                        shiny::p(""),
                                                        shiny::actionButton("returnToWhere2", "<<-- Go back and change the type of features to use"),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr == \"Predefined boundary\"",
                                                          shiny::uiOutput("boundYearControls")
                                                        ),
                                                        shiny::actionButton("confirmYear", "Confirm boundary year selection -->>")
                                        ),
                                        shiny::tabPanel("Select feature",
                                                        value = "ft_value",
                                                        shiny::p(""),
                                                        shiny::actionButton("returnToWhere2_2", "<<-- Go back and change the type of features to use"),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr == \"Predefined boundary\"",
                                                          #shiny::uiOutput("boundYearControls"),
                                                          shiny::uiOutput("areaFilterControls"),
                                                          shiny::uiOutput("areaControls")
                                                        ),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr != \"Predefined boundary\"",
                                                          shiny::selectInput("gdist_ttime_chr", h3("Proximity measure:"),
                                                                             choices = list("Maximium geometric distance" = "Geometric distance",
                                                                                            "Maximum drive time" = "Travel time"),
                                                                             selected = "Geometric distance")
                                                        ),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr != \"Predefined boundary\" & input.gdist_ttime_chr == \"Geometric distance\"",
                                                          shiny::sliderInput("gdist_dbl", "Geometric distance in Kilometres",
                                                                             min = 10, max = 50, value = 20)
                                                        ),
                                                        shiny::conditionalPanel(
                                                          condition = "input.pa_type_chr != \"Predefined boundary\" & input.gdist_ttime_chr == \"Travel time\"",
                                                          shiny::sliderInput("ttime_dbl", "Drive time in minutes",
                                                                             min = 15, max = 60, value = 20)
                                                        ),
                                                        shiny::actionButton("confirmWhere3", "Confirm feature selection -->>")
                                        ),
                                        shiny::tabPanel("Select the prediction years",
                                                        value = "pred_yr",
                                                        shiny::p(""),
                                                        shiny::actionButton("returnToWhere", "<<-- Go back and change 'Where' settings"),
                                                        # shiny::sliderInput("year_x_dbl", h3("Prediction year"),
                                                        #                    min = 2020, max = 2031, value = 2020, sep = ""
                                                        # ),
                                                        shiny::uiOutput("when_controls"),
                                                        #shiny::textOutput("when_valid_chr"),
                                                        shiny::uiOutput("confirm_when_controls")
                                                        #shiny::actionButton("confirmWhen", "Confirm the prediction years -->>")
                                        ),
                                        shiny::tabPanel("Select the type of statistics",
                                                        value = "stat_type",
                                                        shiny::p(""),
                                                        shiny::actionButton("returnToWhen", "<<-- Go back and change 'When' settings"),
                                                        shiny::uiOutput("statisticControls"),
                                                        shiny::sliderInput("n_its_int", "Number of simulation iterations", 1, 100, 10),
                                                        shiny::sliderInput("uncertainty_int", "Uncertainty Interval",
                                                                           min = 0.01, max = 1, value = c(0.025,0.975), step = 0.005),
                                                        shiny::actionButton("confirmWhat", "Confirm the statistics to be generated -->>")
                                        ),
                                        shiny::tabPanel("Select the population",
                                                        value = "population",
                                                        shiny::p(""),
                                                        shiny::actionButton("returnToWhat", "<<-- Go back and change 'What' settings"),
                                                        shiny::uiOutput("disorderControls"),
                                                        shiny::uiOutput("ageRangeControls"),
                                                        shiny::actionButton("confirmWho", "Confirm the population to be simulated -->>")
                                        ),
                                        shiny::tabPanel("Review",
                                                        value = "review",
                                                        shiny::p(),
                                                        shiny::actionButton("returnToWho", "<<-- Go back and change 'Who' settings"),
                                                        shiny::p(),
                                                        shiny::p("You have provided all the information that is required to generate a profile."),
                                                        shiny::p("Take a moment to review the settings that you have entered."),
                                                        shiny::p("This is your last opportunity to change these settings prior to them being passed to the simulation."),
                                                        shiny::p(),
                                                        shiny::actionButton("confirmAll", "Confirm these are the settings you wish to use -->>")
                                        ),
                                        shiny::tabPanel("Make report",
                                                        value = "make_rpt",
                                                        shiny::p(),
                                                        shiny::h1("Customise report"),
                                                        shiny::textInput("user_name_chr", "Your name / your organisation's name", value = "Anonymous User"),
                                                        shiny::uiOutput("areaNameControls"),
                                                        # shiny::conditionalPanel(
                                                        #   condition = "input.pa_type_chr != \"Predefined boundary\"",
                                                        #   shiny::textInput("area_name_chr", "Name of the custom geometry that you are profiling", value = "Custom Area")
                                                        # ),
                                                        shiny::p("Note: It will take between 5 and 20 minutes to generate your report."),
                                                        shiny::radioButtons("report_format_chr","Report format:",
                                                                            c("PDF" = "PDF",
                                                                              "HTML" = "HTML",
                                                                              "Word" = "Word"), inline=T)
                                                        ,
                                                        shiny::downloadButton("report", "Generate a report")
                                        ),
                                        shiny::tabPanel("About Springtides",
                                                        value = "about_springtides",
                                                        shiny::p(),
                                                        shiny::h1("App and model"),
                                                        shiny::p("The Springtides App is a simple user interface to a computer simulation model of the epidemiology of mental and substance use disorders in young people, also called Springtides. When using this app you are asked a number of questions about the type of epidemiological profile that you would like. Your answers are then passed to an instance of the simulation model and a report is generated for you to download. That report provides both a summary of the model results and an overview of the data and algorithms that produced those results."),
                                                        shiny::h1("Modelling framework and development context"),
                                                        shiny::p("The Springtides app and simulation model were developed by Orygen in the statistical software R using Orygen's readyforwhatsnext open source modelling framework. The source code for both Springtides and readyforwhatsnext is due for public release as R packages later in 2020. Currently, access to these code libraries is by invitation only as testing is currently ongoing. This current version of the Springtides model is supported by an Australian data pack. However, the algorithms that run the model are compatible with data packs from other jurisdictions."),
                                                        shiny::h1("Reporting bugs and feature requests"),
                                                        shiny::p("You are currently using a development version of the Springtides App, which means that verification and validation checks are ongoing. Results produced by this app should be interpreted with care and you are encouraged to cross reference findings with other data sources. We'd greatly appreciate you reporting any suspected errors or suggested improvements to the Springtides development team. Email matthew.hamilton@orygen.org.au")
                                        )
                     ))

  )
}
#' @title make_inactivity_script_chr
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_inactivity_script_chr
#' @export

make_inactivity_script_chr <- function(){
  # Passsword authentication based on: https://stackoverflow.com/questions/28987622/starting-shiny-app-after-password-input
"function idleTimer() {
var t = setTimeout(logout, 3600000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 3600000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"
}
#' @title make_ui_fn
#' @description FUNCTION_DESCRIPTION
#' @param secure_lgl PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shinymanager]{secure-app}}
#' @rdname make_ui_fn
#' @export
#' @importFrom shinymanager secure_app
make_ui_fn <- function(secure_lgl){
  if(secure_lgl){
    shinymanager::secure_app(head_auth = tags$script(make_inactivity_script_chr()),
                             ui = make_ui_body_fn())
  }else{
    make_ui_body_fn()
  }
}
#' @title make_basic_ui_fn
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{fluidPage}},\code{\link[shiny]{headerPanel}},\code{\link[shiny]{htmlOutput}},\code{\link[shiny]{sliderInput}},\code{\link[shiny]{textInput}},\code{\link[shiny]{builder}},\code{\link[shiny]{radioButtons}},\code{\link[shiny]{downloadButton}},\code{\link[shiny]{selectInput}},\code{\link[shiny]{conditionalPanel}}
#'  \code{\link[shinythemes]{shinytheme}}
#' @rdname make_basic_ui_body_fn
#' @export
#' @importFrom shiny fluidPage headerPanel uiOutput sliderInput textInput p radioButtons downloadButton selectInput conditionalPanel
#' @importFrom shinythemes shinytheme
make_basic_ui_body_fn <- function(){
  shiny::fluidPage(theme = shinythemes::shinytheme("journal"),
            tags$style(".btn {
                     background-color: orange;
                                }"),
            shiny::headerPanel('Springtides'),

            # App title ----
            # titlePanel("Tabsets"),

            # Sidebar layout with input and output definitions ----
            sidebarLayout(

              # Sidebar panel for inputs ----
              sidebarPanel(
                shiny::uiOutput("statisticControls"),
                shiny::uiOutput("disorderControls"),
                shiny::uiOutput("ageRangeControls"),
                shiny::uiOutput("when_controls"),
                shiny::sliderInput("n_its_int", "Number of simulation iterations", 1, 100, 10),
                shiny::sliderInput("uncertainty_int", "Uncertainty Interval",
                                   min = 0.01, max = 1, value = c(0.025,0.975), step = 0.005),
                shiny::textInput("user_name_chr", "Your name / your organisation's name", value = "Anonymous User"),
                shiny::uiOutput("areaNameControls"),
                shiny::p("Note: It will take between 5 and 20 minutes to generate your report."),
                shiny::radioButtons("report_format_chr","Report format:",
                                    c("PDF" = "PDF",
                                      "HTML" = "HTML",
                                      "Word" = "Word"), inline=T),
                shiny::downloadButton("report", "Generate a report")
              ),

              # Main panel for displaying outputs ----
              mainPanel(

                # Output: Tabset w/ plot, summary, and table ----
                tabsetPanel(type = "tabs",
                            # tabPanel("Plot", plotOutput("plot")),
                            # tabPanel("Summary", verbatimTextOutput("summary")),
                            tabPanel("Area to profile",
                                     shiny::selectInput("pa_type_chr", h3("Type of geometry"),
                                                        choices = list("PHN Boundary" = "Predefined boundary",
                                                                       "Poximity to Headspace Centres" = "HSS"
                                                                       # ,
                                                                       # "Base on proximity to custom coordinates" = "Custom"
                                                        ),
                                                        selected = "Predefined boundary"),
                                     shiny::conditionalPanel(
                                       condition = "input.pa_type_chr == \"Predefined boundary\"",
                                       shiny::uiOutput("areaControls")
                                     ),
                                     shiny::conditionalPanel(
                                       condition = "input.pa_type_chr == \"HSS\"",
                                       shiny::uiOutput("headspaceControls"),
                                       # shiny::selectInput("pa_type_chr", h3("Geometry"),
                                       #                    choices = list("Select from a menu of existing options" = "Predefined boundary",
                                       #                                   "Generate your own" = "HSS"
                                       #                                   # ,
                                       #                                   # "Base on proximity to custom coordinates" = "Custom"
                                       #                    ),
                                       #                    selected = "Predefined boundary"),
                                       shiny::conditionalPanel(
                                         condition = "input.pa_type_chr != \"Predefined boundary\"",
                                         shiny::selectInput("gdist_ttime_chr", h3("Proximity measure"),
                                                            choices = list("Maximium geometric distance" = "Geometric distance",
                                                                           "Maximum drive time" = "Travel time"),
                                                            selected = "Geometric distance")
                                       ),
                                       shiny::conditionalPanel(
                                         condition = "input.pa_type_chr != \"Predefined boundary\" & input.gdist_ttime_chr == \"Geometric distance\"",
                                         shiny::sliderInput("gdist_dbl", "Geometric distance in Kilometres",
                                                            min = 10, max = 50, value = 20)
                                       ),
                                       shiny::conditionalPanel(
                                         condition = "input.pa_type_chr != \"Predefined boundary\" & input.gdist_ttime_chr == \"Travel time\"",
                                         shiny::sliderInput("ttime_dbl", "Drive time in minutes",
                                                            min = 15, max = 60, value = 20)
                                       )
                                     )
                            )

                )

              )
            )
  )
}
#' @title make_basic_ui_fn
#' @description FUNCTION_DESCRIPTION
#' @param secure_lgl PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shinymanager]{secure-app}}
#' @rdname make_basic_ui_fn
#' @export
#' @importFrom shinymanager secure_app
make_basic_ui_fn <- function(secure_lgl){
  if(secure_lgl){
    shinymanager::secure_app(head_auth = tags$script(make_inactivity_script_chr()),
                             ui = make_basic_ui_body_fn())
  }else{
    make_basic_ui_body_fn()
  }
}
