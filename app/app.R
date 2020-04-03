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

