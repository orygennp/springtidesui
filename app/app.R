library(shiny)
library(springtidesui)
options(shiny.maxRequestSize=100*1024^2)
launch_app(r_data_dir_chr = normalizePath("C:/Users/mahamilton/Desktop/Readyforwhatsnext/Data/R_Format"),
           credentials_tb = data.frame(
             user = c("1", "orygenNP", "orygenHSR", "OrygenPol","RFWN"),
             password = c("000", "Melton", "FirstBounce", "Electorate","Framework"),
             stringsAsFactors = FALSE))

