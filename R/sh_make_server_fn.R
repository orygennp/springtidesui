#' @title filter_by_map_view_sf
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param bounds PARAM_DESCRIPTION
#' @param col_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_crop}}
#'  \code{\link[dplyr]{pull}},\code{\link[dplyr]{filter}}
#'  \code{\link[rlang]{sym}}
#' @rdname filter_by_map_view_sf
#' @export
#' @importFrom sf st_crop
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
filter_by_map_view_sf <- function(sf,
                                  bounds,
                                  col_chr) {
  cropped_sf <- sf %>%
    sf::st_crop(c(xmin = bounds$west,
                  ymin = bounds$south,
                  xmax = bounds$east,
                  ymax = bounds$north))
  inc_chr_vec <- cropped_sf %>%
    dplyr::pull(!!rlang::sym(col_chr)) %>%
    unique()
  sf %>%
    dplyr::filter(!!rlang::sym(col_chr) %in% inc_chr_vec)
}

## Modified from: https://rstudio.github.io/leaflet/markers.html
get_color_chr <- function(sf,
                     col_chr = "service_name",
                     value_chr_vec = NA_character_) {
  if(ifelse(identical(value_chr_vec, character(0)),T,is.na(value_chr_vec))){
    "green"
  }else{
    purrr::map_chr(sf %>% dplyr::pull(!!rlang::sym(col_chr)),
                   ~ ifelse(.x %in% value_chr_vec,
                   "red",
                   "green"))
  }
}
make_points_text_chr <- function(micro_chr_vec,
                                 pa_type_chr = "Predefined boundary",
                                 gdist_ttime_chr = NULL,
                                 gdist_dbl = NULL,
                                 ttime_dbl = NULL,
                                 inc_proximity_lgl = T){
  gdist_dbl <- ifelse(pa_type_chr=="Predefined boundary",
                      NA_real_,
                      ifelse(is.null(gdist_dbl),
                             NA_real_,
                             gdist_dbl))
  ttime_dbl <- ifelse(pa_type_chr=="Predefined boundary",
                      NA_real_,
                      ifelse(is.null(ttime_dbl),
                             NA_real_,
                             ttime_dbl))
  gdist_ttime_chr <- ifelse(pa_type_chr=="Predefined boundary",
                            NA_character_,
                            ifelse(is.null(gdist_ttime_chr),
                                   NA_character_,
                                   gdist_ttime_chr)
  )
  if(identical(micro_chr_vec,character(0))){#is.null(input$micro_chr_vec)
    points_chr <- "" # Replace with numeric count of points for custom coordinates and >5 services
  }else{
    points_chr <- micro_chr_vec %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed( ',', ' and')
  }
  if(points_chr == ""){
    points_chr
  }else{
    proximity_chr <- ifelse(inc_proximity_lgl,
                            paste0("Area within ",
                            ifelse(is.na(gdist_ttime_chr),
                                   "",
                                   ifelse(gdist_ttime_chr=="Geometric distance",
                                          paste0(gdist_dbl, " Km "),
                                          paste0(ttime_dbl, " Minutes Drive "))),
                            "of "),
                            "")
    paste0(proximity_chr,
           points_chr,
           ifelse(T,
                  paste0(" Headspace Centre",ifelse(length(micro_chr_vec)>1,"s","")), # Abstract
                  " Custom Coordinates")) # Replace with condition logic for custom coordinates
  }
}
#' @title make_basic_server_fn
#' @description FUNCTION_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @param credentials_tb PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shinymanager]{secure-app}},\code{\link[shinymanager]{check_credentials}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[ready4space]{get_data}}
#'  \code{\link[sf]{st_crop}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[shiny]{reactive}},\code{\link[shiny]{reactiveValues}},\code{\link[shiny]{observeEvent}},\code{\link[shiny]{renderUI}},\code{\link[shiny]{tag}},\code{\link[shiny]{conditionalPanel}},\code{\link[shiny]{sliderInput}},\code{\link[shiny]{builder}},\code{\link[shiny]{selectInput}},\code{\link[shiny]{actionButton}},\code{\link[shiny]{checkboxGroupInput}},\code{\link[shiny]{wellPanel}},\code{\link[shiny]{textInput}},\code{\link[shiny]{radioButtons}},\code{\link[shiny]{downloadButton}},\code{\link[shiny]{downloadHandler}},\code{\link[shiny]{HTML}},\code{\link[shiny]{htmlOutput}},\code{\link[shiny]{dateRangeInput}},\code{\link[shiny]{renderPrint}},\code{\link[shiny]{reactiveValuesToList}}
#'  \code{\link[leaflet]{leaflet}},\code{\link[leaflet]{setView}},\code{\link[leaflet]{addProviderTiles}},\code{\link[leaflet]{leafletProxy}},\code{\link[leaflet]{addControl}},\code{\link[leaflet]{removeControl}},\code{\link[leaflet]{leafletOutput}}
#'  \code{\link[stringi]{stri_replace_all}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[springtides]{filter_if_var_exists}},\code{\link[springtides]{get_input_vec}},\code{\link[springtides]{get_disorder_choices_chr_vec}},\code{\link[springtides]{get_age_range_choices_int_vec}},\code{\link[springtides]{subset_vec_if_var_exists}},\code{\link[springtides]{make_area_name_chr}},\code{\link[springtides]{tf_stat_chr}},\code{\link[springtides]{make_output_params_ls}},\code{\link[springtides]{get_input_ls}},\code{\link[springtides]{make_stat_choices_ls}}
#'  \code{\link[purrr]{reduce}},\code{\link[purrr]{map}},\code{\link[purrr]{flatten}}
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_replace}}
#'  \code{\link[lubridate]{as_date}},\code{\link[lubridate]{period}}
#'  \code{\link[rmarkdown]{render}},\code{\link[rmarkdown]{pdf_document}},\code{\link[rmarkdown]{html_document}},\code{\link[rmarkdown]{word_document}}
#' @rdname make_basic_server_fn
#' @export
#' @importFrom shinymanager secure_server check_credentials
#' @importFrom dplyr filter pull
#' @importFrom ready4space get_data
#' @importFrom sf st_crop
#' @importFrom rlang sym
#' @importFrom shiny reactive reactiveValues observeEvent renderUI tagList conditionalPanel sliderInput p selectInput h3 actionButton checkboxGroupInput wellPanel textInput radioButtons downloadButton downloadHandler HTML uiOutput dateRangeInput renderPrint reactiveValuesToList
#' @importFrom leaflet leaflet setView addProviderTiles providerTileOptions leafletProxy addPolygons highlightOptions clearShapes renderLeaflet
#' @importFrom stringi stri_replace_last_fixed
#' @importFrom ready4utils data_get
#' @importFrom springtides filter_if_var_exists get_input_vec get_disorder_choices_chr_vec get_age_range_choices_int_vec subset_vec_if_var_exists make_area_name_chr tf_stat_chr make_output_params_ls get_input_ls make_stat_choices_ls
#' @importFrom purrr reduce map_chr flatten_chr
#' @importFrom stringr str_sub str_replace_all
#' @importFrom lubridate as_datetime years
#' @importFrom rmarkdown render pdf_document html_document word_document
make_basic_server_fn <- function(r_data_dir_chr,
                           credentials_tb = NULL){
  function(input, output, session){
    if(!is.null(credentials_tb)){
      result_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials_tb))
    }
      data_pckg_chr <- "springtides" # Abstract
      pa_r4_chr <- "aus_pa_r4"
      data(list = pa_r4_chr,
           package = data_pckg_chr,
           envir = environment())
      eval(parse(text = paste0("pa_r4<-",pa_r4_chr)))
      ## Reactive Functions
      cropSf <- shiny::reactive({
        shiny::req(input$pa_type_chr)
        if(input$pa_type_chr !="HSS"){
          sf <- getPolysSf()
          sp_unit_var_chr <- getSpUnitVarChr()
        }else{
          sf <- getPointsSf()
          sp_unit_var_chr <- "service_name"
        }
        if(is.null(input$map_bounds)) {
          sf
        } else {
          bounds <- input$map_bounds
          filter_by_map_view_sf(sf,
                                bounds,
                                col_chr = sp_unit_var_chr)
        }
      })
      getSpUnitVarChr <- shiny::reactive({
        shiny::req(input$meso2_type_chr)
        shiny::req(input$meso2_bound_yr)
          pa_r4@lookup_tb@sp_uid_lup %>%
            ready4space::get_data(value_chr = pa_r4@lookup_tb@sp_abbreviations_lup %>%
                                    ready4space::get_data(col_chr = "long_name",
                                                          value_chr = input$meso2_type_chr), #"PHN",
                                  area_bound_yr = input$meso2_bound_yr)


      })
      getPointsSf <- shiny::reactive({
        #shiny::req(input$type_of_service_chr)
        ready4space::get_data(pa_r4@lookup_tb@sp_data_pack_lup,
                              col_chr = "area_type",
                              value_chr = pa_r4@lookup_tb@sp_abbreviations_lup %>%
                                ready4space::get_data(col_chr = "long_name",
                                                      value_chr = input$type_of_service_chr #%>% paste0(" Service")
                                                      ), # Need to reconcile names in coordinates_lup and abbreviations
                              r_data_dir_chr = r_data_dir_chr)
      })
      getPolysSf <- shiny::reactive({
        shiny::req(input$meso2_bound_yr)
        shiny::req(input$meso2_type_chr)
          filtered_dp_lup <- pa_r4@lookup_tb@sp_data_pack_lup %>%
            dplyr::filter(area_bound_yr == input$meso2_bound_yr)
          ready4space::get_data(filtered_dp_lup,
                                r_data_dir_chr = r_data_dir_chr,
                                col_chr = "area_type",
                                value_chr = pa_r4@lookup_tb@sp_abbreviations_lup %>%
                                  ready4space::get_data(col_chr = "long_name",
                                                        value_chr = input$meso2_type_chr)
                                )
      })
      makeBaseMap <- shiny::reactive({
        leaflet::leaflet() %>%
          leaflet::setView(lng = 133.25, lat = -27.15, zoom = 4) %>%
          leaflet::addProviderTiles("CartoDB.DarkMatter",
                                    layerId = "basetile",
                                    options = leaflet::providerTileOptions(minZoom = 3)
          )
      })
      ## Reactive Lists
      markerClickLs <- shiny::reactiveValues(ids = vector())
      polysClickLs <- shiny::reactiveValues(ids = vector())
      reactive_ls <- shiny::reactiveValues(coordinates_chr_vec = character(0))
      ## Observers
      shiny::observeEvent(input$confirmZoom,{ #
       # shiny::req(input$type_of_service_chr)
        if(length(input$type_of_service_chr)==0){
          output$map <- leaflet::renderLeaflet({
            polysClickLs$ids <- NULL
            markerClickLs$ids <- NULL
            makeBaseMap()
          })
        }else{
          points_sf <- cropSf()#getPointsSf()
          if(nrow(points_sf)>0){
            leaflet::leafletProxy("map", data = points_sf) %>%
              leaflet::clearShapes() %>%
              addAwesomeMarkers(data = points_sf,
                                layerId = ~ service_name,
                                popup = ~ paste0(service_name," ",service_type," in ",PHN_NAME, " PHN"), ## ABSTRACT
                                label = ~ as.character(service_name),
                                icon=awesomeIcons(
                                  icon = 'ios-close',
                                  iconColor = 'black',
                                  library = 'ion',
                                  markerColor = get_color_chr(points_sf)
                                )
              )
          }
        }
        #shiny::req(input$type_of_service_chr)
      })
      # Event observers
      shiny::observeEvent(input$changeDisorder, {
        reactive_ls$disorderOnDbl <- 0
        reactive_ls$reportDbl <- ifelse(is.null(reactive_ls$reportDbl),0,max(reactive_ls$reportDbl-1,0))
      })
      shiny::observeEvent(input$changeStatistic, {
        reactive_ls$statisticOnDbl <- 0
        reactive_ls$reportDbl <- ifelse(is.null(reactive_ls$reportDbl),0,max(reactive_ls$reportDbl-1,0))
        reactive_ls$disorder_choices_vec <- NULL
      })
      shiny::observeEvent(input$changeWhen, {
        reactive_ls$whenOnDbl <- 0
        reactive_ls$reportDbl <- ifelse(is.null(reactive_ls$reportDbl),0,max(reactive_ls$reportDbl-1,0))
      })
      shiny::observeEvent(input$confirmDisorder, {
        reactive_ls$disorderOnDbl <- 1
        reactive_ls$reportDbl <- ifelse(is.null(reactive_ls$reportDbl),1,min(reactive_ls$reportDbl+1,3))
      })
      shiny::observeEvent(input$confirmStatistic, {
        shiny::req(input$stat_chr)
        reactive_ls$statisticOnDbl <- 1
        reactive_ls$reportDbl <- ifelse(is.null(reactive_ls$reportDbl),1,min(reactive_ls$reportDbl+1,3))
        reactive_ls$disorder_choices_vec <- springtides::get_input_vec(fn = springtides::get_disorder_choices_chr_vec, # Abstract
                                                                       args = list(lup_r4 = pa_r4@lookup_tb,
                                                                                   path_to_data_chr = r_data_dir_chr,
                                                                                   stat_chr = input$stat_chr,
                                                                                   sex_chr_vec = c("Female","Male")),
                                                                       n = Inf) %>% stringr::str_replace_all("_"," ") %>%
          sort()
      })
      shiny::observeEvent(input$confirmWhen, {
        reactive_ls$whenOnDbl <- 1
        reactive_ls$reportDbl <- ifelse(is.null(reactive_ls$reportDbl),1,min(reactive_ls$reportDbl+1,3))
      })

      shiny::observeEvent(input$confirmYear, {
        shiny::req(input$meso2_type_chr)
        reactive_ls$geom_nav_dbl <- 1
        sp_unit_var_chr <- getSpUnitVarChr()#"PHN_NAME"#
        data_sf <- cropSf()
        leaflet::leafletProxy("map", data = data_sf) %>%
          leaflet::clearShapes() %>%
          leaflet::addPolygons(fillColor =  "orange",
                               label = ~ paste(input$meso2_type_chr,":", data_sf %>% dplyr::pull(!!rlang::sym(sp_unit_var_chr))),
                               color = "#BDBDC3",
                               layerId = eval(parse(text = paste0("~ ", sp_unit_var_chr))),
                               # weight = 1
                               stroke = T,
                               fillOpacity = 0.5,
                               highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
          )
      })
      shiny::observeEvent(input$confirmYear, {
        shiny::req(input$meso2_type_chr)
        shiny::req(input$meso2_bound_yr)
        bnd_sf <- readRDS(reactive_ls$meso2_choices_ls$meso2_type_tb %>%
                            dplyr::filter(area_bound_yr == input$meso2_bound_yr) %>%
                            dplyr::pull(source_reference) %>%
                            paste0(r_data_dir_chr,
                                   "/",
                                   .,".rds"
                            )) %>%
          springtides::filter_if_var_exists(var_chr = "STE_NAME16", ### ABSTRACT
                                            var_val_xxx = "Other Territories",
                                            cond_chr = "!=") %>%
          purrr::reduce(c("2899","6798","6799","7151"),# Abstract
                        .init = .,
                        ~ .x %>% springtides::filter_if_var_exists(var_chr = "POA_NAME",
                                                                   var_val_xxx = .y,
                                                                   cond_chr = "!=")) %>%
          purrr::reduce(c("2899","6798","6799","7151"),# Abstract
                        .init = .,
                        ~ .x %>% springtides::filter_if_var_exists(var_chr = "POA_NAME16",
                                                                   var_val_xxx = .y,
                                                                   cond_chr = "!="))

        reactive_ls$meso2_chr_choices_vec <- bnd_sf  %>%
          dplyr::pull(!!rlang::sym(reactive_ls$meso2_choices_ls$meso2_uid_tb %>%
                                     dplyr::filter(year == input$meso2_bound_yr) %>%
                                     dplyr::pull(var_name))) %>%
          sort()
        if(ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
                                 lookup_variable = "long_name",
                                 lookup_reference = input$meso2_type_chr,
                                 target_variable = "short_name",
                                 evaluate = F) %in% (pa_r4@lookup_tb@sp_resolution_lup %>%
                                                     dplyr::filter(area_count >800) %>%
                                                     dplyr::pull(area_type) %>%
                                                     unique())){
          reactive_ls$meso2_filter_choices_chr_vec <- reactive_ls$meso2_chr_choices_vec %>%
            stringr::str_sub(start=1,end=2) %>%
            unique() %>%
            sort()
        }else{
          reactive_ls$meso2_filter_choices_chr_vec <- NULL
        }
        reactive_ls$geom_nav_dbl <- 3
      })
      shiny::observeEvent(input$confirmWhere2, {
        shiny::req(input$pa_type_chr)
        shiny::req(input$meso2_type_chr)
        if(is.null(reactive_ls$pa_type_chr)){
          test_lgl <- F
        }else{
          test_lgl <- input$pa_type_chr=="HSS" ## ABSTRACT
        }
        if(!test_lgl){
          meso2_type_tb <- pa_r4@lookup_tb@sp_data_pack_lup %>%
            dplyr::filter(main_feature == "Boundary") %>%
            dplyr::filter(area_type == input$meso2_type_chr %>%
                            ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
                                                  lookup_variable = "long_name",
                                                  lookup_reference = .,
                                                  target_variable = "short_name",
                                                  evaluate = F))
          meso2_bound_yr_chr_vec <- meso2_type_tb %>%
            dplyr::pull(area_bound_yr) %>%
            unique() %>%
            sort()
          meso2_uid_tb <- pa_r4@lookup_tb@sp_uid_lup %>%
            dplyr::filter(spatial_unit == input$meso2_type_chr %>%
                            ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
                                                  lookup_variable = "long_name",
                                                  lookup_reference = .,
                                                  target_variable = "short_name",
                                                  evaluate = F))
          reactive_ls$meso2_choices_ls <- list(meso2_type_tb = meso2_type_tb,
                                               meso2_bound_yr_chr_vec = meso2_bound_yr_chr_vec,
                                               meso2_uid_tb = meso2_uid_tb)
          reactive_ls$geom_nav_dbl <- 2
        }
      })
      shiny::observeEvent(input$map_shape_click, {
        shiny::req(input$meso2_type_chr)
        sp_unit_var_chr <- getSpUnitVarChr()
        polys_sf <- getPolysSf()
        click <- input$map_shape_click
        if(click$id %in% polysClickLs$ids){
          polysClickLs$ids <- polysClickLs$ids[!polysClickLs$ids %in% click$id]
          if(is.null(click$id) ){
            req(click$id)
          }else {
            unclicked_sf <- polys_sf %>% dplyr::filter(!!rlang::sym(sp_unit_var_chr) == click$id)
            leaflet::leafletProxy(mapId = "map" ) %>%
              leaflet::addPolygons(data = unclicked_sf,
                                   fillColor =  "orange",
                                   label = ~ paste(input$meso2_type_chr,":", unclicked_sf %>% dplyr::pull(!!rlang::sym(sp_unit_var_chr))),
                                   color = "#BDBDC3",
                                   layerId = eval(parse(text = paste0("~ ", sp_unit_var_chr))),
                                   # weight = 1
                                   stroke = T,
                                   fillOpacity = 0.5,
                                   highlightOptions = leaflet::highlightOptions(color = "white", weight = 2,bringToFront = TRUE)
              )
          }
        }else{
          polysClickLs$ids <- c(polysClickLs$ids, click$id) %>% sort()
          clicked_sf <- polys_sf %>% dplyr::filter(!!rlang::sym(sp_unit_var_chr) %in% polysClickLs$ids)
          if( is.null( click$id ) ){
            req( click$id )
          } else {
            leaflet::leafletProxy(mapId = "map" ) %>%
              leaflet::addPolygons(data = clicked_sf,
                                   fillColor =  "red",
                                   label = paste(input$meso2_type_chr,":", clicked_sf %>% dplyr::pull(!!rlang::sym(sp_unit_var_chr))),
                                   color = "#BDBDC3",
                                   layerId = eval(parse(text = paste0("~ ", sp_unit_var_chr))),
                                   # weight = 1
                                   stroke = T,
                                   fillOpacity = 0.5,
                                   highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
              )
          }
        }
      })
      shiny::observeEvent(input$map_marker_click, {
        points_sf <- getPointsSf()
        sp_unit_var_chr <- "service_name"
        click <- input$map_marker_click
        if(click$id %in% markerClickLs$ids){
          markerClickLs$ids <- markerClickLs$ids[!markerClickLs$ids %in% click$id]
          if(is.null(click$id) ){
            req(click$id)
          }else {
            unclicked_sf <- points_sf %>% dplyr::filter(!!rlang::sym(sp_unit_var_chr) == click$id)
            leaflet::leafletProxy(mapId = "map" ) %>%
              #leaflet::clearShapes() %>%
              addAwesomeMarkers(data = points_sf,
                                layerId = ~ service_name,
                                label = ~ paste0(service_name," ",service_type," in ",PHN_NAME, " PHN"), ## ABSTRACT
                                icon= awesomeIcons(
                                  icon = 'ios-close',
                                  iconColor = 'black',
                                  library = 'ion',
                                  markerColor = get_color_chr(points_sf,
                                                              value_chr_vec = markerClickLs$ids)
                                ))
          }
        }else{
          markerClickLs$ids <- c(markerClickLs$ids, click$id) %>% sort()
          clicked_sf <- points_sf %>% dplyr::filter(!!rlang::sym(sp_unit_var_chr) %in% markerClickLs$ids)
          if( is.null( click$id ) ){
            req( click$id )
          } else {
            leaflet::leafletProxy(mapId = "map" ) %>%
              addAwesomeMarkers(data = points_sf,
                                layerId = ~ service_name,
                                label = ~ paste0(service_name," ",service_type," in ",PHN_NAME, " PHN"), ## ABSTRACT
                                icon= awesomeIcons(
                                  icon = 'ios-close',
                                  iconColor = 'black',
                                  library = 'ion',
                                  markerColor = get_color_chr(points_sf,
                                                              value_chr_vec = markerClickLs$ids)
                                ))
          }
        }
      })
      shiny::observeEvent(input$pa_type_chr, {
        reactive_ls$geom_nav_dbl <- 1
        reactive_ls$meso2_choices_ls <- NULL
        reactive_ls$meso2_bound_yr <- NULL
        reactive_ls$meso2_chr_choices_vec <- NULL
        reactive_ls$meso2_first_chr <- NULL
        output$map <- leaflet::renderLeaflet({
          polysClickLs$ids <- NULL
          markerClickLs$ids <- NULL
          makeBaseMap()
        })
      })
      shiny::observeEvent(input$returnToWhere, {
        reactive_ls$geom_nav_dbl <- 1
        reactive_ls$meso2_choices_ls <- NULL
        reactive_ls$meso2_bound_yr <- NULL
        reactive_ls$meso2_chr_choices_vec <- NULL
        reactive_ls$meso2_first_chr <- NULL
        output$map <- leaflet::renderLeaflet({
          polysClickLs$ids <- NULL
          markerClickLs$ids <- NULL
          makeBaseMap()
        })
      })
      shiny::observeEvent(input$type_of_custom_geom_chr, {
        output$map <- leaflet::renderLeaflet({
          polysClickLs$ids <- NULL
          markerClickLs$ids <- NULL
          makeBaseMap()
        })
      })
      ## Outputs
      output$ageRangeControls <- shiny::renderUI({
        shiny::req(input$confirmDisorder)

        if(ifelse(is.null(input$confirmDisorder),T,input$confirmDisorder==0)){
          age_vec = c(18,19)
        }else{
          shiny::req(input$stat_chr)
          shiny::req(input$disorder_chr)
          age_vec <- springtides::get_age_range_choices_int_vec(lup_r4 = pa_r4@lookup_tb, # ABSTRACT
                                                                path_to_data_chr = r_data_dir_chr,
                                                                stat_chr = input$stat_chr,
                                                                disorder_chr = input$disorder_chr %>% #
                                                                  stringr::str_replace_all(" ","_"))
        }
        shiny::tagList(
          shiny::conditionalPanel(
           condition = "input.confirmDisorder != 0",
            shiny::sliderInput("age_range_int_vec",
                               shiny::p("Age range"),
                               min = min(age_vec),
                               max = max(age_vec),
                               value = age_vec
            )
          )
        )
      })
      # output$areaControls <- shiny::renderUI({
      #   if(input$pa_type_chr !="Predefined boundary" | is.null(reactive_ls$meso2_chr_choices_vec) |ifelse(is.null(reactive_ls$geom_nav_dbl),T,reactive_ls$geom_nav_dbl<3))
      #     return()
      #   shiny::tagList(
      #     shiny::selectInput("meso2_chr", h3("Feature"),
      #                        choices = reactive_ls$meso2_chr_choices_vec %>%
      #                          springtides::subset_vec_if_var_exists(var_val_chr = input$meso2_first_chr,
      #                                                                fn = startsWith)
      #     )
      #
      #   )
      # })
      # output$areaFilterControls <- shiny::renderUI({
      #   if(input$pa_type_chr !="Predefined boundary"| is.null(reactive_ls$meso2_filter_choices_chr_vec) | ifelse(is.null(reactive_ls$geom_nav_dbl),T,reactive_ls$geom_nav_dbl<3)
      #      )
      #     return()
      #   shiny::tagList(
      #     shiny::selectInput("meso2_first_chr", h3(ifelse(input$meso2_type_chr == "Postal Area",
      #                                                     paste0("First two digits of ",input$meso2_type_chr),
      #                                                     paste0("First two letters of ",input$meso2_type_chr))),
      #                        choices = reactive_ls$meso2_filter_choices_chr_vec)
      #
      #   )
      # })
      output$boundYearControls <- shiny::renderUI({
        if(is.null(reactive_ls$meso2_choices_ls)|ifelse(is.null(reactive_ls$geom_nav_dbl),T,reactive_ls$geom_nav_dbl<2))
          return()
        if(ifelse(is.null(reactive_ls$geom_nav_dbl),T,reactive_ls$geom_nav_dbl<3)){
          shiny::tagList(

            shiny::selectInput("meso2_bound_yr", shiny::p("Boundary year"),
                               choices = reactive_ls$meso2_choices_ls$meso2_bound_yr_chr_vec),
            shiny::actionButton("confirmYear", #"Confirm boundary year selection -->>"
                                "Show features in current map zoom -->>")

          )
        }else{
          shiny::req(input$meso2_bound_yr)
          shiny::tagList(
            #shiny::p("Boundary year"),
            shiny::HTML(paste("<b>","Boundary year","</b>")),
            shiny::p(input$meso2_bound_yr)
          )
        }
      })
      output$customGeometryControls <- shiny::renderUI({
        shiny::req(input$pa_type_chr)
        if(input$pa_type_chr !="HSS") ## Abstract
          return()
        shiny::tagList(
          shiny::selectInput("type_of_custom_geom_chr",
                             shiny::p("Custom geometries based on proximity to"),
                             choices = c("Service locations selected from a list" = "Coordinate lookup",
                                         "Locations you define" = "User defined points")),
          shiny::conditionalPanel(
            condition = "input.type_of_custom_geom_chr == \"Coordinate lookup\" ",
            shiny::checkboxGroupInput("type_of_service_chr",
                                      shiny::p("Type of service"),
                                      choices = pa_r4@lookup_tb@sp_site_coord_lup$service_type %>% unique()),
            actionButton("confirmZoom",
                         "Show services in current map zoom -->>")
          )
        )
      })
      output$dateValidation <- shiny::renderUI({
        shiny::req(input$dateRange)
        if(ifelse(is.null(input$dateRange),F,input$dateRange[1]<input$dateRange[2])){
          shiny::actionButton("confirmWhen", "Confirm dates selection -->>")
        }else{
          shiny::p("A valid date range with the second entry a date later than the first entry is required.")
        }
      })
      output$map <- renderLeaflet({
        makeBaseMap()
      })
      output$pointsList  <- shiny::renderUI({
        shiny::req(input$pa_type_chr)
        shiny::tagList(
          if(input$pa_type_chr == "Predefined boundary"){
            return()
          }else{
            shiny::req(input$gdist_ttime_chr)
            shiny::req(input$gdist_dbl)
            shiny::req(input$ttime_dbl)
            if(ifelse(!is.null(input$map_marker_click),
                      length(markerClickLs$ids)>0,
                      F)){
              shiny::tagList(
                shiny::HTML(paste("<b>","Selected locations","</b>")),
                shiny::p(make_points_text_chr(micro_chr_vec = markerClickLs$ids,
                                              pa_type_chr = input$pa_type_chr,
                                              gdist_ttime_chr = input$gdist_ttime_chr,
                                              gdist_dbl = input$gdist_dbl,
                                              ttime_dbl = input$ttime_dbl,
                                              inc_proximity_lgl = F))
              )
            }else{
              shiny::tagList(
                shiny::HTML(paste("<b>","Selected locations","</b>")),
                p("Click on the markers on the map to select locations.")
              )
            }
          }
        )
      })
      output$polysList  <- shiny::renderUI({
        shiny::tagList(
          if(ifelse(is.null(reactive_ls$geom_nav_dbl),
                    T,
                    reactive_ls$geom_nav_dbl<3)){
            return()
          }else{
            if(ifelse(!is.null(input$map_shape_click),
                      length(polysClickLs$ids)>0,
                      F)){
              shiny::tagList(
                shiny::HTML(paste("<b>","Selected features","</b>")),
                shiny::p(paste0(polysClickLs$ids, collapse = ", ") %>%
                           stringi::stri_replace_last_fixed(',', ' and'))
              )
            }else{
              shiny::tagList(
                shiny::HTML(paste("<b>","Selected features","</b>")),
                p("Click on the shapes on the map to select features.")
              )
            }
          }
        )
      })
      output$outputControls <- shiny::renderUI({
        shiny::req(input$pa_type_chr)
        shiny::req(input$gdist_ttime_chr)
        shiny::req(input$gdist_dbl)
        shiny::req(input$ttime_dbl)
        default_chr <- make_points_text_chr(micro_chr_vec = markerClickLs$ids,
                                            pa_type_chr = input$pa_type_chr,
                                            gdist_ttime_chr = input$gdist_ttime_chr,
                                            gdist_dbl = input$gdist_dbl,
                                            ttime_dbl = input$ttime_dbl)
        if((input$pa_type_chr == "Predefined boundary" & ifelse(is.null(reactive_ls$geom_nav_dbl),F,reactive_ls$geom_nav_dbl >= 3) & ifelse(is.null(reactive_ls$reportDbl),F, reactive_ls$reportDbl==3)) |
           (input$pa_type_chr != "Predefined boundary" & ifelse(identical(markerClickLs$ids,character(0)),F,length(markerClickLs$ids) > 0) & ifelse(is.null(reactive_ls$reportDbl),F, reactive_ls$reportDbl==3))){
          shiny::tagList(
            shiny::h3("Generate a custom report"),
            shiny::wellPanel(style = "background:gold",
              shiny::textInput("user_name_chr", "Your name / your organisation's name", value = "Anonymous User"),
              shiny::conditionalPanel(
                condition = "input.pa_type_chr != \"Predefined boundary\"",
                shiny::textInput("area_name_chr", "Name of the custom geometry that you are profiling", value = default_chr)
              ),
              shiny::p("Note: It will take between 5 and 20 minutes to generate your report."),
              shiny::radioButtons("report_format_chr","Report format:",
                                  c("PDF" = "PDF",
                                    "HTML" = "HTML",
                                    "Word" = "Word"), inline=T),
              shiny::downloadButton("report", "Generate a report")
            )
          )
        }else{
          shiny::tagList(
            shiny::h3("Get started"),
            shiny::wellPanel(
              shiny::p("Select the What, Who, When and Where that you wish to profile. You will then be given the option to generate a custom report based on this profile.")
            )
          )
        }
      })
      output$predefinedControls <- shiny::renderUI({
        shiny::req(input$pa_type_chr)
        if(input$pa_type_chr !="Predefined boundary")
          return()
        if(ifelse(is.null(reactive_ls$geom_nav_dbl),T,reactive_ls$geom_nav_dbl<2)){
          shiny::tagList(
            shiny::selectInput("meso2_type_chr",
                               shiny::p("Spatial unit"),
                               choices = pa_r4@lookup_tb@sp_data_pack_lup %>%
                                 dplyr::filter(main_feature == "Boundary") %>%
                                 dplyr::filter(!area_type %in% c(#"AUS",
                                                                 "HSS",#"SA1","SA2",
                                                                 "XX1")) %>% ## ABSTRACT
                                 dplyr::pull(area_type) %>%
                                 unique() %>%
                                 purrr::map_chr(~ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
                                                                       lookup_variable = "short_name",
                                                                       lookup_reference = .x,
                                                                       target_variable = "long_name",
                                                                       evaluate = F)) %>%
                                 sort()),
            shiny::actionButton("confirmWhere2", paste0("Confirm selection of ",
                                                        ifelse(input$pa_type_chr=="HSS", ## ABSTRACT
                                                               "headspace centres",
                                                               "spatial unit"),
                                                        "  -->>"))
          )
        }else{
          shiny::tagList(
            shiny::HTML(paste("<b>","Spatial unit","</b>")),
            shiny::p(input$meso2_type_chr)

          )
        }

      })
      output$proximityControls <- shiny::renderUI({
        ## Extra reqs for conditional panels??
        shiny::req(input$pa_type_chr)
        if(input$pa_type_chr=="HSS" & length(markerClickLs$ids) == 0) ## Abstract
          return()
        shiny::tagList(
          shiny::conditionalPanel(
            condition = "input.pa_type_chr != \"Predefined boundary\"",
            shiny::selectInput("gdist_ttime_chr", shiny::p("Proximity measure"),
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
      })
      output$report <- shiny::downloadHandler(
        filename = function() {
          paste('Springtides_Report', sep = '.', switch( # Abstract
            input$report_format_chr, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        content = function(file) {
          withProgress(message = 'Rendering, please wait!', {
            path_to_template_chr <- system.file("report.Rmd", package = "springtidesui") ## Abstract
            temp_dir_chr <- tempdir()
            file.copy(path_to_template_chr, paste0(temp_dir_chr,'/report.Rmd'), overwrite = TRUE)
            if(is.null(input$meso2_type_chr)){
              meso2_type_chr <- NA_character_
            }else{
              meso2_type_chr <- ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
                                                      lookup_variable = "long_name",
                                                      lookup_reference = input$meso2_type_chr,
                                                      target_variable = "short_name",
                                                      evaluate = F)
            }
            if(is.null(input$map_shape_click)){#input$meso2_chr
              meso2_chr <- NA_character_
            }else{
              meso2_chr <- polysClickLs$ids #input$meso2_chr
            }
            if(identical(markerClickLs$ids,character(0))){
              micro_chr_vec <- NA_character_
            }else{
              micro_chr_vec <- markerClickLs$ids
            }
            params_ls <- list(age_lower = input$age_range_int_vec[1],
                              age_upper = input$age_range_int_vec[2],
                              authorship_1_chr =  "Report generated by the Orygen Springtides App", # Abstract
                              authorship_2_chr =  paste0("Input parameters selected by ",input$user_name_chr),
                              data_pckg_chr = data_pckg_chr,
                              disorder_chr = input$disorder_chr %>%
                                stringr::str_replace_all(" ","_"),
                              format_chr = input$report_format_chr,
                              gdist_dbl = ifelse(input$pa_type_chr=="Predefined boundary",
                                                 NA_real_,
                                                 ifelse(is.null(input$gdist_dbl), NA_real_,input$gdist_dbl)),
                              gdist_ttime_chr = ifelse(input$pa_type_chr=="Predefined boundary",
                                                       NA_character_
                                                       ,ifelse(is.null(input$gdist_ttime_chr), NA_character_,input$gdist_ttime_chr)),
                              meso2_bound_yr = ifelse(input$pa_type_chr=="Predefined boundary",
                                                      as.integer(input$meso2_bound_yr),
                                                      NA_real_),
                              meso2_chr = meso2_chr,
                              meso2_name_chr = springtides::make_area_name_chr(pa_r4 = pa_r4, # Abstract
                                                                               pa_type_chr = input$pa_type_chr,
                                                                               area_type_chr = meso2_type_chr,
                                                                               feature_chr = meso2_chr,
                                                                               area_name_chr = input$area_name_chr),
                              meso2_type_chr = meso2_type_chr,
                              micro_chr_vec = micro_chr_vec,
                              model_end_date = min(input$dateRange[2] %>% lubridate::as_datetime(tz="Australia/Melbourne"), pa_r4@temporal_max), # Abstract
                              model_start_date = max(input$dateRange[1] %>% lubridate::as_datetime(tz="Australia/Melbourne"), pa_r4@temporal_min), # Abstract
                              n_its_int = input$n_its_int,
                              pa_r4_chr = pa_r4_chr,
                              pa_type_chr = input$pa_type_chr,
                              pdf_output_lgl = switch(input$report_format_chr,
                                                      PDF = T,
                                                      HTML = F,
                                                      Word = T),
                              r_data_dir_chr = r_data_dir_chr,
                              rendered_by_shiny_lgl = T,
                              stat_chr = input$stat_chr,
                              ttime_dbl = ifelse(input$pa_type_chr=="Predefined boundary",
                                                 NA_real_,
                                                 ifelse(is.null(input$ttime_dbl), NA_real_,input$ttime_dbl)),
                              uncertainty_1_int = input$uncertainty_int[1],
                              uncertainty_2_int = input$uncertainty_int[2],
                              user_name_chr = input$user_name_chr)
            params_ls$title_chr <- paste0("Predicted ", ## Abstract
                                          springtides::tf_stat_chr(stat_chr = params_ls$stat_chr %>% tolower(),
                                                                   disorder_chr = params_ls$disorder_chr), # Abstract
                                          " in young people aged ",
                                          params_ls$age_lower,
                                          " to ",
                                          params_ls$age_upper,
                                          " for ",
                                          params_ls$meso2_name_chr,
                                          " between ",
                                          params_ls$model_start_date %>% format("%d %B %Y"),
                                          " and ",
                                          params_ls$model_end_date %>% format("%d %B %Y"))
            params_ls$input_ls_path_chr <- params_ls$sim_data_r4_path_chr <- params_ls$sim_results_ls_path_chr <- NA_character_
            output_params_ls <- springtides::make_output_params_ls(input_params_ls = params_ls) # Abstract
            saveRDS(output_params_ls, file = paste0(temp_dir_chr,'/output_params_ls.rds'))
            params_ls$output_params_ls_path_chr <- normalizePath(paste0(temp_dir_chr,'/output_params_ls.rds'))
            out <- rmarkdown::render(paste0(temp_dir_chr,'/report.Rmd'),
                                     switch(input$report_format_chr,
                                            PDF = rmarkdown::pdf_document(),
                                            HTML = rmarkdown::html_document(toc=T,
                                                                            toc_float = T,
                                                                            number_sections = T,
                                                                            theme = "journal"),
                                            Word = rmarkdown::word_document()),
                                     params = params_ls,
                                     envir = new.env())
            file.rename(out, file)
          })
        }
      )
      output$startAgain <- shiny::renderUI({
        shiny::tagList(
          if(ifelse(is.null(reactive_ls$geom_nav_dbl),T,reactive_ls$geom_nav_dbl<3)){
            return()
          }else{
            shiny::actionButton("returnToWhere", "<<-- Change geometry selections")
          }
        )
      })
      output$statisticControls <- shiny::renderUI({
        #shiny::req(input$confirmStatistic)
        if(ifelse(is.null(input$confirmStatistic),T,input$confirmStatistic==0)|
           ifelse(is.null(reactive_ls$statisticOnDbl),T,reactive_ls$statisticOnDbl==0)){
          shiny::tagList(
            shiny::selectInput("stat_chr", p("Statistic"),
                               choices = springtides::get_input_ls(fn = springtides::make_stat_choices_ls, # Abstract
                                                                   args = NULL,
                                                                   n = Inf) %>% purrr::flatten_chr() %>% sort()),
            shiny::sliderInput("uncertainty_int", "Uncertainty Interval",
                               min = 0.01, max = 1, value = c(0.025,0.975), step = 0.005),
            shiny::sliderInput("n_its_int", "Number of simulation iterations", 1, 100, 10),
            shiny::actionButton("confirmStatistic","Confirm statistic selection -->>")
          )
        }else{
          #shiny::req(input$stat_chr)
          shiny::tagList(
            shiny::HTML(paste("<b>","Statistic","</b>")),
            shiny::p(input$stat_chr),
            shiny::sliderInput("uncertainty_int", "Uncertainty Interval",
                               min = 0.01, max = 1, value = c(0.025,0.975), step = 0.005),
            shiny::sliderInput("n_its_int", "Number of simulation iterations", 1, 100, 10),
            shiny::actionButton("changeStatistic", "<<-- Change statistic selection"),
          )
        }
      })
      output$whatControls <- shiny::renderUI({
        shiny::tagList(
          shiny::wellPanel(
            shiny::uiOutput("statisticControls")
          )
        )
        })
      output$whenControls <- shiny::renderUI({
        if(ifelse(is.null(reactive_ls$whenOnDbl),T,reactive_ls$whenOnDbl==0)){
          shiny::tagList(
            shiny::wellPanel(
              shiny::dateRangeInput('dateRange',
                                    label = 'Start and end dates (yyyy-mm-dd)',
                                    start = Sys.Date(),
                                    end = (Sys.Date() + lubridate::years(5)) %>% min(pa_r4@temporal_max),
                                    min = pa_r4@temporal_min,
                                    max = pa_r4@temporal_max
              ),
              shiny::uiOutput("dateValidation")
            )
          )
        }else{
          shiny::tagList(
            shiny::wellPanel(
              shiny::HTML(paste("<b>","Start and end date","</b>")),
              shiny::p(paste0(input$dateRange[1] %>% format("%d %B %Y"),
                              " to ",
                              input$dateRange[2] %>% format("%d %B %Y")#,"."
                              )),
              shiny::actionButton("changeWhen", "<<-- Change dates selection")
            )
          )
        }

      })
      output$whereControls <- shiny::renderUI({
        shiny::tagList(
          shiny::wellPanel(
            shiny::selectInput("pa_type_chr", shiny::p("Type of geometry"),
                               choices = list("Select from a menu of existing options" = "Predefined boundary",
                                              "Generate your own" = "HSS" # Abstract
                                              # ,
                                              # "Base on proximity to custom coordinates" = "Custom"
                               ),
                               selected = "Predefined boundary"),
            shiny::conditionalPanel(
              condition = "input.pa_type_chr == \"Predefined boundary\"",
              shiny::uiOutput("predefinedControls")
            ),
            shiny::conditionalPanel(
              condition = "input.pa_type_chr == \"HSS\"", # Abstract
              shiny::uiOutput("customGeometryControls")
            ),
            shiny::conditionalPanel(
              condition = "input.pa_type_chr == \"Predefined boundary\" & input.confirmWhere2!=0",
              shiny::uiOutput("boundYearControls")
          ),
          shiny::p(""),
          shiny::uiOutput("polysList"),
          shiny::uiOutput("pointsList"),
          shiny::uiOutput("proximityControls"),
          shiny::uiOutput("startAgain")
        )
        )
      })
      output$whoControls <- shiny::renderUI({
        if(ifelse(is.null(input$confirmDisorder),T,input$confirmDisorder==0)|
           ifelse(is.null(reactive_ls$disorderOnDbl),T,reactive_ls$disorderOnDbl==0)){
          if(ifelse(is.null(reactive_ls$statisticOnDbl),F,reactive_ls$statisticOnDbl==1)){
            shiny::tagList(
              shiny::wellPanel(
              shiny::selectInput("disorder_chr",
                                 shiny::p("Disorder or behaviour"),
                                 choices = reactive_ls$disorder_choices_vec),
              shiny::actionButton("confirmDisorder", "Confirm disorder selection -->>")
            )
            )
          }else{
            shiny::tagList(
              shiny::wellPanel(
              shiny::p("Options to specify the target population will appear once you confirm the statistic you wish to generate.")
            )
            )
          }
        }else{
          shiny::tagList(
            shiny::wellPanel(
              shiny::HTML(paste("<b>","Disorder or behaviour","</b>")),
              shiny::p(input$disorder_chr),
              shiny::uiOutput("ageRangeControls"),
              shiny::actionButton("changeDisorder", "<<-- Change disorder selection")
            )
          )
        }
      })
    if(!is.null(credentials_tb)){
      output$res_auth <- shiny::renderPrint({
        shiny::reactiveValuesToList(result_auth)
      })
    }
  }
}

# make_server_fn <- function(r_data_dir_chr,
#                            credentials_tb = NULL){
#   function(input, output, session){
#     if(!is.null(credentials_tb)){
#       result_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials_tb))
#     }
#     ## 1. LOAD CONTEXT DATA
#     data_pckg_chr <- "springtides"
#     pa_r4_chr <- "aus_pa_r4"
#     data(list = pa_r4_chr,
#          package = data_pckg_chr,
#          envir = environment())
#     eval(parse(text = paste0("pa_r4<-",pa_r4_chr)))
#     ## 2. CREATE REACTIVE LIST
#     reactive_ls <- shiny::reactiveValues()
#     ## 3. SET UP TAB PANEL NAVIGATION
#     ## 3.1 NAVIGATION OBSERVERS
#     shiny::updateTabsetPanel(session,inputId = "tabs", selected = "geom_type")
#     shiny::observeEvent(input$confirmWhere1, {
#       reactive_ls$pa_type_chr <- input$pa_type_chr
#       shinyjs::hide(selector = '#tabs li a[data-value="geom_type"]')
#       shinyjs::show(selector = '#tabs li a[data-value="ft_type"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "ft_type")
#     })
#     shiny::observeEvent(input$returnToWhere1, {
#       shinyjs::hide(selector = '#tabs li a[data-value="ft_type"]')
#       shinyjs::show(selector = '#tabs li a[data-value="geom_type"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "geom_type")
#     })
#     shiny::observeEvent(input$returnToWhere2, {
#       shinyjs::hide(selector = '#tabs li a[data-value="bound_yr"]')
#       shinyjs::show(selector = '#tabs li a[data-value="ft_type""]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "ft_type")
#       reactive_ls$meso2_choices_ls <- NULL
#       reactive_ls$meso2_bound_yr <- NULL
#     })
#     shiny::observeEvent(input$confirmWhere2, {
#       if(is.null(reactive_ls$pa_type_chr)){
#         test_lgl <- F
#       }else{
#         test_lgl <- input$pa_type_chr=="HSS"
#       }
#       if(!test_lgl){
#         meso2_type_tb <- pa_r4@lookup_tb@sp_data_pack_lup %>%
#           dplyr::filter(main_feature == "Boundary") %>%
#           dplyr::filter(area_type == input$meso2_type_chr %>% #reactive_ls$meso2_type_chr %>%
#                           ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
#                                                 lookup_variable = "long_name",
#                                                 lookup_reference = .,
#                                                 target_variable = "short_name",
#                                                 evaluate = F))
#         meso2_bound_yr_chr_vec <- meso2_type_tb %>%
#           dplyr::pull(area_bound_yr) %>%
#           unique() %>%
#           sort()
#         meso2_uid_tb <- pa_r4@lookup_tb@sp_uid_lup %>%
#           dplyr::filter(spatial_unit == input$meso2_type_chr %>% #reactive_ls$meso2_type_chr %>%
#                           ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
#                                                 lookup_variable = "long_name",
#                                                 lookup_reference = .,
#                                                 target_variable = "short_name",
#                                                 evaluate = F))
#         reactive_ls$meso2_choices_ls <- list(meso2_type_tb = meso2_type_tb,
#                                              meso2_bound_yr_chr_vec = meso2_bound_yr_chr_vec,
#                                              meso2_uid_tb = meso2_uid_tb)
#       }
#       shinyjs::hide(selector = '#tabs li a[data-value="ft_type"]')
#       shinyjs::show(selector = paste0('#tabs li a[data-value="',
#                                       ifelse(test_lgl,'ft_value','bound_yr'),
#                                       '"]'))
#       shiny::updateTabsetPanel(session,inputId = "tabs",
#                                selected = ifelse(test_lgl,"ft_value","bound_yr"))
#     })
#     shiny::observeEvent(input$returnToWhere2_2, {
#       if(is.null(reactive_ls$pa_type_chr))
#         test_lgl <- F
#       else
#         test_lgl <- input$pa_type_chr=="HSS"
#       shinyjs::hide(selector = '#tabs li a[data-value="ft_value"]')
#       shinyjs::show(selector = paste0('#tabs li a[data-value="',
#                                       ifelse(test_lgl,'ft_type','bound_yr'),
#                                       '"]'))
#       shiny::updateTabsetPanel(session,inputId = "tabs",
#                                selected = ifelse(test_lgl,"ft_type","bound_yr"))
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "ft_type")
#       reactive_ls$meso2_choices_ls <- NULL
#       reactive_ls$meso2_bound_yr <- NULL
#       reactive_ls$meso2_first_chr <- NULL
#       reactive_ls$meso2_chr_choices_vec <- NULL
#     })
#     shiny::observeEvent(input$confirmWhere3, {
#       shinyjs::hide(selector = '#tabs li a[data-value="ft_value"]')
#       shinyjs::show(selector = '#tabs li a[data-value="pred_yr"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "pred_yr")
#     })
#     shiny::observeEvent(input$returnToWhere, {
#       shinyjs::hide(selector = '#tabs li a[data-value="pred_yr"]')
#       shinyjs::show(selector = '#tabs li a[data-value="geom_type"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "geom_type")
#       reactive_ls$meso2_choices_ls <- NULL
#       reactive_ls$meso2_bound_yr <- NULL
#       reactive_ls$meso2_chr_choices_vec <- NULL
#       reactive_ls$meso2_first_chr <- NULL
#     })
#     shiny::observeEvent(input$confirmYear, {
#       bnd_sf <- readRDS(reactive_ls$meso2_choices_ls$meso2_type_tb %>%
#                           dplyr::filter(area_bound_yr == input$meso2_bound_yr) %>%
#                           dplyr::pull(source_reference) %>%
#                           paste0(r_data_dir_chr,
#                                  "/",
#                                  .,".rds"
#                           )) %>%
#         springtides::filter_if_var_exists(var_chr = "STE_NAME16",
#                                           var_val_xxx = "Other Territories",
#                                           cond_chr = "!=") %>%
#         purrr::reduce(c("2899","6798","6799","7151"),
#                       .init = .,
#                       ~ .x %>% springtides::filter_if_var_exists(var_chr = "POA_NAME",
#                                                                  var_val_xxx = .y,
#                                                                  cond_chr = "!=")) %>%
#         purrr::reduce(c("2899","6798","6799","7151"),
#                       .init = .,
#                       ~ .x %>% springtides::filter_if_var_exists(var_chr = "POA_NAME16",
#                                                                  var_val_xxx = .y,
#                                                                  cond_chr = "!="))
#
#       reactive_ls$meso2_chr_choices_vec <- bnd_sf  %>%
#         dplyr::pull(!!rlang::sym(reactive_ls$meso2_choices_ls$meso2_uid_tb %>%
#                                    dplyr::filter(year == input$meso2_bound_yr) %>%
#                                    dplyr::pull(var_name))) %>%
#         sort()
#       if(ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
#                                lookup_variable = "long_name",
#                                lookup_reference = input$meso2_type_chr,
#                                target_variable = "short_name",
#                                evaluate = F) %in% (pa_r4@lookup_tb@sp_resolution_lup %>%
#                                                    dplyr::filter(area_count >800) %>%
#                                                    dplyr::pull(area_type) %>%
#                                                    unique())){
#         reactive_ls$meso2_filter_choices_chr_vec <- reactive_ls$meso2_chr_choices_vec %>%
#           stringr::str_sub(start=1,end=1) %>%
#           unique() %>%
#           sort()
#       }else{
#         reactive_ls$meso2_filter_choices_chr_vec <- NULL
#       }
#       shinyjs::hide(selector = '#tabs li a[data-value="bound_yr"]')
#       shinyjs::show(selector = '#tabs li a[data-value="ft_value"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",
#                                selected = "ft_value"
#       )
#     })
#     shiny::observeEvent(input$confirmWhen, {
#       shinyjs::hide(selector = '#tabs li a[data-value="pred_yr"]')
#       shinyjs::show(selector = '#tabs li a[data-value="stat_type"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "stat_type")
#     })
#     shiny::observeEvent(input$returnToWhen, {
#       shinyjs::hide(selector = '#tabs li a[data-value="stat_type"]')
#       shinyjs::show(selector = '#tabs li a[data-value="pred_yr"]')
#       updateTabsetPanel(session,inputId = "tabs",selected = "pred_yr")
#     })
#     shiny::observeEvent(input$confirmWhat, {
#       shinyjs::hide(selector = '#tabs li a[data-value="stat_type"]')
#       shinyjs::show(selector = '#tabs li a[data-value="population"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "population")
#     })
#     shiny::observeEvent(input$returnToWhat, {
#       shinyjs::hide(selector = '#tabs li a[data-value="population"]')
#       shinyjs::show(selector = '#tabs li a[data-value="stat_type"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "stat_type")
#     })
#     shiny::observeEvent(input$confirmWho, {
#       shinyjs::hide(selector = '#tabs li a[data-value="population"]')
#       shinyjs::show(selector = '#tabs li a[data-value="review"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "review")
#     })
#     shiny::observeEvent(input$returnToWho, {
#       shinyjs::hide(selector = '#tabs li a[data-value="review"]')
#       shinyjs::show(selector = '#tabs li a[data-value="population"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "population")
#     })
#     shiny::observeEvent(input$confirmAll, {
#       shinyjs::hide(selector = '#tabs li a[data-value="review"]')
#       shinyjs::show(selector = '#tabs li a[data-value="make_rpt"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "make_rpt")
#     })
#     shiny::observeEvent(input$startAgain, {
#       shinyjs::hide(selector = '#tabs li a[data-value="make_rpt"]')
#       shinyjs::show(selector = '#tabs li a[data-value="geom_type"]')
#       shiny::updateTabsetPanel(session,inputId = "tabs",selected = "geom_type")
#     })
#     ## 3.2 TAB IDENTIFIER
#     getTabIndex <- shiny::reactive({
#       match(input$tabs,
#             c("about_springtides","geom_type","ft_type","bound_yr","ft_value","pred_yr","stat_type","population","review","make_rpt"))
#     })
#     ## 4. RENDER TAB PANEL CONTENT
#     ## 4.1 CONTENT OBSERVERS
#     shiny::observe({
#       if(is.null(input$meso2_bound_yr)){
#         reactive_ls$meso2_bound_yr <- NULL
#       }else{
#         reactive_ls$meso2_bound_yr <- input$meso2_bound_yr
#       }
#     })
#     shiny::observe({
#       if(is.null(input$meso2_first_chr)){
#         reactive_ls$meso2_first_chr <- NULL
#       }else{
#         reactive_ls$meso2_first_chr <- input$meso2_first_chr
#       }
#     })
#     shiny::observe({
#       if(is.null(input$stat_chr)){
#         reactive_ls$disorder_choices_vec <- NULL
#       }else{
#         reactive_ls$disorder_choices_vec <- springtides::get_input_vec(fn = springtides::get_disorder_choices_chr_vec,
#                                                                        args = list(lup_r4 = pa_r4@lookup_tb,
#                                                                                    path_to_data_chr = r_data_dir_chr,
#                                                                                    stat_chr = input$stat_chr,
#                                                                                    sex_chr_vec = c("Female","Male")),
#                                                                        n = Inf) %>% stringr::str_replace_all("_"," ") %>%
#           sort()
#       }
#     })
#     shiny::observe({
#       print(input$tabs)
#     })
#     ## 4.2 INPUT CONTROLS
#     output$predefinedControls <- shiny::renderUI({
#       if(input$pa_type_chr !="Predefined boundary")
#         return()
#       shiny::tagList(
#         shiny::selectInput("meso2_type_chr",
#                            shiny::h3("Spatial unit"),
#                            choices = pa_r4@lookup_tb@sp_data_pack_lup %>%
#                              dplyr::filter(main_feature == "Boundary") %>%
#                              dplyr::filter(!area_type %in% c("AUS","HSS","SA1","SA2","XX1")) %>%
#                              dplyr::pull(area_type) %>%
#                              unique() %>%
#                              purrr::map_chr(~ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
#                                                                    lookup_variable = "short_name",
#                                                                    lookup_reference = .x,
#                                                                    target_variable = "long_name",
#                                                                    evaluate = F)) %>%
#                              sort())
#       )
#     })
#     output$boundYearControls <- shiny::renderUI({
#       if(is.null(reactive_ls$meso2_choices_ls))
#         return()
#       shiny::tagList(
#         shiny::selectInput("meso2_bound_yr", h3("Boundary year"),
#                            choices = reactive_ls$meso2_choices_ls$meso2_bound_yr_chr_vec)
#
#       )
#     })
#     output$areaFilterControls <- shiny::renderUI({
#       if(input$pa_type_chr !="Predefined boundary"| is.null(reactive_ls$meso2_filter_choices_chr_vec))
#         return()
#       shiny::tagList(
#         shiny::selectInput("meso2_first_chr", h3(ifelse(input$meso2_type_chr == "Postal Area",
#                                                         paste0("First digit of ",input$meso2_type_chr),
#                                                         paste0("First letter of ",input$meso2_type_chr))),
#                            choices = reactive_ls$meso2_filter_choices_chr_vec)
#
#       )
#     })
#     output$areaControls <- shiny::renderUI({
#       if(input$pa_type_chr !="Predefined boundary" | is.null(reactive_ls$meso2_chr_choices_vec))
#         return()
#       shiny::tagList(
#         shiny::selectInput("meso2_chr", h3("Feature"),
#                            choices = reactive_ls$meso2_chr_choices_vec %>%
#                              springtides::subset_vec_if_var_exists(var_val_chr = input$meso2_first_chr,
#                                                                    fn = startsWith)
#         )
#
#       )
#     })
#     output$headspaceControls <- shiny::renderUI({
#       if(input$pa_type_chr !="HSS")
#         return()
#       shiny::tagList(
#         shiny::checkboxGroupInput("micro_chr_vec", "Headspace Centres",
#                                   choices = pa_r4@lookup_tb@sp_site_coord_lup %>%
#                                     dplyr::pull(service_name) %>% unique() %>% sort(),
#                                   inline = T))
#     })
#     output$conditionalGeomFt1Nav <- shiny::renderUI({
#       if(input$pa_type_chr=="HSS" & length(input$micro_chr_vec) == 0)
#         return()
#       shiny::tagList(
#         shiny::actionButton("confirmWhere2", paste0("Confirm selection of ",
#                                                     ifelse(input$pa_type_chr=="HSS",
#                                                            "headspace centres",
#                                                            "spatial unit"),
#                                                     "  -->>"))
#       )
#     })
#     output$when_controls <- shiny::renderUI({
#       if(input$pa_type_chr=="HSS" & length(input$micro_chr_vec) == 0)
#         return()
#       shiny::tagList(
#         shiny::dateRangeInput('dateRange',
#                               label = 'Start and end dates (yyyy-mm-dd)',
#                               start = Sys.Date(),
#                               end = (Sys.Date() + lubridate::years(5)) %>% min(pa_r4@temporal_max),
#                               min = pa_r4@temporal_min,
#                               max = pa_r4@temporal_max
#         )
#       )
#     })
#     confirmWhenButtonText <- shiny::reactive({
#       shiny::validate(
#         shiny::need(input$dateRange[1] < input$dateRange[2], "End date needs to be later than start date."),
#         shiny::need(!is.null(input$dateRange[1]), "Start date needs to be a valid date but is currently empty."),
#         shiny::need(lubridate::is.Date(input$dateRange[1]), "Start date needs to be a valid date."),
#         shiny::need(!is.null(input$dateRange[2]), "End date needs to be a valid date but is currently empty."),
#         shiny::need(lubridate::is.Date(input$dateRange[2]), "End date needs to be a valid date.")
#       )
#       "Confirm the prediction years -->>"
#     })
#     output$confirm_when_controls <- shiny::renderUI({
#       shiny::tagList(
#         shiny::actionButton("confirmWhen", confirmWhenButtonText())
#       )
#     })
#     output$disorderControls <- shiny::renderUI({
#       shiny::tagList(shiny::selectInput("disorder_chr",
#                                         shiny::h3("Disorder or behaviour"),
#                                         choices = reactive_ls$disorder_choices_vec))
#     })
#     output$ageRangeControls <- shiny::renderUI({
#       if(is.null(input$disorder_chr))
#         return()
#       age_vec <- springtides::get_age_range_choices_int_vec(lup_r4 = pa_r4@lookup_tb,
#                                                             path_to_data_chr = r_data_dir_chr,
#                                                             stat_chr = input$stat_chr,
#                                                             disorder_chr = input$disorder_chr %>%
#                                                               stringr::str_replace_all(" ","_"))
#       shiny::tagList(
#         shiny::sliderInput("age_range_int_vec",
#                            shiny::h3("Age range"),
#                            min = min(age_vec), max = max(age_vec), value = age_vec))
#     })
#     output$statisticControls <- shiny::renderUI({
#       shiny::tagList(
#         shiny::selectInput("stat_chr", h3("Statistic:"),
#                            choices = springtides::get_input_ls(fn = springtides::make_stat_choices_ls,
#                                                                args = NULL,
#                                                                n = Inf) %>% purrr::flatten_chr() %>% sort())
#       )
#     })
#     output$areaNameControls <- shiny::renderUI({
#       gdist_dbl <- ifelse(input$pa_type_chr=="Predefined boundary",
#                           NA_real_,
#                           ifelse(is.null(input$gdist_dbl),
#                                  NA_real_,
#                                  input$gdist_dbl))
#       ttime_dbl <- ifelse(input$pa_type_chr=="Predefined boundary",
#                           NA_real_,
#                           ifelse(is.null(input$ttime_dbl), NA_real_,input$ttime_dbl))
#       #metric_dbl <- c(gdist_dbl,ttime_dbl) %>% purrr::keep(~!is.na(.x))
#
#       gdist_ttime_chr <- ifelse(input$pa_type_chr=="Predefined boundary",
#                                 NA_character_,
#                                 ifelse(is.null(input$gdist_ttime_chr), NA_character_,input$gdist_ttime_chr))
#       if(is.null(input$micro_chr_vec)){
#         points_chr <- "" # Replace with numeric count of points for custom coordinates and >5 services
#       }else{
#         points_chr <- input$micro_chr_vec %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed( ',', ' and')
#       }
#       default_chr <- paste0("Area within ",
#                             ifelse(is.na(gdist_ttime_chr),
#                                    "",
#                                    ifelse(gdist_ttime_chr=="Geometric distance",
#                                           paste0(gdist_dbl, " Km "),
#                                           paste0(ttime_dbl, " Minutes Drive "))),
#                             "of ",
#                             points_chr,
#                             ifelse(T,
#                                    paste0(" Headspace Centre",ifelse(length(input$micro_chr_vec)>1,"s","")),
#                                    " Custom Coordinates")) # Replace with condition logic for custom coordinates
#       shiny::conditionalPanel(
#         condition = "input.pa_type_chr != \"Predefined boundary\"",
#         shiny::textInput("area_name_chr", "Name of the custom geometry that you are profiling", value = default_chr)
#       )
#     })
#     ## 4.3 CONTENT TEXT
#     output$about_chr <- shiny::renderUI({
#       if(getTabIndex()==1){
#         h1_chr <- shiny::h1("Welcome")
#         p1_chr <- shiny::p("Thank you for your interest in Springtides, The Youth Mental Health Epi App.")
#         p2_chr <- shiny::p("Copyright Orygen 2018-2020")
#         shiny::HTML(paste0(h1_chr,p1_chr,p2_chr))
#       }else{
#         return()
#       }
#     })
#     output$welcome_chr <- shiny::renderUI({
#       if(getTabIndex()==2){
#         h1_chr <- shiny::h1("Where?")
#         p2_chr <- shiny::p("To get started, you first need to specify the area for which you would like to generate an epidemiological profile. You can either select an area for which there are existing geometries or alternatively generate a geometry that defines your own custom area.")
#         shiny::HTML(paste0(h1_chr,p2_chr))
#       }else{
#         return()
#       }
#     })
#     output$selected_pa_type_chr <- shiny::renderUI({
#       if(getTabIndex()<3)
#         return()
#       h1_chr <- shiny::h1("Where?")
#       p1_chr <- shiny::p(paste0("You have opted ", ifelse(input$pa_type_chr=="Predefined boundary","to use an existing boundary.","to define your own custom boundaries.")))
#       shiny::HTML(paste0(h1_chr,p1_chr))
#     })
#     output$ft_select_chr <- shiny::renderUI({
#       if(getTabIndex()!=3 | !input$pa_type_chr %in% c("Predefined boundary","HSS"))
#         return()
#       if(input$pa_type_chr=="Predefined boundary")
#         text_chr <- "Now select the spatial unit that you would like to use."
#       if(input$pa_type_chr=="HSS")
#         text_chr <- "You can create custom boundaries based on proximity to the headspace centres you select. It is recommended that you select centres that are reasonably close together (e.g. within the same or neighbouring PHNs) as larger geographic areas produce lower resolution results plots."
#       shiny::p(text_chr)
#     })
#     output$selected_ft_type_chr <- shiny::renderUI({
#       if(getTabIndex()<4 | !input$pa_type_chr %in% c("Predefined boundary","HSS"))
#         return()
#       if(input$pa_type_chr=="Predefined boundary")
#         text_chr <- paste0("The spatial unit that you have selected is ",
#                            input$meso2_type_chr,
#                            ".")
#       if(input$pa_type_chr=="HSS")
#         text_chr <-paste0("Your custom boundary will be based on proximity to the following headspace centre",
#                           ifelse(length(input$micro_chr_vec)>1,"s: ",": "), input$micro_chr_vec %>% paste(collapse = ', '),".")
#       shiny::p(text_chr)
#
#     })
#     output$bound_yr_select_chr <- shiny::renderUI({
#       if(getTabIndex()!=4 | !input$pa_type_chr %in% c("Predefined boundary"))
#         return()
#       if(input$pa_type_chr=="Predefined boundary")
#         text_chr <- "Now select the spatial unit boundary year."
#       shiny::p(text_chr)
#     })
#     output$selected_bound_yr_chr <- shiny::renderUI({
#       if(getTabIndex()<5 | !input$pa_type_chr %in% c("Predefined boundary"))
#         return()
#       if(input$pa_type_chr=="Predefined boundary")
#         text_chr <- paste0("The version of those boundaries are those published in ",
#                            input$meso2_bound_yr,
#                            ".")
#       shiny::p(text_chr)
#     })
#     output$ft_value_select_chr <- shiny::renderUI({
#       if(getTabIndex()!=5 | !input$pa_type_chr %in% c("Predefined boundary","HSS"))
#         return()
#       if(input$pa_type_chr=="Predefined boundary")
#         text_chr <- "Now select the feature that you wish to profile."
#       if(input$pa_type_chr=="HSS")
#         text_chr <- "Now select the type of proximity measure and the upper bound to that proximity. PLEASE NOTE: The drive time proximity relies upon an external service provider which is currently only intermittently available. Simulations run with this option selected frequently fail to execute successfully."
#       shiny::p(text_chr)
#     })
#     output$selected_ft_values_chr <- shiny::renderUI({
#       if(getTabIndex()<6 | !input$pa_type_chr %in% c("Predefined boundary","HSS"))
#         return()
#       if(input$pa_type_chr=="Predefined boundary")
#         text_chr <- paste0("The selected feature is ",
#                            input$meso2_chr,
#                            ".")
#       if(input$pa_type_chr=="HSS")
#         text_chr <- paste0("The proximity is based on ",
#                            input$gdist_ttime_chr %>% tolower(),
#                            " with the upper bound on distance being ",
#                            ifelse(input$gdist_ttime_chr=="Geometric distance",
#                                   paste0(input$gdist_dbl, " kilometers."),
#                                   paste0(input$ttime_dbl, " minutes drive.")))
#       shiny::p(text_chr)
#     })
#     output$when_heading_chr <- shiny::renderUI({
#       if(getTabIndex()<6)
#         return()
#       shiny::h1("When?")
#     })
#     output$when_instr_chr <- shiny::renderUI({
#       if(getTabIndex()!=6)
#         return()
#       shiny::p("Now select the dates for which you would like to generate a prediction. The dates must be between July 2016 and June 2031.")
#     })
#     output$when_selected_chr <- shiny::renderUI({
#       if(getTabIndex()<7)
#         return()
#       shiny::p(paste0("The dates for which results will be predicted are ",
#                       input$dateRange[1] %>% format("%d %B %Y"),
#                       " to ",
#                       input$dateRange[2] %>% format("%d %B %Y"),
#                       "."))
#     })
#     output$what_heading_chr <- shiny::renderUI({
#       if(getTabIndex()<7)
#         return()
#       shiny::h1("What?")
#     })
#     output$what_instr_chr <- shiny::renderUI({
#       if(getTabIndex()!=7)
#         return()
#       shiny::p("Now select the main statistic and associated level of uncertainty that you would like to generate.")
#     })
#     output$what_selected_chr <- shiny::renderUI({
#       if(getTabIndex()<8)
#         return()
#       shiny::p(paste0("The main statistic to be generated is ",
#                       input$stat_chr,
#                       ", with an uncertainty interval of ",
#                       input$uncertainty_int[1] * 100,
#                       "% to ",
#                       input$uncertainty_int[2] * 100,
#                       "% that will be based on ",
#                       input$n_its_int,
#                       " simulation iterations."))
#     })
#     output$who_heading_chr <- shiny::renderUI({
#       if(getTabIndex()<8)
#         return()
#       shiny::h1("Who?")
#     })
#     output$who_instr_chr <- shiny::renderUI({
#       if(getTabIndex()!=8)
#         return()
#       shiny::p("Now select the disorder or behaviour and age range of the population that you would like to profile.")
#     })
#     output$who_selected_chr <- shiny::renderUI({
#       if(getTabIndex()<9)
#         return()
#       shiny::p(paste0("The population to be profiled are young people with ",
#                       input$disorder_chr," who are aged between ",
#                       input$age_range_int_vec[1],
#                       " and ",
#                       input$age_range_int_vec[2],
#                       "."))
#
#     })
#     ## 5. RENDER OUTPUT
#     output$report <- shiny::downloadHandler(
#       filename = function() {
#         paste('Springtides_Report', sep = '.', switch(
#           input$report_format_chr, PDF = 'pdf', HTML = 'html', Word = 'docx'
#         ))
#       },
#       content = function(file) {
#         withProgress(message = 'Rendering, please wait!', {
#           path_to_template_chr <- system.file("report.Rmd", package = "springtidesui")#"report.Rmd"#
#           temp_dir_chr <- tempdir()
#           file.copy(path_to_template_chr, paste0(temp_dir_chr,'/report.Rmd'), overwrite = TRUE)
#           if(is.null(input$meso2_type_chr)){
#             meso2_type_chr <- NA_character_
#           }else{
#             meso2_type_chr <- ready4utils::data_get(pa_r4@lookup_tb@sp_abbreviations_lup,
#                                                     lookup_variable = "long_name",
#                                                     lookup_reference = input$meso2_type_chr,
#                                                     target_variable = "short_name",
#                                                     evaluate = F)
#           }
#           if(is.null(input$meso2_chr)){
#             meso2_chr <- NA_character_
#           }else{
#             meso2_chr <- input$meso2_chr
#           }
#           if(is.null(input$micro_chr_vec)){
#             micro_chr_vec <- NA_character_
#           }else{
#             micro_chr_vec <- input$micro_chr_vec
#           }
#           params_ls <- list(age_lower = input$age_range_int_vec[1],
#                             age_upper = input$age_range_int_vec[2],
#                             authorship_1_chr =  "Report generated by the Orygen Springtides App",
#                             authorship_2_chr =  paste0("Input parameters selected by ",input$user_name_chr),
#                             data_pckg_chr = data_pckg_chr,
#                             disorder_chr = input$disorder_chr %>%
#                               stringr::str_replace_all(" ","_"),
#                             format_chr = input$report_format_chr,
#                             gdist_dbl = ifelse(input$pa_type_chr=="Predefined boundary",
#                                                NA_real_,
#                                                ifelse(is.null(input$gdist_dbl), NA_real_,input$gdist_dbl)),
#                             gdist_ttime_chr = ifelse(input$pa_type_chr=="Predefined boundary",
#                                                      NA_character_
#                                                      ,ifelse(is.null(input$gdist_ttime_chr), NA_character_,input$gdist_ttime_chr)),
#                             meso2_bound_yr = ifelse(input$pa_type_chr=="Predefined boundary",
#                                                     as.integer(input$meso2_bound_yr),
#                                                     NA_real_),
#                             meso2_chr = meso2_chr,
#                             meso2_name_chr = springtides::make_area_name_chr(pa_r4 = pa_r4,
#                                                                              pa_type_chr = input$pa_type_chr,
#                                                                              area_type_chr = meso2_type_chr,
#                                                                              feature_chr = meso2_chr,
#                                                                              area_name_chr = input$area_name_chr),
#                             meso2_type_chr = meso2_type_chr,
#                             micro_chr_vec = micro_chr_vec,
#                             model_end_date = min(input$dateRange[2] %>% lubridate::as_datetime(tz="Australia/Melbourne"), pa_r4@temporal_max),
#                             model_start_date = max(input$dateRange[1] %>% lubridate::as_datetime(tz="Australia/Melbourne"), pa_r4@temporal_min),
#                             n_its_int = input$n_its_int,
#                             pa_r4_chr = pa_r4_chr,
#                             pa_type_chr = input$pa_type_chr,
#                             pdf_output_lgl = switch(input$report_format_chr,
#                                                     PDF = T,
#                                                     HTML = F,
#                                                     Word = T),
#                             r_data_dir_chr = r_data_dir_chr,
#                             rendered_by_shiny_lgl = T,
#                             stat_chr = input$stat_chr,
#                             ttime_dbl = ifelse(input$pa_type_chr=="Predefined boundary",
#                                                NA_real_,
#                                                ifelse(is.null(input$ttime_dbl), NA_real_,input$ttime_dbl)),
#                             uncertainty_1_int = input$uncertainty_int[1],
#                             uncertainty_2_int = input$uncertainty_int[2],
#                             user_name_chr = input$user_name_chr)
#           params_ls$title_chr <- paste0("Predicted ",
#                                         springtides::tf_stat_chr(stat_chr = params_ls$stat_chr %>% tolower(),
#                                                                  disorder_chr = params_ls$disorder_chr),
#                                         " in young people aged ",
#                                         params_ls$age_lower,
#                                         " to ",
#                                         params_ls$age_upper,
#                                         " for ",
#                                         params_ls$meso2_name_chr,
#                                         " between ",
#                                         params_ls$model_start_date %>% format("%d %B %Y"),
#                                         " and ",
#                                         params_ls$model_end_date %>% format("%d %B %Y"))
#           out <- rmarkdown::render(paste0(temp_dir_chr,'/report.Rmd'),
#                                    switch(input$report_format_chr,
#                                           PDF = rmarkdown::pdf_document(),
#                                           HTML = rmarkdown::html_document(toc=T,
#                                                                           toc_float = T,
#                                                                           number_sections = T,
#                                                                           theme = "journal"),
#                                           Word = rmarkdown::word_document()),
#                                    params = params_ls,
#                                    envir = new.env())
#           file.rename(out, file)
#         })
#       }
#     )
#     if(!is.null(credentials_tb)){
#       output$res_auth <- shiny::renderPrint({
#         shiny::reactiveValuesToList(result_auth)
#       })
#     }
#   }
# }
