
# ============================================= #
# setup                                         #
# ============================================= #

# reading/writing data
# library(readxl)
library(sf)
library(xlsx)
# library(r2excel)  # function customised: see 'load function' section
# devtools::install_github("kassambara/r2excel")

# data manipulation
library(data.table)
library(stringr)    # tidyverse
library(dplyr)    # tidyverse
library(readr)    # tidyverse
library(tidyr)    # tidyverse
library(fastmatch)
# library(tidyverse)

# shiny + dashboard
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyBS)
#library(shinycustomloader)
library(colourpicker)
#library(jsTreeR)

# Data table render
library(DT)

# Map
library(leaflet)
library(leaflet.extras)

# graphical output other than shiny
library(RColorBrewer) # for color of leaflet polygonlayer

# histograms
library(googleVis)

# Network
# library(igraph) # imported by tidygraph
library(visNetwork)
library(tidygraph)

# radial/diagonal network
library(data.tree) # also for 
library(networkD3)

# interactive sunburst plot
library("plotly")


# ============================================= #
# load function                                 #
# ============================================= #

# custom modifications for performance reasons (r2excel::xlsx.addTable())
source("data/function_xlsx.addTable_mod.r")
source("data/function_collection.R", local=T)

# ============================================= #
# "variables":                                  #
# ============================================= #

# ============================================= #
# load species data                             #
# ============================================= #

load("data/Carabidae_data.RDA", verbose=T)
load("data/Carabidae_variables.RDA", verbose=T)

# ============================================= #
# load bibliography                             #
# ============================================= #

# ============================================= #
# prepare shiny app menu                        #
# ============================================= #

# for inputSlider: Year
YearMinMax <- c(min(Carabidae_data$Year[Carabidae_data$Year!=0], na.rm = T), max(Carabidae_data$Year, na.rm = T))

# species lists for menu
nameSet <- list(CarabidaeSpecies = Carabidae_data[,logical(1), keyby=Carabidae_name]$Carabidae_name,
                CarabidaeGenus = sort(unique(sub(" .*$", "", Carabidae_data$Carabidae_name))),
                FungusSpecies = Carabidae_data[,logical(1), keyby=Fungus_name]$Fungus_name,
                FungusGenus = sort(unique(sub(" .*$", "", Carabidae_data$Fungus_name))),
                Interaction = Carabidae_data[,logical(1), keyby=Inter_type]$Inter_type,
                CountriesDB = c("",Carabidae_data[,logical(1), keyby=Country][Country %in% shp$NAME,]$Country),
                CountriesSHP = sort(shp$NAME[is.na(shp$SubRegSum)]) # <NA> means country and not region
)


# remove "x" as species ...
nameSet$CarabidaeSpecies <- nameSet$CarabidaeSpecies[nameSet$CarabidaeSpecies!="x"]
nameSet$CarabidaeGenus <- nameSet$CarabidaeGenus[nameSet$CarabidaeGenus!="x"]
nameSet$FungusSpecies <- nameSet$FungusSpecies[nameSet$FungusSpecies!="x"]
nameSet$FungusGenus <- nameSet$FungusGenus[nameSet$FungusGenus!="x"]
# ... and missing countries
nameSet$CountriesDB <- nameSet$CountriesDB[nameSet$CountriesDB!=""]

# group content in alphabetical groups
nameSet$CarabidaeSpecies = split(nameSet$CarabidaeSpecies, sub(" .*$", "", nameSet$CarabidaeSpecies))
### nameSet$CarabidaeGenus = split(nameSet$CarabidaeGenus, substring(nameSet$CarabidaeGenus, 1, 1))
nameSet$FungusSpecies = split(nameSet$FungusSpecies, sub(" .*$", "", nameSet$FungusSpecies))
### nameSet$FungusGenus = split(nameSet$FungusGenus, substring(nameSet$FungusGenus, 1, 1))


# workaround: single entry list elements are automatically simplified in the the selectInput() menu.
# Putting a new list around the element does the trick. Important: the innermost list must be w/o name.
index_single <- which(unlist(lapply(nameSet[["CarabidaeSpecies"]],length))==1)
nameSet[["CarabidaeSpecies"]][index_single] <- lapply(index_single, function(i) unname(nameSet[["CarabidaeSpecies"]][i]))
index_single <- c()
# index_single <- which(unlist(lapply(nameSet[["CarabidaeGenus"]],length))==1)
# nameSet[["CarabidaeGenus"]][index_single] <- lapply(index_single, function(i) unname(nameSet[["CarabidaeGenus"]][i]))
# index_single <- c()
index_single <- which(unlist(lapply(nameSet[["FungusSpecies"]],length))==1)
nameSet[["FungusSpecies"]][index_single] <- lapply(index_single, function(i) unname(nameSet[["FungusSpecies"]][i]))
index_single <- c()
# index_single <- which(unlist(lapply(nameSet[["FungusGenus"]],length))==1)
# nameSet[["FungusGenus"]][index_single] <- lapply(index_single, function(i) unname(nameSet[["FungusGenus"]][i]))


rm(index_single)



# ============================================= #
# Layout                                        #
# ============================================= #

# Carabidae_World <- getPlainMap(mapView=initView, activeLayer = "world region resolution")

# ============================================= #
# UI                                            #
# ============================================= #

source("UI-Carabidae.R", local=T)

# ============================================= #
# server                                        #
# ============================================= #

server <- function(input, output, session) {

    # helping object to be aware of current map layer choise
    activeLayer <- "world region resolution"    ###"country resolution"
    
    ReactVal <- reactiveValues(CountryClickSelection="*")
    
    # used to draw the edge legend
    EdgeTab <- reactiveValues(data=NULL)
    
    
    reactivVal <- reactiveValues(xxx=NULL)
    # =================================== #
    #   filtered table (reactive object)  #
    # =================================== # 
    
    dataFilter <- reactive({
        shiny::validate(need(input$Carabidae_Selection, message=FALSE), 
                        need(input$Fungus_Selection, message=FALSE),
                        need(input$InteractionType, message=FALSE), need(input$years, message=FALSE),
                        need(input$Country_Selection, message=FALSE))
      
        Carabidae_selection <- Carabidae_data[Carabidae_name %chin% input$Carabidae_Selection & 
                                      Fungus_name %chin% input$Fungus_Selection & 
                                      Inter_type %chin% input$InteractionType & 
                                      Country %chin% input$Country_Selection &
                                      Year %between% input$years]
        
        reactivVal$xxx=list(Aph=Carabidae_data$Carabidae_name %chin% input$Carabidae_Selection, 
                            Fung=Carabidae_data$Fungus_name %chin% input$Fungus_Selection,
                            inter=Carabidae_data$Inter_type %chin% input$InteractionType,
                            Country=Carabidae_data$Country %chin% input$Country_Selection,
                            year=Carabidae_data$Year %between% input$years)
        
        if (nrow(Carabidae_selection)==0) {
            showNotification(shiny::HTML("<b>Side Bar Selections:</b><br/>The filter returned an empty table."), type="warning", duration=10)
        }
        
        return(Carabidae_selection)
    })
    
    getView <- reactive({
        shiny::validate(need(input$Country_Selection, message=FALSE))

            if (length(input$Country_Selection) != 0) {
                currentView <- list(lng=isolate(input$MapPlot_lng), # shp$LON[shp$NAME==input$Country_Selection],
                                    lat=isolate(input$MapPlot_lat), # shp$LAT[shp$NAME==input$Country_Selection],
                                    zoom=isolate(input$MapPlot_zoom))
            }else{
                currentView <- initView
            }

        return(currentView)
    })
    
    # ============== #
    #   menu         #
    # ============== #     
 
    ### SIDEBAR
    
    # Change Carabidae choices to species or genus
    # ----------------------------------------
    observe( {
        updatePickerInput(session,
            inputId="Carabidae_Selection",
            choices = nameSet[[input$Carabidae_RdBtn]],
            selected = unname(unlist(nameSet[[input$Carabidae_RdBtn]]))
        )
    })
    
    # Change Fungus choices to species or genus
    # -----------------------------------------
    observeEvent(input$Fungus_RdBtn, {
        updatePickerInput(session, 
            inputId="Fungus_Selection",
            choices=nameSet[[input$Fungus_RdBtn]],
            selected = unname(unlist(nameSet[[input$Fungus_RdBtn]]))
            )
    })
    
    # # # Change Interaction type to available choices
    # !!!!! (when testing, it slowed down the update process dramatically, most likely because of reciprocal updating)
    # # # -----------------------------------------
    # observe({
    #   valid_choices <- nameSet$Interaction %in% dataFilter()$Inter_type
    # 
    #   print("Interaction update")
    #
    #   updatePickerInput(
    #     session=session,
    #     inputId="InteractionType",
    #     choices = nameSet$Interaction,
    #     selected = nameSet$Interaction[valid_choices],
    #     choicesOpt = list(
    #       disabled = !valid_choices,
    #       style = ifelse(
    #         valid_choices,
    #         yes = "",
    #         no = "color: rgba(119, 119, 119, 0.5);")))
    # })
    
    # clear filter button
    # -----------------------------------------
    observeEvent(input$ClearFilter_bt, {
        updateRadioButtons(session, inputId="Carabidae_RdBtn", selected="CarabidaeSpecies")
        updateRadioButtons(session, inputId="Fungus_RdBtn", selected="FungusSpecies")
#        updateRadioButtons(session, inputId="Country_RdBtn", selected="in")
        updatePickerInput(session, inputId="Carabidae_Selection", selected = nameSet$CarabidaeSpecies)
        updatePickerInput(session, inputId="Fungus_Selection", selected = unlist(nameSet$FungusSpecies))
        updatePickerInput(session, inputId="InteractionType", selected = nameSet$Interaction)
        updatePickerInput(session, inputId="Country_Selection", selected = nameSet[[isolate(input$CountryList_RdBtn)]])
        updateSliderInput(session, inputId="years",  value= YearMinMax)
    })
    
    # toggle-network-filter-setting button
    # -----------------------------------------
    observeEvent(input$Toggle_DataFilter_btn, {
      shinyjs::toggleElement("years")
      
      shinyjs::toggleElement("Carabidae_RdBtn")
      shinyjs::toggleElement("Carabidae_Selection")
      shinyjs::toggleElement("Fungus_RdBtn")
      shinyjs::toggleElement("Fungus_Selection")
      
      shinyjs::toggleElement("InteractionType")
      shinyjs::toggleElement("Country_Selection")
      
      shinyjs::toggleElement("clearDiv")
      
      shinyjs::toggleElement("CarabidaeTitle")
      shinyjs::toggleElement("SepCen1")
      shinyjs::toggleElement("SepCen2")
      shinyjs::toggleElement("SepCen3")
      shinyjs::toggleElement("SepCen4")
      shinyjs::toggleElement("SepCen5")
      shinyjs::toggleElement("SepCen6")
    })
    
    # toggle-network-layout-setting button
    # -----------------------------------------
    observeEvent(input$Toggle_NetworkLayout__menuBtn, {
      shinyjs::toggleElement("NetworkLayout_title")
      shinyjs::toggleElement("NetworkLayout_div")
      shinyjs::toggleElement("SepCen7")
    })
    
    # toggle-network-colour-setting button
    # -----------------------------------------
    observeEvent(input$Toggle_NWColour_menuBtn, {
        shinyjs::toggleElement("Col_Carabidae_title")
        shinyjs::toggleElement("BackgroundCol_Carabidae_div")
        shinyjs::toggleElement("BorderCol_Carabidae_div")
        shinyjs::toggleElement("AColHigh_div")
        
        shinyjs::toggleElement("Col_Fungus_title")
        shinyjs::toggleElement("BackgroundCol_Fungus_div")
        shinyjs::toggleElement("BorderCol_Fungus_div")
        shinyjs::toggleElement("FColHigh_div")
        
        shinyjs::toggleElement("EdgeCol_title")
        shinyjs::toggleElement("EdgeCol_div")
        shinyjs::toggleElement("HighlightCol_Edges_div")
        
        shinyjs::toggleElement("SepCen8")
        shinyjs::toggleElement("SepCen9")
        shinyjs::toggleElement("SepCen10")
    })

    
    # toggle-network-symbol-setting button
    # -----------------------------------------
    observeEvent(input$Toggle_NWShape_menuBtn, {
        shinyjs::toggleElement("SymbolTitle")
        shinyjs::toggleElement("ShapeIpt_Carabidae_div")
        shinyjs::toggleElement("ShapeIpt_Fungus_div")
    })
 
    ### GLOBAL TAB BUTTONS
    # (in reality choices of a single radio button - tab_rdBtn)
    
    # open main-tab button 1
    # -----------------------------------------
    observeEvent(input$main_tab_btn, {
        updateTabItems(session, inputId = "tabs", selected = "mainTab")
    })
    
    # open table-tab button 2
    # -----------------------------------------
    observeEvent(input$table_tab_btn, {
        updateTabItems(session, inputId = "tabs", selected = "tableTab")
    })
    
    # open about-tab button 3
    # -----------------------------------------
    observeEvent(input$about_tab_btn_acc, {
        updateTabItems(session, inputId = "tabs", selected = "accTab")
    })
    
    # open about-tab button 5
    # -----------------------------------------
    observeEvent(input$radNet_tab_btn, {
      updateTabItems(session, inputId = "tabs", selected = "radNet")
    })
  
    
    ### NETWORK (menu)
    
    observeEvent(input$highSpec_al, {
      shinyjs::toggleElement("findCarabidae_node")
      shinyjs::toggleElement("findFungus_node")
    })
    observeEvent(input$legend_al, {
      shinyjs::toggleElement("NetworkLegend2")
      shinyjs::toggleElement("NetworkLegendDpdn2")
    })
    
    # find Carabidae in network
    # -----------------------------------------
    observe({
      updateSelectInput(
        session = session,
        inputId = "findCarabidae_node",
        choices = c("<Carabidae>"="-", sort(unique(dataFilter()$Carabidae_name))),
        selected = "-")
    })
    
    # find Fungus in network
    # -----------------------------------------
    observe({
      updateSelectInput(
        session = session,
        inputId = "findFungus_node",
        choices = c("<Fungus>"="-", sort(unique(dataFilter()$Fungus_name))),
        selected = "-")
    })
    
    output$NetworkLegend <- renderUI(
      HTML(
        sub("#404040;",input$BackgroundCol_Carabidae, fixed = T,
            sub("XXXXX", shapes[input$ShapeIpt_Carabidae], fixed = T,
                "<table style='color: #DADADA; display: inline-table; width: 50%'><tr><td><span style='height: 2em; width: 2em; display: inline-block; color: #FAFAFA;'><svg version='1.1' focusable='false' x='0px' y='0px' viewBox='0 0 512 512'><g>XXXXX</g></svg></span></td><td>Carabidae&nbsp;</td>")),
        
        sub("#404040;",input$BackgroundCol_Fungus, fixed = T,
            sub("XXXXX", shapes[input$ShapeIpt_Fungus], fixed = T,
                "<td><span style='height: 2em; width: 2em; display: inline-block;'><svg version='1.1' focusable='false' x='0px' y='0px' viewBox='0 0 512 512'><g>XXXXX</g></svg></span></td><td>Fungus&nbsp;</td></tr></table>"))
      )
    )
    output$NetworkLegend2 <- renderUI(
      HTML(
        sub("#404040;",input$BackgroundCol_Carabidae, fixed = T,
            sub("XXXXX", shapes[input$ShapeIpt_Carabidae], fixed = T,
                "<table style='color: #DADADA; display: inline-table; width: 50%'><tr><td><span style='height: 2em; width: 2em; display: inline-block; color: #FAFAFA;'><svg version='1.1' focusable='false' x='0px' y='0px' viewBox='0 0 512 512'><g>XXXXX</g></svg></span></td><td>Carabidae&nbsp;</td></tr>")),
        
        sub("#404040;",input$BackgroundCol_Fungus, fixed = T,
            sub("XXXXX", shapes[input$ShapeIpt_Fungus], fixed = T,
                "<tr><td><span style='height: 2em; width: 2em; display: inline-block;'><svg version='1.1' focusable='false' x='0px' y='0px' viewBox='0 0 512 512'><g>XXXXX</g></svg></span></td><td>Fungus&nbsp;</td></tr></table>"))
      )
    )
    
    output$NetworkLegendDpdn <- renderUI({
      # shiny::validate(need(EdgeTab$data, message=FALSE))
      if (is.null(EdgeTab$data)) {
        return(HTML("<br>&nbsp;<br><br><br><br><br><br>&nbsp;<br>&nbsp;"))
      }else{
        tableText <- data.table(rows = c("<tr><td style='width: 100px'> <hr style='border-width: 1px !important; border-color: #FAFAFA !important; text-align: center; margin: 0;'> </td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='dashed'> </td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='dots'> </td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='dash-dot'> </div></td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='longdash'> </td></td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='twodash'> </td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='dash-dot-dot'> </div> </td><td> &nbsp;: ",
                                     "<tr><td style='width: 100px'> <div class='dash-dot-dot-dot'> </td><td> &nbsp;: "),
                            names=rep(NA,8),
                            closings="</td></tr>",
                            id=1:8)
        InAcCodeTab <- EdgeTab$data %>% select(Inter_type, value) %>% distinct() %>% as.data.table()
        InAcCodeTab$Inter_type <- gsub("<|>","",InAcCodeTab$Inter_type)
        
        setkey(InAcCodeTab,value)
        tableText$names <- InAcCodeTab$Inter_type[1:8]
#       tableText[InAcCodeTab, on=c(id="value"), names:=i.Inter_type]
        tableText <- tableText[!is.na(names)]
        if (nrow(tableText)>7) {tableText$names[8] <- "various"}
        
        returnTable <- paste0("<table>",
                              paste0(tableText[,rows],tableText[,names],tableText[,closings],collapse=""),
                              "</table>")
        
        return(HTML(returnTable))
      }
    })
    output$NetworkLegendDpdn2 <- renderUI({
      # shiny::validate(need(EdgeTab$data, message=FALSE))
      if (is.null(EdgeTab$data)) {
        return(HTML("<br>&nbsp;<br><br><br><br><br><br>&nbsp;<br>&nbsp;"))
      }else{
        tableText <- data.table(rows = c("<tr><td style='width: 100px'> <hr style='border-width: 1px !important; border-color: #FAFAFA !important; text-align: center; margin: 0;'> </td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='dashed'> </td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='dots'> </td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='dash-dot'> </div></td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='longdash'> </td></td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='twodash'> </td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='dash-dot-dot'> </div> </td><td> &nbsp;: ",
                                         "<tr><td style='width: 100px'> <div class='dash-dot-dot-dot'> </td><td> &nbsp;: "),
                                names=rep(NA,8),
                                closings="</td></tr>",
                                id=1:8)
        InAcCodeTab <- EdgeTab$data %>% select(Inter_type, value) %>% distinct() %>% as.data.table()
        InAcCodeTab$Inter_type <- gsub("<|>","",InAcCodeTab$Inter_type)
        
        setkey(InAcCodeTab,value)
        tableText$names <- InAcCodeTab$Inter_type[1:8]
        #       tableText[InAcCodeTab, on=c(id="value"), names:=i.Inter_type]
        tableText <- tableText[!is.na(names)]
        if (nrow(tableText)>7) {tableText$names[8] <- "various"}
        
        returnTable <- paste0("<table>",
                              paste0(tableText[,rows],tableText[,names],tableText[,closings],collapse=""),
                              "</table>")
        
        return(HTML(returnTable))
      }
    })
    
    
    ### MAP VIEW
    
    # Change Country list choices in dropdown menu
    # ------------------------------------------------
    observeEvent(input$CountryList_RdBtn, {
      updateSelectInput(session, 
                        inputId="Country_drdn",
                        choices=c("<Select Country>"="-", nameSet[[input$CountryList_RdBtn]]),
                        selected=input$Country_drdn)
    })
    
    # center view on selected country (Map box bottom)
    # ------------------------------------------------
    observe({
      shiny::validate(need(input$Country_drdn, message=FALSE))
      showCountry <- input$Country_drdn
      if (showCountry == "-") {
        leafletProxy('MapPlot') %>%
          flyTo(lng =  initView$lng, lat = initView$lat, zoom = initView$zoom) # initial view
      }else{
        bbox <- as.list(st_bbox(shp$geometry[shp$NAME==showCountry]))
        leafletProxy('MapPlot') %>%
          flyToBounds(lng1=bbox$xmin, lng2=bbox$xmax, lat1=bbox$ymin, lat2=bbox$ymax)
      }
    })
         
    # ============== #
    #   World Plot   #
    # ============== #
    
  #  MDO <- reactive( getMapDataObject(shp=shp, Carabidae_selection=dataFilter()) )

    # plot world map
    # --------------
    output$MapPlot <- renderLeaflet({
      # render w/o polygon layers
      worldMap <- getPlainMap(mapView=initView, activeLayer=activeLayer)
      
      # # full render
      # worldMap <- renderMap(isolate(MDO()), activeLayer= activeLayer, mapView=initView)
      
     # Carabidae_World
    })

    
    # update polygon/control layers (and add them initially)
    # ------------------------------------------------------
    observe({
        #shiny::validate(need(input$MapPlot, message=FALSE)) # ERROR: alwaws false
      
        # change shapes (=polygons) and controls
        leafletProxy('MapPlot') %>%
            updatePolygons(MDO=getMapDataObject(shp=shp, Carabidae_selection=dataFilter()), activeLayer=activeLayer, mapView=getView())
    })
    
    
    # functionality / behaviour
    # =========================
    
    # switch non-active polygon layer off (simulating radio buttons with check boxes)
    # -----------------------------------------------------------------------------------
    observeEvent(input$MapPlot_groups,{
        # shiny::validate(need(input$MapPlot, message=FALSE)) --- switches off the function --- don't use it!
        selected_groups <- req(input$MapPlot_groups)
        if (("country resolution" %in% selected_groups) & (activeLayer != "country resolution")) {
            leafletProxy('MapPlot') %>%
                hideGroup("world region resolution") %>%
                removeControl(layerId = "worldRegionResolutionLegend")
            activeLayer <<- "country resolution"
        }else if(("world region resolution" %in% selected_groups) & (activeLayer != "world region resolution")) {
            leafletProxy('MapPlot') %>%
                hideGroup("country resolution") %>%
                removeControl(layerId = "countryResolutionLegend")
            activeLayer <<- "world region resolution"
        }
    })
    
    # "Map click"- filter for polygons
    # --------------------------------
    observeEvent(input$MapPlot_shape_click, {
        CountryID <- input$MapPlot_shape_click
        if (CountryID$group == "world region resolution") {
            Countries <- paste0(shp$NAME[grep(names(subreg_name_lookupTab)[subreg_name_lookupTab==CountryID$id], x=as.character(shp$SUBREGION))], collapse="|")
           # Countries <- gsub("\\s","\\\\\\s",Countries)
        }else{
            Countries <- shp$NAME[shp$NAME==CountryID$id]
            # updateSelectInput(session, inputId="Country_Selection", select=Countries)
        }
        ReactVal$CountryClickSelection <- Countries
        #Countries <- paste0("[", Countries, "]")
        updateSearch(DTproxy, keywords = list(global = NULL, columns = c("","","","","","",Countries)))
    })
    
    # ================ #
    #   Table          #
    # ================ # 
    
    # table on the front page
    # -----------------------
    
    # table proxy to enable updates
    DTproxy <- dataTableProxy("CarabidaeTabFront")
    
    output$CarabidaeTabFront <- renderDT({
        DT <- copy(dataFilter())

        ### DT <- DT_child_row(DT)
        # Country column is no 8 # see/search: observeEvent(input$MapPlot_shape_click,
        
        DT <- DT_GBIF_button(DT)
        # Country column is no 7
        
        ### DT <- DT_direct_links(DT)
        # Country column is no 5
        
    })
    
    
    # Table in Table-tab (this one is scrollable)
    # ---------------------------
    
    output$CarabidaeTable <- renderDT({
        DTab <- datatable(
            dataFilter()[,.(Carabidae_link, Fungus_link, Inter_type, Citation, Country)],
            escape=FALSE,
            selection='none',
            rownames=F,
            colnames=c('Carabidae Names'='Carabidae_link',
                       'Fungus Names'='Fungus_link',
                       'Interaction Type'='Inter_type'),
            extensions = 'Scroller',
            options = list(
                deferRender = TRUE,
                scroller = F,
                pageLength = 18
            )
        ) %>%
            formatStyle(columns=seq(1,5), color = 'black')
        
        return(DTab)
    })
    
    # Download button [table-tab] (to download currently selected records only)
    # -------------------------------------------------------------------------
    output$exportTabCSV <- downloadHandler(
        filename = paste0("Carabidae-Fungus-Interaction_",Sys.Date(),".csv"),
        content = function(file) {
            Tab2Export <- copy(dataFilter())
            setnames(Tab2Export, old = c('GBIFID_Carabidae','Carabidae_name','GBIFID_Fungus','Fungus_name','Inter_type'), new = c('GBIF-ID (Carabidae)','Carabidae Name','GBIF-ID (Fungus)','Fungus Name','Interaction Type'))
            
            cnames <- colnames(Tab2Export) 
            cnames <- cnames[!(cnames %chin% c('GBIF-ID (Carabidae)', 'Carabidae Name', 'GBIF-ID (Fungus)', 'Fungus Name', 'Interaction Type', 'References', 'Country'))]
            
            Tab2Export[,(cnames):=NULL]
            
            fwrite(Tab2Export, 
                   file = file,
                   append = FALSE,
                   quote = "auto",
                   sep = ",",
                   na = "", 
                   dec = ".", 
                   row.names = FALSE, 
                   col.names = TRUE,
                   qmethod = "escape",
                   scipen = 8L,
                   yaml = T,
                   verbose = FALSE)
            rm(Tab2Export)
        },
        contentType="text/csv"
    )
    
    output$exportTabXLSX <- downloadHandler(
      filename =  paste0("output/Carabidae-Fungus-Interaction_",Sys.Date(),".xlsx"),
      content = function(file) {
        # re-formatting data
        # -----------------
        Tab2Export <- copy(dataFilter())
        setnames(Tab2Export, old = c('GBIFID_Carabidae','Carabidae_name','GBIFID_Fungus','Fungus_name','Inter_type'), new = c('GBIF-ID (Carabidae)','Carabidae Name','GBIF-ID (Fungus)','Fungus Name','Interaction Type'))
        
        cnames <- colnames(Tab2Export) 
        cnames <- cnames[!(cnames %chin% c('GBIF-ID (Carabidae)', 'Carabidae Name', 'GBIF-ID (Fungus)', 'Fungus Name', 'Interaction Type', 'References', 'Country'))]
        
        Tab2Export[,(cnames):=NULL]

        # building xlsx file
        # ------------------
        workbook <- xlsx::createWorkbook(type="xlsx")
        sheet1 <- xlsx::createSheet(workbook, sheetName = "Carabidae - Fungus-Interactions")
        
        # text <- "Ben Fekih et al., 2020. The Covid19 of Carabidaes: The slavation of pandemies. The Carabidae Journal 1(1): 1:10. doi: 10.1111/japh.2020.act6969.x"
        # xlsx.addHeader(workbook, sheet1, value=text, level=4, color="black", underline=1)
        
        text2 <- paste0("Ben Fekih et al., 2020\n",
                        "The Covid19 of Carabidaes: The slavation of pandemies.\n",
                        "The Carabidae Journal 1(1): 1:10.\n", "doi: 10.1111/japh.2020.act6969.x")
        r2excel::xlsx.addParagraph(workbook, sheet1,
                                   value=text2,
                                   isItalic=F,
                                   colSpan=4,
                                   rowSpan=6,
                                   fontColor="#353535",
                                   fontSize=13)
        
        if (!all(dim(Tab2Export)==dim(Carabidae_data))) {
            r2excel::xlsx.addParagraph(workbook, sheet1,
                                       value="Selected Data:",
                                       isItalic=F,
                                       isBold=T,
                                       startRow=8,
                                       startCol=1,
                                       colSpan = 1,
                                       rowSpan = 1,
                                       fontColor="#010101", # at the time of implementation: white and black are inverted. An 'almost black' mighht be more resistent to time ...
                                       fontSize=13)
        }
        
        xlsx.addTable_mod(wb=workbook, 
                          sheet=sheet1, 
                          data=Tab2Export,
                          startCol=1,
                          startRow = 10,
                          row.names=F, 
                          fontSize=10, 
                          colnamesFill='#DEE6F0',
                          rowFill=c("#FFFFFF","#E6E6E6"))
        
        saveWorkbook(workbook, file=file)
      }
    )
   

    # in the node table the ID column need to be named 'name', and the tooltip column 'title'
    # ================ #
    #   Network        #  TTTTT
    # ================ #
    
    Carabidae_network <- reactive({
      if (nrow(dataFilter())>0) {
        
        edges <- copy(dataFilter())
        # Carabidae network object
        
        edges[, ':='(Carabidae_link=NULL, Fungus_link=NULL, References=NULL, Citation=NULL, Country=NULL, Year=NULL) ]
        setcolorder(edges, c("GBIFID_Carabidae", "GBIFID_Fungus", "Carabidae_name", "Fungus_name", "Inter_type"))
        
        edges[,':='(value = match(Inter_type, unique(Inter_type, nmax = NA)),
                    id = 1:.N)]
        edges[,dashes:='false']
        # edges[,dashes := data.table::fcase( # value here is the value column
        #     value==1, 'false',                # solid
        #     value==2, "[20]",                # dash     # paste0("[", edges$value[edges$value==2] * 10,"]")
        #     value==3, "[1,24]",              # dot      # paste0("[1,",edges$value[edges$value==3] * 8,"]")
        #     value==4, "[40,32,1,32]",        # dotdash  # paste0("[", edges$value[edges$value==4] * 10,",", edges$value[edges$value==4] * 8,",1,",edges$value[edges$value==4] * 8,"]")
        #     value==5, "[70,15]",             # long dash
        #     value==6, "[60,20,30,20]",       # two dash
        #     value==7, "[50,42,1,42,1,42]",   # dash-2x dots
        #     default = "[50,42,1,42,1,42,1,42]"   # dash-3x dots # paste0("[", edges$value[edges$value==4] * 10,",", edges$value[edges$value==4] * 8,",1,",edges$value[edges$value==4] * 8,"]")
        #   )]
        setnames(edges, new = c('from','to'), old = c('GBIFID_Carabidae','GBIFID_Fungus'))
        
        nodes <- data.table(name=as.integer(unique(c(edges$GBIFID_Carabidae, edges$GBIFID_Fungus))))
        nodes <- nodes[tax_tab[,c("GBIFID", "node_name", "kingdom")], on=.(name = GBIFID) ]
        nodes[,':='(title = gsub("\\n","<br>",str_wrap(string=node_name, width=15)),
                    kingdom = ifelse(kingdom=="Animalia","Carabidaes",kingdom),
                    type = kingdom=="Fungi")]
        setnames(nodes, new = c('group', 'label'), old = c('kingdom','node_name'))
        
        # Species ID to positional ID (node table)
        edges[,':='(from = match(from, nodes$name),
                    to = match(to, nodes$name))]
        
        Carabidae_network <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "name")
        
        EdgeTab$data <- edges
        return(Carabidae_network)
      }else{
        return(NULL)
      }
    })
    
    output$network <- renderVisNetwork({
        if (!is.null(Carabidae_network())) {
          
            # if(first) {
            #   net <- Carabidae_network() %>% 
            #     activate(nodes) %>%
            #     left_join(coord, by=c('name'='id'))
            # }else{
            #   net <- Carabidae_network()
            # }
          
            net <- visIgraph(Carabidae_network(), type = "full", idToLabel=F) %>%
                visInteraction(hover = TRUE,
                               navigationButtons = TRUE,
                               tooltipStyle = tooltip_network) %>%
                visOptions(#selectedBy = list(variable = "label", multiple = T),
                           highlightNearest = list(enabled = TRUE,
                                                   degree = list(from = 1, to = 1),
                                                   algorithm = "hierarchical")) %>%
                visGroups(
                  groupname = "Carabidaes",
                  size = 25, # node types with shape will still vary in size ...
                  color = list(
                    background = isolate(input$BackgroundCol_Carabidae),
                    border = isolate(input$BorderCol_Carabidae),
                    highlight = isolate(input$HighlightCol_Carabidae)),
                  shape = "square") %>%
                visGroups(groupname = "Fungi",
                    color = list(
                      background = isolate(input$BackgroundCol_Fungus),
                      size = 25,
                      border = isolate(input$BorderCol_Fungus),
                      highlight = isolate(input$HighlightCol_Fungus)),
                    shape = "triangle") %>%
                visEdges(
                    smooth = FALSE,
                    shadow = FALSE,
                    #scaling = list(customScalingFunction= function(x) {log10(x+0.1)}),
                    arrows = list(to = FALSE, from = FALSE),
                    color = list(color = input$EdgeCol,
                                 highlight = input$HighlightCol_Edges)) %>%
            #visConfigure(enabled=T, container="networkLegendTest") %>%
              visIgraphLayout(layout = input$NetworkLayout)
            
            # if(first) {
            #     first <<- F
            #     print("w/o function")
            # }else{
            #   print("layout function")
            #     net <- net %>%
            #       visIgraphLayout(layout = input$NetworkLayout)
            # }
            
            return(net)
        }else{
          showNotification(shiny::HTML("<b>Empty Network Exception:</b><br/>Table to calculate the network from has zero rows."), type="warning", duration=10)
        }
    })

    
    # highlight selected nodes in network
    #------------------------------------
    observe({
      shiny::validate(need(input$findCarabidae_node, message=FALSE), need(input$findFungus_node, message=FALSE))
      
        Carabidae <- input$findCarabidae_node
        Fungus <- input$findFungus_node
        
        nodes_selection <- ifelse(is.null(Fungus), "", # yes=NULL caused an error
                                  ifelse (test=Fungus != "-", yes=Carabidae_data[Fungus_name==Fungus, GBIFID_Fungus][1], no=""))
        nodes_selection <- unlist(ifelse(test = is.null(Carabidae), 
                                  yes = nodes_selection,
                                  no = ifelse(test=Carabidae != "-", 
                                              # not using list() return only the first value in c() ...
                                              yes = list(c(as.character(Carabidae_data[Carabidae_name==Carabidae, GBIFID_Carabidae][1]), nodes_selection)),
                                              no = nodes_selection)))
        
        nodes_selection <- nodes_selection[nodes_selection!=""]

        visNetworkProxy("network") %>%
            visSelectNodes(id = as.character(nodes_selection))
    })
    
    # update node colours
    # -------------------
    observe({
        shiny::validate(need(input$BorderCol_Fungus, message=FALSE), need(input$HighlightCol_Fungus, message=FALSE),
                        need(input$BackgroundCol_Fungus, message=FALSE))
        visNetworkProxy("network") %>%
            visGroups(
                groupname = "Carabidaes", 
                color = list(
                  background = input$BackgroundCol_Carabidae,
                  border = input$BorderCol_Carabidae,
                  highlight = input$HighlightCol_Carabidae)) %>%
          visGroups(
              groupname = "Fungi", 
              color = list(
                background = input$BackgroundCol_Fungus,
                border = input$BorderCol_Fungus,
                highlight = input$HighlightCol_Fungus))
    })
    
    # update node shape
    # -------------------
    observe({
      shiny::validate(need(input$ShapeIpt_Carabidae, message=FALSE), need(input$ShapeIpt_Fungus, message=FALSE))
      visNetworkProxy("network") %>%
        visGroups(
          groupname = "Carabidaes",
          shape = input$ShapeIpt_Carabidae) %>%
        visGroups(
          groupname = "Fungi", 
          shape = input$ShapeIpt_Fungus)
    })

    
    
    # ================ #
    #   Histogram      #
    # ================ #
    
    output$HistogramDataSet=renderGvis({
        Carabidaes <- Carabidae_data[Year!= 0 & !is.na(Year),][,.('Count'=.N), by=Year]
        setorder(Carabidaes, -Year)

        gvisOptions['colors'] <- "['#0a4d48']"
        gvisOptions['title'] <- '... in the Data Set'
        histDataSet <- gvisBarChart(Carabidaes, xvar='Year', yvar='Count', options=gvisOptions)
        return(histDataSet)
    })

    output$HistogramSelection=renderGvis({
        selection <- dataFilter()[Year!= 0 & !is.na(Year),][,.(Count=.N), by=Year]
        setorder(selection, -Year)
        
        gvisOptions['colors'] <- "['#8d6600']"
        gvisOptions['title'] <- '... in Selected Time Period'
        histSelect <- gvisBarChart(selection, xvar='Year', yvar='Count', options=gvisOptions)

        ## Can we make it automatically scalable according to window size?
        #
        # tmp$html$chart["jsDrawChart"] <- sub("var chart = new google",
        #                                      "function resize () { var chart = new google",
        #                                      x = tmp$html$chart["jsDrawChart"], fixed=T)
        # 
        # tmp$html$chart["jsDrawChart"] <- sub("draw(data, options);",
        #                                      "draw(data, options); } window.onload = resize; window.onresize = resize;",
        #                                      x = tmp$html$chart["jsDrawChart"], fixed=T)
        #-----
        # see: https://stackoverflow.com/questions/8950761/google-chart-redraw-scale-on-window-resize
        #tmp$html$chart["jsDrawChart"] <- paste0(tmp$html$chart["jsDrawChart"], "$(window).resize(function() { if(this.resizeTO) clearTimeout(this.resizeTO); this.resizeTO = setTimeout(function() { $(this).trigger('resizeEnd'); }, 500); }); $(window).on('resizeEnd', function() { drawChart(data); });")
        
        return(histSelect)
    })
      
    # ================ #
    #  Sunburst Plot   #
    # ================ #
    
    output$SunburstPolt <- renderPlotly({ # c("kingdom","phylum","order","family","genus","species")
        sunburst <- NULL # return object

        tableSelection <- copy(dataFilter())
        # ReactVal$CountryClickSelection has the value '*' as default
        tableSelection <- tableSelection[grep(ReactVal$CountryClickSelection, tableSelection$Country),]

        if (nrow(tableSelection)>0) {

          tableSelection <-data.table(GBIFID=c(tableSelection$GBIFID_Carabidae, tableSelection$GBIFID_Fungus))
          setkey(tableSelection, GBIFID)
          setkey(tax_tab, GBIFID)
          tableSelection <- tableSelection[tax_tab, nomatch=0]
          tableSelection <- tableSelection[GBIFID != -1][,c("kingdom","phylum","order","family","genus","species")]
          
          # Replacing NA with the parent allows to remove label/parent pairs without disrupting the branch. (Species names which are NA can be dropped immediately later.)
         # tableSelection[, genus := ifelse(is.na(genus), family, genus)]
          
          tableSelection_Fungi <- tableSelection[kingdom == "Fungi"]
          tableSelection_Fungi[, phylum := ifelse(is.na(phylum), kingdom, phylum)]  # those changes need to stay sequential
          tableSelection_Fungi[, order  := ifelse(is.na(order), phylum, order)]
          tableSelection_Fungi[, family := ifelse(is.na(family), order, family)]
          tableSelection_Fungi[, genus := ifelse(is.na(genus), family, genus)]

          tableSelection_Carabidae <- tableSelection[kingdom == "Animalia"]
          tableSelection_Carabidae[, family := ifelse(is.na(family), kingdom, family)]
          tableSelection_Fungi[, genus := ifelse(is.na(genus), family, genus)]

          tableSelection <- data.table::rbindlist(
            l = list(tableSelection[,.(label = kingdom, parent="")],
                     tableSelection_Fungi[,.(label=phylum, parent=kingdom)],
                     tableSelection_Fungi[,.(label=order, parent=phylum)],
                     tableSelection_Fungi[,.(label=family, parent=order)],
                     tableSelection_Fungi[,.(label=genus, parent=family)],
                     tableSelection_Carabidae[,.(label=family, parent=kingdom)],
                     tableSelection_Carabidae[,.(label=genus, parent=family)],
                     tableSelection_Carabidae[!is.na(species),][,.(label=species, parent=genus)]),
            use.names = F
          )
          tableSelection <- tableSelection[label!=parent,] # first filter, then counting (!!!) to get the right numbers
          tableSelection <- unique(tableSelection[, ':='(value = .N, ids = label), by = label])
          
          # tableSelection
          # # A tibble: 663 x 4
          #   label            parent       value ids             
          #   <chr>            <chr>        <int> <chr>           
          # 1 Animalia         ""            5688 Animalia        
          # 2 Fungi            ""            5218 Fungi           
          # 3 Zygomycota       "Fungi"       4394 Zygomycota      
          # 4 Ascomycota       "Fungi"        812 Ascomycota      
          # 5 Basidiomycota    "Fungi"          7 Basidiomycota   
          # 6 Chytridiomycota  "Fungi"          5 Chytridiomycota 
          # 7 Entomophthorales "Zygomycota"  4377 Entomophthorales
          # 8 Hypocreales      "Ascomycota"   544 Hypocreales     
          # 9 Capnodiales      "Ascomycota"   147 Capnodiales     
          # 10 Glomerellales    "Ascomycota"    18 Glomerellales   
          # # … with 653 more rows
          
          # plotly_4.9.2.1
          # ==============
          # officially known about the layout function (at least by the community): 
          # Warning message:
          # 'layout' objects don't have these attributes: 'sunburstcolorway', 'extendsunburstcolors'
          # ...
          # For now: you need to live with this warning message
          
          sunburst <- plot_ly(
            labels =  tableSelection$label,
            parents = tableSelection$parent,
            values =  tableSelection$value,
            maxdepth = 4,
            type = 'sunburst') %>%
            layout(
              margin = list(l = 1, r = 1, b = 3, t = 3),
              sunburstcolorway = c("#62c2e8","#85bd81","#64c4af","#d9f5be")[c(1,2)],
              extendsunburstcolors = TRUE,
              paper_bgcolor='rgba(0,0,0,0)',
              plot_bgcolor='rgba(0,0,0,0)')
        }
        return(sunburst)
    })

    # radial network
    output$radial <- renderDiagonalNetwork({ # renderRadialNetwork({
      # ReactVal$CountryClickSelection has the value '*' as default
      # we don't need to worry about missing GBIF IDs since we start to work with the taxonomy table (we don't need to count occurrences)
      tableSelection <- tax_tab[GBIFID %in% dataFilter()[grep(pattern=ReactVal$CountryClickSelection, x=Country), c(GBIFID_Carabidae, GBIFID_Fungus)]]
      
      if (nrow(tableSelection)>0) {
          # tableSelection[, pathString := ifelse(kingdom=="Animalia",
          #                                       paste("cellular Organism", kingdom,                family, genus, species, sep = "/"),
          #                                       paste("cellular Organism", kingdom, phylum, order, family, genus, species, sep = "/"))]
          # tableSelection[, pathString := gsub("/NA","", pathString, fixed=T)]
          
          #rN <- networkD3::radialNetwork(
          dN <- networkD3::diagonalNetwork(
            data.tree::ToListExplicit(
              data.tree::as.Node(tableSelection,
                                 pathDelimiter = "/"),
                      unname = TRUE), 
                  linkColour = "#2f6b89", # "#0c6d5d",
                  nodeColour = "#939f80",
                  nodeStroke = "#577a79",
                  textColour = "#445854"
                )
          return(dN)
      }else{
        return(NULL)
      }
    })
}

shinyApp(ui, server)
