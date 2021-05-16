
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

# Data table
library(DT)

# Map
library(leaflet)
library(leaflet.extras)

# graphical output other than shiny
library(RColorBrewer) # for color of leaflet polygonlayer

library(googleVis)

# Network
library(igraph)
library(visNetwork)
library(tidygraph)

# interactive circular plot
library(data.tree) # also for radial network
library(circlepackeR)
# devtools::install_github("jeromefroe/circlepackeR")

# interactive sunburst plot
library("plotly")

#interactive treemap
library(treemap)
library(d3treeR)
# remotes::install_github("d3treeR/d3treeR")
library(ECharts2Shiny)
library(jsonlite)

library(d3r)

# radial network
library(networkD3)


# ============================================= #
# load function                                 #
# ============================================= #

# custom modifications for performance reasons (r2excel::xlsx.addTable())
source("data/function_xlsx.addTable_mod.r")
source("data/function_cite_csv_v2_DT.r")

# trim <- function(x) {gsub('^\\s+|\\s+$','',x)}


# ============================================= #
# "variables":                                  #
# ============================================= #

subreg_name_lookupTab <- c("Latin America and the Caribbean", "Sub-Saharan Africa", "Western Europe", "Northern Europe", "Eastern Europe and Northern Asia", "Western Asia", "Central Asia", "Polynesia", "Micronesia", "Melanesia", "Australia and New Zealand", "Southern Europe", "South-eastern Asia", "Southern Asia", "Eastern Asia", "Caribbean", "Northern America", "Southern Africa", "Middle Africa", "Northern Africa", "Eastern Africa", "Central America", "Western Africa", "South America","CCAMLR", "British Indian Ocean Territory", "United States Minor Outlying Islands")
names(subreg_name_lookupTab) <- c("419", "202", "155", "154", "151", "145", "143", "61", "57", "54", "53", "39", "35", "34", "30", "29", "21", "18", "17", "15","14","13","11","5","999","998","997")


# initView <- list(lng=23.106111, lat=53.5775, zoom=3) # Europe
initView <- list(lng=0, lat=8.754795, zoom=1) # World

readme_file <- "data/Readme.Rmd"

# ============================================= #
# load species data                             #
# ============================================= #

load("data/carabidae.RDA", verbose=T)
source("data/function_collection.R", local=T, verbose=T)

Carabidae_data<-as.data.table(Carabidae_data)
tax_tab<-as.data.table(tax_tab)


# ============================================= #
# load shapefile                                #
# ============================================= #

shp <- st_read("data/GIS_data_mod/TM_WORLD_BORDERS-0.3_modified_small30.shp")

# shp <- st_read("../data/GIS_data_mod/TM_WORLD_BORDERS-0.3_modified_2.shp")
shp$NAME <- as.character(shp$NAME)
#setting projection to WGS84
shp <- st_set_crs(shp, 4326)

# providing space for 'count' and human readable subregion names
shp$Freq <- NA 
shp$SubRegSum <- NA # ID column (helper column) to summarise the subregion counts


subreg_name_lookupTab <- subreg_name_lookupTab[names(subreg_name_lookupTab) %chin% shp$NAME]
id <- fastmatch::fmatch(names(subreg_name_lookupTab), shp$NAME)
shp$SubRegSum[id] <- as.integer(shp$NAME[id])

# purely human readable names now
shp$NAME[id] <- subreg_name_lookupTab

rm(id)

# ============================================= #
# modifying data                                #
# ============================================= #

# connection table
Carabidae_data[, Citation := References]
Carabidae_data[, References := gsub('&amp;',"&", gsub("</b> \\(", ", ", gsub("\\)\\..*", '', gsub("^.*'content': '<b>", "", as.character(References)))))]
Carabidae_data$References[c(3852,4915,7523,7527,879,885,8867,887,889,9064,9065,9066,9067,9068,9069,9070,9071,9072,9073,9074,9075,9080,9081,9082,9083,9115,9116,9117,9118)] <- "NA"


Carabidae_data[, Citation := paste0("<!--", References, "-->", Citation)]
Carabidae_data[, Country := ifelse(is.na(Country)," ",Country)]
Carabidae_data[, Inter_type := ifelse(is.na(Inter_type),"NA",Inter_type)]


Carabidae_data[,GBIFID_Carabidae := ifelse(GBIFID_Carabidae==0, -1, GBIFID_Carabidae)]
Carabidae_data[,GBIFID_Fungus := ifelse(GBIFID_Fungus==0, -1, GBIFID_Fungus)]

# temp line of code
Carabidae_data <- Carabidae_data[GBIFID_Carabidae != -1 & GBIFID_Fungus != -1]


Carabidae_data[,Year := suppressWarnings(as.integer(ifelse(grepl(x=References, pattern="collection"), 0, gsub(".*?([0-9]+).*", "\\1", References))))]
Carabidae_data[, Year := ifelse(is.na(Year), 0, Year)]


dim(tax_tab)
# [1] 2729   13
tax_tab <- unique(tax_tab, by="GBIFID")
dim(tax_tab)
# [1] 2695   13

# we need to make use of the pathString again ... but for now let's take it out ...
tax_tab[,pathString := NULL]
tax_tab[,n := NULL]

# ... hereby done
tax_tab[, pathString := ifelse(kingdom=="Animalia",
                               paste("cellular Organism", kingdom,                family, genus, species, sep = "/"),
                               paste("cellular Organism", kingdom, phylum, order, family, genus, species, sep = "/"))]
tax_tab[, pathString := gsub("/NA","", pathString, fixed=T)]


# throughing out Carabidae and Fungi (entire connection) which are not in the taxonomy table
common_GBIFIDs <- intersect(tax_tab$GBIFID, c(Carabidae_data$GBIFID_Carabidae, Carabidae_data$GBIFID_Fungus))
# length(unique(common_GBIFIDs))
# [1] 2500
# 
# length(unique(c(Carabidae_data$GBIFID_Carabidae,  <----- !!!! 9 more
#                 Carabidae_data$GBIFID_Fungus)))
# [1] 2509

dim(Carabidae_data)
# [1] 9870   13
Carabidae_data <- Carabidae_data[GBIFID_Carabidae %in% common_GBIFIDs]
dim(Carabidae_data)
# [1] 9864   13
Carabidae_data <- Carabidae_data[GBIFID_Fungus %in% common_GBIFIDs]
dim(Carabidae_data)
# [1] 9858   13
length(unique(c(Carabidae_data$GBIFID_Carabidae, 
                Carabidae_data$GBIFID_Fungus)))
# [1] 2498


# unknown_Carabidae_additions <- unique(Carabidae_data[GBIFID_Carabidae== -1], by="Carabidae_name")
# unknown_Carabidae_additions[,GBIFID_Carabidae:=1:.N *-1]
# 
# tax_tab <- rbindlist(l=list(tax_tab, ),
#                      use.names = T,
#                      fill
#                      )



Carabidae_data[tax_tab, on = c(GBIFID_Carabidae="GBIFID"), Genus_Carabidae := i.genus]
Carabidae_data[tax_tab, on = c(GBIFID_Fungus="GBIFID"), Genus_Fungus := i.genus]


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

# interface
titleString <- tags$div(HTML('<i class="fa fa-bug" style = "color:#F39C12;" aria-hidden="true"></i> 
                             <b style = "color:#f1faf8">Carabidae - Fungi interactions</b>'))

tabNames <- c("mainTab", "tableTab", "accTab", "aboutTab","radNet")
names(tabNames) <- c("1","2","3","4","5")


Carabidae_World <- getPlainMap(mapView=initView, activeLayer = "world region resolution")

# histograms
gvisOptions=list(width='auto',
                 height='425', 
                 backgroundColor='transparent', 
                 vAxis='{textStyle:{color:"#26272d"}}',
                 hAxis='{textStyle:{color:"#26272d"}}',
                 titleTextStyle='{color:"#26272d"}',
                 legend="{position: 'top', maxLines: 1}", 
                 axes="{x:{ side: 'bottom'}}",
                 hAxis.format="format: 'none'",
                 hAxis.slantedText='true',
                 colors= "['#2e662a', '#662b00']",
                 opacity="['.2', '.2']",
                 vAxis.gridlines.color="#0F0",
                 hAxis.gridlines.color="#F00",
                 vAxis.direction = -1,
                 title='Number of Publications per Year',
                 hAxis.maxValue = "automatic")
#vAxis.maxValue

# network - styling tooltips
tooltip_network <- 
  'position: fixed;
    visibility: hidden;
    padding: 2px;
    white-space: nowrap;
    font-size:16px;
    border: 1px solid;
    border-radius: 5px;
    color: #333333;
    border-color: #CCCCCC;
    background-color: #FAFAFA;'


# network - icons for shape selection dropdown menu
shapeTab <- data.frame(
  returnVal = c("ellipse", "database", "box", "circle", "square", "triangle", "triangleDown", "dot", "star", "diamond"),
  img = c(
    "<img src='menu-ellipse-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;ellipse&nbsp;</div></img>",
    "<img src='menu-database-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;db&nbsp;</div></img>",
    "<img src='menu-box-solid.svg' width=30%><div class='shapeIcon_dpdn'></div>&nbsp;box&nbsp;</img>",
    "<img src='menu-circle-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;circle&nbsp;</div></img>",
    "<img src='menu-square-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;square&nbsp;</div></img>",
    "<img src='menu-triangle-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;triangle 1&nbsp;&nbsp;</div></img>",
    "<img src='menu-triangleDown-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;triangle 2&nbsp;&nbsp;</div></img>",
    "<img src='menu-dot-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;dot&nbsp;</div></img>",
    "<img src='menu-star-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;star&nbsp;</div></img>",
    "<img src='menu-diamond-solid.svg' width=30%><div class='shapeIcon_dpdn'>&nbsp;diamond&nbsp;</div></img>")
)

# network - construct legend
shapes <- c(box="<path style='fill:#404040;' d='M383.8,348.2H128.2c-22.3,0-40.3-18-40.3-40.3V204.1c0-22.3,18-40.3,40.3-40.3h255.5c22.3,0,40.3,18,40.3,40.3 v103.8C424.1,330.2,406,348.2,383.8,348.2z'/>",
            circle="<circle style='fill:#404040;' cx='256' cy='256' r='170'/>",
            database="<path style='fill:#404040;' d='M425.4,173.8v191.8c0,32.1-75.8,58.2-169.4,58.2S86.6,397.8,86.6,365.7V173.8c8.7,8.8,22.9,17.7,45.5,25.5 c33.3,11.4,77.2,17.7,123.8,17.7s90.6-6.3,123.8-17.7C402.5,191.5,416.7,182.7,425.4,173.8z M256,88.1 c-93.5,0-169.4,26.1-169.4,58.2s75.8,58.2,169.4,58.2s169.4-26.1,169.4-58.2S349.5,88.1,256,88.1z'/>",
            diamond="<rect style='fill:#404040;' x='149.2' y='149.2' transform='matrix(0.7071 -0.7071 0.7071 0.7071 -106.0387 256)' width='213.7' height='213.7'/>",
            dot="<circle style='fill:#404040;' cx='256' cy='256' r='151.5'/>",
            ellipse="<ellipse style='fill:#404040;' cx='256' cy='256' rx='233.4' ry='109.2'/>",
            square="<rect style='fill:#404040;' x='104.9' y='104.9' width='302.2' height='302.2'/>",
            star="<polygon style='fill:#404040;' points='256,102.6 297.2,222.2 417.2,219.8 319.4,294.6 355.7,409.4 255.5,339.4 156.3,409.4 192.6,293.3 94.8,219.8 216.1,222.2'/>",
            triangle="<polygon style='fill:#404040;' points='258.1,104.9 432,407.1 80,407.1'/>",
            triangleDown="<polygon style='fill:#404040;' points='253.9,407.1 80,104.9 432,104.9'/>")


# ============================================= #
# UI                                            #
# ============================================= #


ui <- dashboardPagePlus(
    collapse_sidebar = FALSE,
    sidebar_fullCollapse = T,
    skin="green",

    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    
    useShinyjs(),

# ====   H E A D E R   ====================================================================================================================
    header = dashboardHeaderPlus(
        title = titleString,
        enable_rightsidebar = FALSE,
        disable=F,
        fixed = F,
        left_menu = tagList(
            actionButton(inputId = "main_tab_btn", icon=icon("home"), label=NULL, class='btn-green', style="min-width: 40px;"),
            tags$button(
              id = "table_tab_btn",
              class="btn btn-default action-button btn-green",
              icon("table"),
              type="button",
              style="min-width: 40px;"
            ),
            tags$button(
              id = "about_tab_btn_acc",
              class="btn btn-default action-button btn-green",
              icon("info-circle"),
              type="button",
              style="min-width: 40px;"
            ),
            tags$button(
              id = "about_tab_btn_pl",
              class="btn btn-default action-button btn-green",
              img(src='book-open-solid.svg', style='width:1em;height:1em;'),
              type="button",
              style="min-width: 40px;"
            ),
            tags$button(
              id = "radNet_tab_btn",
              class="btn btn-default action-button btn-green",
              img(src='tree-solid.svg', style='width:1em;height:1em;'),
              type="button",
              style="min-width: 40px;"
            ),
            
            bsTooltip( id = "main_tab_btn", title = "main" ),
            bsTooltip( id = "table_tab_btn", title = "table" ),
            bsTooltip( id = "about_tab_btn_acc", title = "about" ),
            bsTooltip( id = "about_tab_btn_pl", title = "read more" ),
            bsTooltip( id = "radNet_tab_btn", title = "tree" )
        )
    ),

# ====   R I G H T - S I D E  B A R   =====================================================================================================
    rightsidebar = rightSidebar(
        background = "dark",

        sidebarMenu(
            id = "tabs",
            menuItem("Main", tabName = "mainTab", icon = icon("home")),
            menuItem("Table", tabName = "tableTab", icon = icon("table")),
            menuItem("About - accordion", tabName = "accTab", icon = icon("readme")),
            menuItem("About - plain", tabName = "aboutTab", icon = icon("readme")),
            HTML("<li><a href='#shiny-tab-radNet' data-toggle='tab' data-value='radNet'><img src='tree-solid.svg'' style='width:1em;height:1em;'/><span>Tree</span></a></li>")
        )
    ),
 
# ====   S I D E  B A R   =================================================================================================================
    sidebar = dashboardSidebar(
        
        sidebarMenu(
            tags$h3("Settings", color = "#eeeeee", align = "center"),
            
            tags$div(HTML(as.character("<button id='Toggle_DataFilter_btn' type='button' style='width: 85%;' class='btn btn-default action-button btn-dark'>\n  <img src='database-search-solid.svg' style='width:1em;height:1em'/>\n &nbsp;Filter Dataset\n</button>") )),

            sliderInput(
                inputId = 'years',
                label = "Years:",
                value = YearMinMax,
                min = YearMinMax[1],
                max = YearMinMax[2],
                sep = ""),
              
            tags$div(id="SepCen1", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
            
            
            tags$p(id="CarabidaeTitle","Carabidaes", style="margin: 12px 5px 0px 15px; font-size: 1.25em;"),
            radioButtons(
                inputId = "Carabidae_RdBtn",
                label = "Carabidae:",
                choiceNames = c("species","genus only"),
                choiceValues = c("CarabidaeSpecies","CarabidaeGenus"),
                selected = "CarabidaeSpecies",
                inline = T),
            pickerInput(
              inputId = "Carabidae_Selection",
              label = NULL,
              choices = nameSet$CarabidaeSpecies,
              selected = unname(unlist(nameSet$CarabidaeSpecies)),
              multiple = T,
              options = list(`actions-box` = TRUE,
                             `selected-text-format` = paste0("count > ", length(nameSet$CarabidaeSpecies) -1),
                             `count-selected-text` = "all")),
            
            tags$div(id="SepCen3", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
            
            radioButtons(
                inputId = "Fungus_RdBtn", 
                label = "Fungus:",
                choiceNames = c("species","genus only"), 
                choiceValues = c("FungusSpecies","FungusGenus"), 
                selected = "FungusSpecies",
                inline = T),
            pickerInput(
              inputId = "Fungus_Selection",
              label = NULL,
              choices = nameSet$FungusSpecies,
              selected = unname(unlist(nameSet$FungusSpecies)),
              multiple = T,
              options = list(`actions-box` = TRUE,
                             `selected-text-format` = paste0("count > ", length(unlist(nameSet$FungusSpecies)) -1),
                             `count-selected-text` = "all")),

            tags$div(id="SepCen2", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
            
            pickerInput(
                inputId = "InteractionType",
                label = "Type of Interaction:", 
                choices = nameSet$Interaction,
                selected = nameSet$Interaction,
                multiple = T,
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = paste0("count > ", length(nameSet$Interaction) -1),
                               `count-selected-text` = "all")),
            
            tags$div(id="SepCen4", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
            
            # radioButtons(
            #     inputId = "Country_RdBtn",
            #     label = "Country:",
            #     choiceNames = c("select", "exclude"),
            #     choiceValues = c("in","ex"),
            #     selected = "in",
            #     inline = T),
            pickerInput(
                inputId = "Country_Selection",
                label = "Country:",
                choices = c("unknown"=" ", nameSet$CountriesDB),
                selected = c("unknown"=" ", nameSet$CountriesDB),
                multiple = T,
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = paste0("count > ", length(nameSet$CountriesDB) -1),
                               `count-selected-text` = "all")),
              
            tags$div(id="SepCen5", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
            
            tags$div(id="clearDiv", style='margin-top: 20px;', HTML(as.character("<button id='ClearFilter_bt' type='button' class='btn btn-default action-button btn-dark' style='margin: 0px auto;'><i class=\"fa fa-refresh\"></i>&nbsp;Remove Filter\n</button>") )),
            # actionButton(
            #     inputId="ClearFilter_bt", 
            #     label="Remove Filter", 
            #     icon=icon(name="refresh"),
            #     class = "btn-dark"
            # )
            tags$div(id="SepCen6",tags$hr(style="border-style: double;")),
            
            
            # TOGGLE NETWORK COLOUR SETTINGS
            # actionButton: the 'palette' icon loaded, but had app-updating problems. We therefore load the icon as locally stored svg.
            tags$div(HTML(as.character("<button id='Toggle_NetworkLayout__menuBtn' type='button' style='width: 85%;' class='btn btn-default action-button btn-dark'><img src='project-diagram-solid.svg' style='width:1em;height:1em'/>&nbsp;Network Layout\n</button>") )),
            # actionButton(
            #   inputId="Toggle_NetLayoutMenu_btn", 
            #   label="Network Layout", 
            #   icon=icon(name="project-diagram"),
            #   class = "btn-dark"
            # ),
            
          # "layout_with_dh" did not end calculating  
            hidden(
              tags$p(id="NetworkLayout_title","Network Layout", style="margin: 12px 5px 0px 15px; font-size: 1.25em;"),
              div(id="NetworkLayout_div", style="display:inline-block;width:100%;text-align: center;",
                  selectInput(
                    inputId = "NetworkLayout",
                    label = NULL,
                    choices = c("layout_nicely","layout_as_bipartite","layout_as_star","layout_as_tree","layout_on_grid","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_gem","layout_with_graphopt","layout_with_kk","layout_with_lgl","layout_with_mds","layout_with_sugiyama"),
                    selected = "layout_nicely",
                    multiple = FALSE,
                    selectize = F)),
              tags$div(id="SepCen7",tags$hr(style="border-style: double;"))
            ),
            
            
            # TOGGLE NETWORK COLOUR SETTINGS
            # actionButton: the 'palette' icon loaded, but had app-updating problems. We therefore load the icon as locally stored svg.
            tags$div(HTML(as.character("<button id='Toggle_NWColour_menuBtn' type='button' style='width: 85%;' class='btn btn-default action-button btn-dark'><img src='palette-solid.svg' style='width:1em;height:1em'/> &nbsp;Network Colours\n</button>") )),
            # actionButton(
            #   inputId="Toggle_NWColour_menuBtn", 
            #   label="Network Colours", 
            #   icon=icon(name="palette"),
            #   class = "btn-dark"
            # ),
            
          hidden(
              tags$p(id="Col_Carabidae_title","Node Colour Carabidaes", style="margin: 12px 5px 0px 15px; font-size: 1.25em;"),
              div(id="BackgroundCol_Carabidae_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                      inputId = "BackgroundCol_Carabidae",
                      label = "Fill",
                      value = "#0085AF",
                      showColour = "background",
                      allowTransparent = TRUE,
                      closeOnClick = TRUE)),
              div(id="BorderCol_Carabidae_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                      inputId = "BorderCol_Carabidae",
                      label = "Border",
                      value = "#013848",
                      showColour = "background",
                      allowTransparent = TRUE,
                      closeOnClick = TRUE)),
              div(id="AColHigh_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                      inputId = "HighlightCol_Carabidae",
                      label = "Highlight",
                      value = "#FF8000",
                      showColour = "background",
                      allowTransparent = TRUE,
                      closeOnClick = TRUE)),
              
              tags$div(id="SepCen8", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
              
              tags$p(id="Col_Fungus_title","Node Colour Fungus", style="margin: 12px 5px 0px 15px; font-size: 1.25em;"),
              div(id="BackgroundCol_Fungus_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                    inputId = "BackgroundCol_Fungus",
                    label = "Fill",
                    value = "#1CAD2D",
                    showColour = "background",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE)),
              div(id="BorderCol_Fungus_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                    inputId = "BorderCol_Fungus",
                    label = "Border",
                    value = "#165211",
                    showColour = "background",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE)),
              div(id="FColHigh_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                    inputId = "HighlightCol_Fungus",
                    label = "Highlight",
                    value = "#FF8000",
                    showColour = "background",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE)),
              
              tags$div(id="SepCen9", tags$hr(style="text-align: center; margin: 5px 20px 5px 20px;")),
                  
              tags$p(id="EdgeCol_title","Edge Colour", style="margin: 12px 5px 0px 15px; font-size: 1.25em;"),
              div(id="EdgeCol_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                    inputId = "EdgeCol",
                    label = "Stroke",
                    value = "#474F6F",
                    showColour = "background",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE)),
              div(id="HighlightCol_Edges_div", style="display:inline-block;width:32%;text-align: center;",
                  colourpicker::colourInput(
                    inputId = "HighlightCol_Edges",
                    label = "Highlight",
                    value = "#C62F4B",
                    showColour = "background",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE)),
              tags$div(id="SepCen10", tags$hr(style="border-style: double;"))
            ),
           
          # actionButton: the 'palette' icon loaded, but had app-updating problems. We therefore load the icon as locally stored svg.
          tags$div(HTML(as.character("<button id='Toggle_NWShape_menuBtn' type='button' style='width: 85%;' class='btn btn-default action-button btn-dark'>\n  <img src='shapes-solid.svg' style='width:1em;height:1em'/>\n &nbsp;Network Symbols\n</button>") )),
          # actionButton(
          #     inputId="Toggle_NWShape_menuBtn",
          #     label="Network Symbols",
          #     icon=icon(name="shapes"),
          #     class = "btn-dark"
          #   ),
   
          hidden(
            tags$p(id="SymbolTitle","Node Symbols", style="margin: 12px 5px 0px 15px; font-size: 1.25em;"),
            div(id="ShapeIpt_Carabidae_div", style="display:inline-block;width:49%;text-align: center;",
                # pickerInput(
                #   inputId = "ShapeIpt_Carabidae",
                #   label = "Carabidaes",
                #   selected = "square",
                #   choices = list('label inside'=c("ellipse", "database", "box", "circle"), 'label outside'=c("square", "triangle", "triangleDown", "dot", "star", "diamond")))),
                pickerInput(
                    inputId = "ShapeIpt_Carabidae",
                    label = "Carabidaes",
                    selected = "square",
                    choices = shapeTab$returnVal,
                    choicesOpt = list(content = shapeTab$img))),
            
            div(id="ShapeIpt_Fungus_div", style="display:inline-block;width:49%;text-align: center;",
                pickerInput(
                  inputId = "ShapeIpt_Fungus",
                  label = "Fungi",
                  selected = "triangle",
                  choices = shapeTab$returnVal,
                  choicesOpt = list(content = shapeTab$img)))
            )
        )
    ),

    
# ====   B O D Y   ========================================================================================================================
    body = dashboardBody(
        tags$head(tags$link(rel="icon", type="image/x-icon", href="favicon.ico")),
        tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
      
        # error handling
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: visible;
                                                 content: 'An error occurred. Please contact the admin.'; }"),

        bsTooltip(id = "findCountry",
                  title = 'Center the current view of the world map to the selected country.<br><br> Zoom level might need to be adjusted manually.',
                  placement = "bottom",
                  trigger = "hover",
                  options = list(container = "body")),
        
        bsTooltip(id = "exportTab",
                  title = 'Download filtered Carabidae-Fungus interaction records.',
                  placement = "bottom",
                  trigger = "hover",
                  options = list(container = "body")),
        
        tabItems(
# ----- First tab content -----
            tabItem(tabName = "mainTab",
                fluidRow(
    # ----- left column -----
                    column(
                        width = 6, style='padding:0px;',
                        
                        boxPlus(
                            width = 12,
                            title = "Carabidae-Fungus Network", 
                            enable_dropdown = F,
                            footer_padding=F,
                            enable_label = F,
                            status = "info", 
                            solidHeader = FALSE, 
                            collapsible = TRUE,
                            closable = FALSE,
                            enable_sidebar = TRUE,
                            sidebar_width = 25,
                            sidebar_start_open = TRUE,
                            sidebar_content = tagList(
                                actionLink(inputId="highSpec_al", label="Highlight species", icon = NULL, style="text-decoration: underline;"),
                                tags$p(),
                                #tags$p("Higlight species inside the network:"),
                                hidden(
                                  selectInput(
                                    inputId="findCarabidae_node",
                                    label=NULL,
                                    choices=c("<Carabidae>"="-", unname(unlist(nameSet$CarabidaeSpecies))),
                                    selectize=T),
                                  selectInput(
                                    inputId="findFungus_node",
                                    label=NULL,
                                    choices=c("<Fungus>"="-", unname(unlist(nameSet$FungusSpecies))),
                                    selectize=T)
                                ),
                                tags$div(id="SepCen_sbar", tags$hr(style="text-align: center; margin: 20px 0px 15px 0px;")),
                                actionLink(inputId="legend_al", label="Legend", icon = NULL, style="text-decoration: underline;"),
                                tags$p(),
                                uiOutput("NetworkLegend2",inline = T, style="width-max: 50%;"),
                                uiOutput("NetworkLegendDpdn2")
                            ),
                            # withLoader(
                                 visNetworkOutput("network", width = "100%"),
                            #     type="html",
                            #     loader="loader6"),
                            fluidRow(#style = "background-color: #4d3a7d;",
                                  column(
                                    width = 6,
                                    uiOutput("NetworkLegend",inline = T, style="width-max: 50%;")),
                                  column(
                                    width = 6,
                                    dropMenu(
                                      tag=actionButton("showEdgeLegend_btn", "Edge Legend", style="margin: 6px;", class="btn-grBG"),# 
                                      theme="translucent",
                                      placement = "top",
                                      uiOutput("NetworkLegendDpdn")
                                )
                            )) 
                        ),
                        
                        boxPlus(
                            title = "Hierarchical Plot of Species",
                            width = 12,
                            status = "info",
                            solidHeader = FALSE,
                            collapsible = T,
                            closable = FALSE,
                            collapsed = TRUE,
                            enable_dropdown = F,
                            enable_label = T,
                            label_text = 
                               tags$div(style="display: inline", HTML(as.character(
                                    '<div id="plot_rdBtn" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline shiny-bound-input"  style="margin-bottom: 0px; margin-top: 0px; display: inline;>
                                        <label class="control-label shiny-label-null" for="plot_rdBtn"></label>
                                        <div class="shiny-options-group" data-toggle="buttons" style="display: inline;">
                                            <label id="tree_btn_1" class="btn btn-secondary btn-dark active" margin-left="2px" margin-right="3px">
                                                <input type="radio" name="plot_rdBtn" value="treemap" checked="checked">
                                                <i class="fa fa-tree"></i>
                                            </label>
                                            <label id="tree_btn_2" class="btn btn-secondary btn-dark" margin-left="2px" margin-right="3px">
                                                <input type="radio" name="plot_rdBtn" value="TreeMap">
                                                <i class="fa fa-tree"></i>
                                            </label>
                                            <label id="sunburst_btn" class="btn btn-secondary btn-dark" margin-left="2px" margin-right="3px">
                                                <input type="radio" name="plot_rdBtn" value="SunburstPolt">
                                                <i class="fa fa-sun"></i>
                                            </label>
                                            <label id="circle_btn" class="btn btn-secondary btn-dark" margin-left="2px" margin-right="3px">
                                                <input type="radio" name="plot_rdBtn" value="CirclePlot">
                                                <i class="fa fa-tint"></i>
                                            </label>
                                        </div>
                                    </div>'
                               ))),
                            conditionalPanel(
                                condition = "input.plot_rdBtn=='treemap'",
                                # withLoader(
                                    d3tree2Output(outputId = "treemap_Carabidae"),
                                #     type="html",
                                #     loader="loader6"),
                                # withLoader(
                                     d3tree2Output(outputId = "treemap_Fungus"),
                                #     type="html",
                                #     loader="loader6")
                            ),
                            conditionalPanel(
                                condition = "input.plot_rdBtn=='TreeMap'",
                                loadEChartsLibrary(),
                                tags$div(id="TreeMap", style="width:100%; height:200%;"),
                                deliverChart(div_id = "TreeMap")
                            ),
                            conditionalPanel(
                                condition = "input.plot_rdBtn=='SunburstPolt'",
                                # withLoader(
                                     plotlyOutput(outputId="SunburstPolt"),
                                #     type="html",
                                #     loader="loader6")
                                ),
                            conditionalPanel(
                                condition = "input.plot_rdBtn=='CirclePlot'",
                                circlepackeROutput(outputId="CirclePlot")
                            )
                        ),
                        
                        boxPlus(
                          title = "Google Treemap",
                          width = 12,
                          status = "info",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          closable = FALSE,
                          collapsed = TRUE,
                          htmlOutput("googleTreemap")
                        )
                    ),
    # ----- right column -----
                    column(
                        width = 6, style='padding:0px;',
                        
                        box(
                            width = 12,
                            title = "Global Distribution", 
                            closable = FALSE,
                            collapsible = TRUE,
                            status = "info", 
                            solidHeader = F, 
                            # enable_sidebar = FALSE,
                            fluidRow(
                                column( # this column() is important. It ensures that the map stays within the limits of the parent box
                                    width = 12,
                                    # withLoader(
                                         leafletOutput(outputId="MapPlot")
                                    #     type="image",
                                    #     loader='Loading-Louse_v5.gif'
                                    # )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    boxPad(
                                        color = "black",
                                        style = "box-sizing: border-box",
                                        dropMenu(
                                            tag=actionButton("locateCountry_btn", "Locate Country", width="100%", class="btn-dark"),
                                            theme="translucent",
                                            radioButtons(
                                                inputId="CountryList_RdBtn", 
                                                label=NULL, 
                                                choiceNames=c("World Map","Data Set"), 
                                                choiceValues=c("CountriesSHP","CountriesDB"), 
                                                selected="CountriesDB", 
                                                inline=T),
                                            selectInput(
                                              inputId="Country_drdn",
                                              label=NULL,
                                              choices=c("<Select Country>"="-", nameSet$CountriesDB),
                                              selectize=T)
                                        )
                                    )
                                )
                            )
                        ),
                        
                        boxPlus(
                          title = "Distribution of Publications Over Time ...",
                          width = 12,
                          status = "info",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          closable = FALSE,
                          collapsed = TRUE,
                          fluidRow(
                            column(width=6,
                                   htmlOutput("HistogramDataSet")),
                            column(width=6,
                                   htmlOutput("HistogramSelection") )
                          )
                        )
                    )
                ),
                
    tags$hr(style="border-color: #b2c2af; border-width: 2px; margin-top:5px"),
                fluidRow(
                    box(
                        title = "Data Set",
                        width = 12,
                        status = "info",
                        solidHeader = FALSE,
                        collapsible = TRUE,
                        dataTableOutput(outputId="CarabidaeTabFront")
                    )   
                ),
verbatimTextOutput("coordOut"), # debugging output option
tags$div(id="networkLegendTest") # to test dump inside the setting of the network
            ),
# ----- Second tab content -----
            tabItem(tabName = "tableTab",
                    titlePanel("Filtered Data"),
                    dataTableOutput(outputId='CarabidaeTable'),
                    tags$p(),
                    downloadButton(outputId="exportTabCSV", " Export Table (.csv)"),
                    downloadButton(outputId="exportTabXLSX", " Export Table (.xlsx)")
            ),
# ----- Third tab content -----
            tabItem(tabName = "accTab",
                    accordion(
                        inputId = "AccMain",
                        accordionItem(
                            id = "AccItem_1",
                            title = "Project Objective",
                            color = "info",
                            collapsed = TRUE,
                            "Nam dui ligula, fringilla a, euismod sodales, sollicitudin vel, wisi. Morbi auctor lorem non justo. Nam lacus libero, pretium at, lobortis vitae, ultricies et, tellus. Donec aliquet, tortor sed accumsan bibendum, erat ligula aliquet magna, vitae ornare odio metus a mi. Morbi ac orci et nisl hendrerit mollis. Suspendisse ut massa. Cras nec ante. Pellentesque a nulla. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam tincidunt urna. Nulla ullamcorper vestibulum turpis. Pellentesque cursus luctus mauris."
                        ),
                        accordionItem(
                            id = "AccItem_2",
                            title = "Data description",
                            color = "info",
                            collapsed = TRUE,
                            "Nulla malesuada porttitor diam. Donec felis erat, congue non, volutpat at, tincidunt tristique, libero. Vivamus viverra fermentum felis. Donec nonummy pellentesque ante. Phasellus adipiscing semper elit. Proin fermentum massa ac quam. Sed diam turpis, mo- lestie vitae, placerat a, molestie nec, leo. Maecenas lacinia. Nam ipsum ligula, eleifend at, accumsan nec, suscipit a, ipsum. Morbi blandit ligula feugiat magna. Nunc eleifend conse- quat lorem. Sed lacinia nulla vitae enim. Pellentesque tincidunt purus vel magna. Integer non enim. Praesent euismod nunc eu purus. Donec bibendum quam in tellus. Nullam cursus pulvinar lectus. Donec et mi. Nam vulputate metus eu enim. Vestibulum pellentesque felis eu massa."
                        ),
                        accordionItem(
                            id = "AccItem_3",
                            title = "Bibliography",
                            color = "info",
                            collapsed = TRUE,
                            "Quisque ullamcorper placerat ipsum. Cras nibh. Morbi vel justo vitae lacus tincidunt ultrices. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. In hac habitasse platea dictumst. Integer tempus convallis augue. Etiam facilisis. Nunc elementum fermentum wisi. Aenean placerat. Ut imperdiet, enim sed gravida sollicitudin, felis odio placerat quam, ac pulvinar elit purus eget enim. Nunc vitae tortor. Proin tempus nibh sit amet nisl. Vi- vamus quis tortor vitae risus porta vehicula."
                        ),
                        accordionItem(
                            id = "AccItem_4",
                            title = "How to cite",
                            color = "info",
                            collapsed = TRUE,
                            HTML("Garfield, Arbuckle, Jon, Wilson Liz, Nermal, Odie (2020) The Art of Eating. <i> The Food Journal</i> 7(2): 313-315<br>doi: 10.11110/kdlsfii.ksof-889</br>")
                        )
                    ),
                    accordion(
                        inputId = "AccFunding",
                        accordionItem(
                            id = "AccItem_5",
                            title = "Funding",
                            color = "teal",
                            collapsed = FALSE,
                            HTML("<p>This work was financed by food stamp collections.</p>")
                        )
                    ),
                    accordion(
                        inputId = "AccDisclamer",
                        accordionItem(
                            id = "AccItem_6",
                            title = "Disclaimer",
                            color = "teal",
                            collapsed = FALSE,
                            HTML("<p>Made with <a href='https://www.naturalearthdata.com/'  target='_blank' rel='noopener noreferrer'>Natural Earth</a>. Free vector and raster map data.</p><img src='NEV-Logo-Black.png' class='natearth' />")
                        )
                    )
            ),
# ----- Forth tab content -----
            tabItem("aboutTab",
                    titlePanel("A Message from Us to You"),
                    shiny::includeMarkdown(readme_file)
            ),
# ----- Fifth tab content -----
            tabItem(tabName = "radNet",
                    # withLoader(
                       diagonalNetworkOutput("radial", height = "3000px")
                    #   type="image",
                    #   loader='Loading-Louse_v5.gif'
                    # )

                   
                    # radialNetworkOutput("radial", height = "1000px")
            )
        )
    )
)


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
    
    # open about-tab button 4
    # -----------------------------------------
    observeEvent(input$about_tab_btn_pl, {
        updateTabItems(session, inputId = "tabs", selected = "aboutTab")
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
            dataFilter() %>%
                select(Carabidae_link, Fungus_link, Inter_type, Citation, Country),
            escape=FALSE,
            selection='none',
            rownames=F,
            colnames=c('Carabidae Names'='Carabidae_link',
                       'Fungus Names'='Fungus_link',
                       'Interaction Type'='Inter_type'),
            extensions = 'Scroller',
            options = list(
                deferRender = TRUE,
                scroller = F
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
        
        Carabidae_network <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "name")
        
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
    
    # # (!!!! if we use this function to avoid re-rendering, we need to manually click on nodes to select and deselcet them in order to visualize the colour change ...)
    # # update Edge colours 
    # # -------------------
    # observe({
    #   shiny::validate(need(input$EdgeCol, message=FALSE), need(input$HighlightCol_Edges, message=FALSE))
    #   visNetworkProxy("network") %>%
    #       visEdges(
    #         color = list(color = input$EdgeCol,
    #                      highlight = input$HighlightCol_Edges))
    # })
    
    
    output$coordOut <- renderPrint({
      return(dataFilter())
    })
    
    # observeEvent(input$EdgeCol,
    #              # update the input object
    #              visNetworkProxy("network") %>%
    #                visGetPositions()
    # )
    # observe({
    #   print(input$network_positions)
    #   
    #   #visNetworkProxy("network") %>%
    #       # visEdges(
    #       #   value = EdgeTab$data$value +10,
    #       #   color = list(color = input$EdgeCol,
    #       #                highlight = input$HighlightCol_Edges)) %>%
    #     # testing if this visGroup function can trigger also edge redraws ... doesn't look like it
    #     # visGroups(
    #     #   groupname = "Fungi", 
    #     #   shape = "star")
    #   
    # })
    
    
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
    
    # ===================== #
    #   google Treemap      #
    # ===================== #
    
    output$googleTreemap <- renderGvis({
      
      tableSelection <- dataFilter()
      # ReactVal$CountryClickSelection has the value '*' as default
      tableSelection <- tableSelection[grep(ReactVal$CountryClickSelection, tableSelection$Country),]
      
      if (nrow(tableSelection)>0) {
          tableSelection <-data.table(GBIFID=c(tableSelection$GBIFID_Carabidae, tableSelection$GBIFID_Fungus))[GBIFID != -1]
          setkey(tableSelection, GBIFID)
          setkey(tax_tab, GBIFID)
          tableSelection <- tableSelection[tax_tab, nomatch=0][,c("kingdom","phylum","order","family","genus","species")]
          
          # Replacing NA with the parent allows to remove label/parent pairs without disrupting the branch. (Species names which are NA can be dropped immediately later.)
          tableSelection[, genus := ifelse(is.na(genus), family, genus)]
          
          tableSelection_Fungi <- tableSelection[kingdom == "Fungi"]
          tableSelection_Fungi[, phylum := ifelse(is.na(phylum), kingdom, phylum)] # those changes need to stay sequential
          tableSelection_Fungi[, order  := ifelse(is.na(order), phylum, order)]
          tableSelection_Fungi[, family := ifelse(is.na(family), order, family)]
          
          tableSelection_Carabidae <- tableSelection[kingdom == "Animalia"]
          tableSelection_Carabidae[, family := ifelse(is.na(family), kingdom, family)]
          
          tableSelection <- data.table::rbindlist(
            l = list(tableSelection[,.(label = kingdom, parent="Carabidae - Fungus interactions")], # adding a common root node (which we use here with the idea of a title)
                     tableSelection_Fungi[,.(label=phylum, parent=kingdom)],
                     tableSelection_Fungi[,.(label=order, parent=phylum)],
                     tableSelection_Fungi[,.(label=family, parent=order)],
                     tableSelection_Fungi[,.(label=genus, parent=family)],
                     tableSelection_Carabidae[,.(label=family, parent=kingdom)],
                     tableSelection_Carabidae[,.(label=genus, parent=family)],
                     tableSelection[!is.na(species),][,.(label=species, parent=genus)]),
            use.names = F
          )
          tableSelection <- tableSelection[label!=parent,] # first filter, then counting (!!!) to get the right numbers
          tableSelection <- unique(tableSelection[, ':='(value = .N, ids = label), by = label])
          
      tableSelection <- data.table::rbindlist(
          l=list(data.table(
                    label="Carabidae - Fungus interactions", 
                    parent=NA, 
                    value=sum(tableSelection[parent=="Carabidae - Fungus interactions","value"]),
                    ids="Carabidae - Fungus interactions"),
                 tableSelection),
          use.names = F)
      tableSelection[, size := log10(value)]
      
      # transform counts of fungi to negative space (that might give the kingdoms different base colour on a 2-colour gradient)
      tableSelection[, value := ifelse(label %in% unique(unlist(tableSelection_Fungi)),-1*value,value)]
#      tableSelection[, value := value - min(value)] # work only with positive values: does gvisTreeMap() likes this better? 
        
      # tableSelection
      #                            label                      parent value                         ids      size
      #   1: Carabidae - Fungus interactions                        <NA> 14894 Carabidae - Fungus interactions 3.9970367
      #   2:                       Fungi Carabidae - Fungus interactions     0                       Fungi 3.6956568
      #   3:                    Animalia Carabidae - Fungus interactions  9932                    Animalia 3.6963564
      #   4:                  Zygomycota                       Fungi   686                  Zygomycota 3.6310377
      #   5:               Basidiomycota                       Fungi  4955               Basidiomycota 0.8450980
      # ---                                                                                                    
      # 661:          Euceraphis betulae                  Euceraphis  4974          Euceraphis betulae 1.0791812
      # 662:      Rhopalomyzus lonicerae                Rhopalomyzus  4968      Rhopalomyzus lonicerae 0.7781513
      # 663:          Sitobion miscanthi                    Sitobion  4970          Sitobion miscanthi 0.9030900
      # 664:           Cinara tujafilina                      Cinara  4965           Cinara tujafilina 0.4771213
      # 665:                Myzus cerasi                       Myzus  5001                Myzus cerasi 1.5910646
      

        gvisOpt_Tree <- list(
          headerColor = '#988f86',
          highlightOnMouseOver = 'true',
          maxDepth = 1,
          maxPostDepth = 1,
          minHighlightColor = '#8c6bb1',
          midHighlightColor = '#9ebcda',
          maxHighlightColor = '#edf8fb',
          minColor = '#009688',
          midColor = '#f7f7f7',
          maxColor = '#ee8100',
          headerHeight = 20,
          showScale = 'true',
          height = '500',
          useWeightedAverageForAggregation = 'true',
          width = 'auto',
          backgroundColor='transparent')
          
      
        gTreemap <- gvisTreeMap(
          tableSelection,
          idvar = "label",
          parentvar = "parent",
          sizevar = "size",
          colorvar = "value",
          options = gvisOpt_Tree,
          chartid ="gTreemap"
        )
        return(gTreemap)
      }
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
    
    # ================ #
    #   Circle Plot    #
    # ================ #
    
    # # Circular plot in World-tab
    output$CirclePlot <- renderCirclepackeR({
        tableSelection <- dataFilter()

        # ReactVal$CountryClickSelection has the value '*' as default
        tableSelection <- tableSelection[grep(pattern=ReactVal$CountryClickSelection, x=tableSelection$Country),]

        if (nrow(tableSelection)>0) {
            # get all GBIF-IDs of species -> remove undetermined once (-1) -> count > add count to taxonomy while keeping only present species
          
            CirclePlot_tbl <- data.table(GBIFID=c(tableSelection$GBIFID_Carabidae, tableSelection$GBIFID_Fungus))
            CirclePlot_tbl <- CirclePlot_tbl[GBIFID != -1][, .(value = .N) , by = GBIFID]
            setkey(CirclePlot_tbl, GBIFID)
            setkey(tax_tab, GBIFID)
            CirclePlot_tbl <- CirclePlot_tbl[tax_tab, nomatch=0]
            
            CirclePlot_tbl[, pathString := ifelse(kingdom=="Animalia",
                                                  paste("cellular Organism", kingdom,                family, genus, species, sep = "/"),
                                                  paste("cellular Organism", kingdom, phylum, order, family, genus, species, sep = "/"))]
            
            CirclePlot_tbl[, pathString := gsub("/NA","",pathString, fixed = T)]

            circPlot <- circlepackeR(data.tree::as.Node(CirclePlot_tbl[,c("pathString", "value")]), 
                                     size = "value", 
                                     color_min = "#d9f5be", 
                                     color_max = "#3d4b54")

            return(circPlot)
        }else{
            return(NULL)
        }
    })

    
    # ================ #
    #   Tree maps      #
    # ================ #
    
    # ... with purple background
    output$treemap_Fungus <- renderD3tree2({
        tableSelection <- dataFilter()

        # ReactVal$CountryClickSelection has the value '*' as default
        tableSelection <- tableSelection[grep(pattern=ReactVal$CountryClickSelection,
                                              x=tableSelection$Country),]

        if (nrow(tableSelection)>0) {

            TrMap_tbl_Fungus <- data.table(GBIFID = tableSelection$GBIFID_Fungus)
            TrMap_tbl_Fungus <- TrMap_tbl_Fungus[GBIFID != -1][, .(value = .N) , by = GBIFID]
            setkey(TrMap_tbl_Fungus, GBIFID)
            setkey(tax_tab, GBIFID)
            TrMap_tbl_Fungus <- TrMap_tbl_Fungus[tax_tab, nomatch=0]
            TrMap_tbl_Fungus <- TrMap_tbl_Fungus[,c("value", "phylum", "order", "family", "genus", "species")]
            TrMap_tbl_Fungus <- unique(TrMap_tbl_Fungus)

            ## remove phylogenetic levels in case they are unique in current table
            # TrMap_tbl_Fungus <- TrMap_tbl_Fungus %>%
            #   select(which(summarize_all(TrMap_tbl_Fungus, n_distinct, na.rm=T)>1))

            tm_Fungus <- treemap(
              TrMap_tbl_Fungus,
                index=colnames(TrMap_tbl_Fungus)[2:6],
                vSize="value",
                vColor="value",
                type="value",
                palette = "Blues",
                title ="Number Fungi in current selection",
                border.col=c("black","white"),
                border.lwds=c(3, 6:1),
                fontcolor.labels="#ebebeb",
                align.labels=c("right","top"),
                fontsize.labels = 16,
                title.legend="",
                position.legend="none"
            )
            tm_Fungus <- d3tree2(tm_Fungus,  rootname = "Fungi" )
            return(tm_Fungus)
        }else{
            return(NULL)
        }
    })
 
    # # ... with blue background
    output$treemap_Carabidae <- renderD3tree2({
      tableSelection <- dataFilter()

      # ReactVal$CountryClickSelection has the value '*' as default
      tableSelection <- tableSelection[grep(pattern=ReactVal$CountryClickSelection,
                                            x=tableSelection$Country),]

      if (nrow(tableSelection)>0) {
        
        TrMap_tbl_Apid <- data.table(GBIFID = tableSelection$GBIFID_Carabidae)
        TrMap_tbl_Apid <- TrMap_tbl_Apid[GBIFID != -1][, .(value = .N) , by = GBIFID]
        setkey(TrMap_tbl_Apid, GBIFID)
        setkey(tax_tab, GBIFID)
        TrMap_tbl_Apid <- TrMap_tbl_Apid[tax_tab, nomatch=0]
        TrMap_tbl_Apid <- TrMap_tbl_Apid[,c("value", "phylum", "class", "order", "family", "genus", "species")]
        TrMap_tbl_Apid <- unique(TrMap_tbl_Apid)

        tm_Carabidae <- treemap(
          TrMap_tbl_Apid,
          index=colnames(TrMap_tbl_Apid)[2:7],
          vSize="value",
          vColor="value",
          type="value",
          palette = "Purples",
          title ="Number of Carabidae in current selection",
          border.col=c("black","white"),
          border.lwds=c(3, 6:1),
          fontcolor.labels="#ebebeb",
          align.labels=c("right","top"),
          fontsize.labels = 16,
          title.legend="",
          position.legend="none"
        )
        tm_Carabidae <- d3tree2(tm_Carabidae,  rootname = "Animalia" )
        return(tm_Carabidae)
      }else{
        return(NULL)
      }
    })

    # ... with red background
    renderTreeMap(div_id = "TreeMap", leafDepth=1, theme="default", data = makeTreemapInput(recordSelection=Carabidae_data))
    
    # radial network
    output$radial <- renderDiagonalNetwork({ # renderRadialNetwork({
      # ReactVal$CountryClickSelection has the value '*' as default
      # we don't need to worry about missing GBIF IDs since we start to work with the taxonomy table (we don't need to count occurrences)
      tableSelection <- tax_tab[tax_tab$GBIFID %in% dataFilter()[grep(pattern=ReactVal$CountryClickSelection, x=Country), c(GBIFID_Carabidae, GBIFID_Fungus)]]
      
      if (nrow(tableSelection)>0) {
          tableSelection[, pathString := ifelse(kingdom=="Animalia",
                                                paste("cellular Organism", kingdom,                family, genus, species, sep = "/"),
                                                paste("cellular Organism", kingdom, phylum, order, family, genus, species, sep = "/"))]
          tableSelection[, pathString := gsub("/NA","", pathString, fixed=T)]
          
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
