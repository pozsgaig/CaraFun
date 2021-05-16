# ============================================= #
# functions used by server                      #
# ============================================= #

# DT_child_row <- function(DT) {
#     DT <- copy(DT)
#     DT[, ':='(Inter_type = as.factor(Inter_type),
#               Country = Country,
#               Carabidae_name = as.factor(Carabidae_name),
#               Fungus_name = as.factor(Fungus_name),
#               GBIF="<img src='GBIF-2015-mark.png' alt='to GBIF.org' style='width:2.5ex;height:2.5ex;'/>")]
#     DT <- DT[, c("GBIF", "Carabidae_link", "Carabidae_name", "Fungus_link", "Fungus_name", "Inter_type", "Citation", "Country", "Year")]
#     
#     dt <- datatable(
#         DT,
#         escape=F, # c("Citation", "Fungus_link", "Carabidae_link", "GBIF")
#         selection='single',
#         rownames=F,
#         filter = 'top',
#         colnames=c(#'Carabidae Names'='Carabidae_link', # (!) 'new name' = 'old name'
#                    'Fungus Names'='Fungus_link',
#                    'Interaction Type'='Inter_type'),
#         extensions = 'Scroller',
#         options = list(
#             search = list(regex=T, caseInsensitive = FALSE),
#             deferRender = TRUE,
#             scrollY = 545,
#             scrollX = FALSE,
#             scroller = TRUE,
#             pageLength = 15,
#             columnDefs = list( list(visible = FALSE, targets = c(1, 3)),
#                                list(orderable = FALSE, className = 'details-control', targets = 0))),
#         callback = JS(
#             "table.column(1).nodes().to$().css({cursor: 'pointer'});",
#             "var format = function(d) {",
#             "return '<pre>GBIF: ' + d[1] + '</pre><pre>GBIF: ' + d[3] +'</pre>';",
#             "};",
#             "table.on('click', 'td.details-control', function() {",
#             "var td = $(this), row = table.row(td.closest('tr'));",
#             "if (row.child.isShown()) {",
#             "row.child.hide();",
#             "td.html(\"<img src='GBIF-2015-mark.png' alt='GBIF.org' style='width:2.5ex;height:2.5ex;'/>\");",
#             "} else {",
#             "row.child(format(row.data())).show();",
#             "td.html(\"<img src='GBIF-2015-mark_gray.png' alt='GBIF.org' style='width:2.5ex;height:2.5ex;'/>\");",
#             "}",
#             "});")
#     ) %>%
#     DT::formatStyle(columns=seq(1,ncol(DT)), color = 'black') %>%
#     DT::formatStyle(columns=c("Carabidae_name","Fungus_name"), fontStyle='italic') %>%
#     DT::formatStyle(columns="GBIF", textAlign='center', width='5px')
#     
#     return(dt)
# }

# DT_GBIF_button <- function(DT) {
#     DT <- copy(DT)
#     DT[, ':='(Inter_type = as.factor(Inter_type),
#               Country = Country,
#               Carabidae_name = as.factor(Carabidae_name),
#               Fungus_name = as.factor(Fungus_name),
#               GBIF_a = sub(pattern='<i>.*</i>','<img src="GBIF-2015-mark.png" alt="www.GBIF.org" style="width:2.5ex;height:2.5ex;">', x=Carabidae_link),
#               GBIF_f = sub(pattern='<i>.*</i>','<img src="GBIF-2015-mark.png" alt="www.GBIF.org" style="width:2.5ex;height:2.5ex;">', x=Fungus_link)
#     )]
#     DT <- DT[, c("GBIF_a", "Carabidae_name", "GBIF_f", "Fungus_name", "Inter_type", "Citation", "Country", "Year")]
#     
#     DT[, .(GBIF_a, Carabidae_name, GBIF_f, Fungus_name, Inter_type, Citation, Country, Year)]
#     
#     GBIF_cols <- which(colnames(DT) %in% c('GBIF_a','GBIF_f'))
#     
#     dt <- datatable(
#         DT,
#         escape= c(2,4,7),
#         selection='single',
#         rownames=F,
#         filter = 'top',
#         colnames=c(' '='GBIF_a',' '='GBIF_f',
#                    'Carabidae Names'='Carabidae_name', # (!) 'new name' = 'old name'
#                    'Fungus Names'='Fungus_name',
#                    'Interaction Type'='Inter_type'),
#         extensions = 'Scroller',
#         options = list(
#             search = list(regex=T, caseInsensitive = FALSE),
#             columnDefs = list(list(targets = c(0,2), searchable = FALSE, orderable = FALSE)),
#             deferRender = TRUE,
#             scrollY = 545,
#             scrollX = FALSE,
#             scroller = TRUE,
#             pageLength = 15)) %>%
#         DT::formatStyle(columns=seq(1,ncol(DT)), color = 'black') %>%
#         DT::formatStyle(columns=c("Carabidae Names","Fungus Names"), fontStyle='italic') %>%
#         DT::formatStyle(columns=GBIF_cols, textAlign='center', width='5px')
#     
#     return(dt)
# }

DT_direct_links <- function(DT) {
  DT <- DT[, c("Carabidae_link", "Fungus_link", "Inter_type", "Citation", "Country", "Year")]
  DT[, ':='(Inter_type = as.factor(Inter_type),
            Country = Country
           )]

  dt <- datatable(
    DT,
    escape=F, # c("Citation", "Fungus_link", "Carabidae_link", "GBIF")
    selection='single',
    rownames=F,
    filter = 'top',
    colnames=c('Carabidae Names'='Carabidae_link', # (!) 'new name' = 'old name'
               'Fungus Names'='Fungus_link',
               'Interaction Type'='Inter_type'),
    extensions = 'Scroller',
    options = list(
      search = list(regex=T, caseInsensitive = FALSE),
      deferRender = TRUE,
      scrollY = 545,
      scrollX = FALSE,
      scroller = TRUE,
      pageLength = 15)) %>%
    DT::formatStyle(columns=seq(1,ncol(DT)), color = 'black')

  return(dt)
}


# # =============================================== #
# # function for skipping nodes with only one child #
# # =============================================== #
# 
# prunePhylogeny <- function(table, term) {
#   child <- table[table$parent==term,]
#   if (nrow(child)==0) {
#     return(table)
#   }else{
#     if (nrow(child)==1) {
#       if (nrow(table[table$parent==child$label,])>0) {
#         table <- table[table$parent!=term,]
#         table[table$parent==child$label,"parent"] <- term
#         table <- prunePhylogeny(table=table, term=term)
#       }
#     }else{
#       for(term in child$label) {
#         table <- prunePhylogeny(table=table, term=term)
#       }
#     }
#     return(table)
#   }
# }

# ============================================= #
# plotting function for the tree map (red)      #
# ============================================= #

# ECharts2Shiny package (the red one which are manually assigned to a div)
makeTreemapInput <- function(recordSelection){
    #GBIF-ID of -1 stands for x, (= undetermined species) and is therefore filtered
    
    treemap_tbl <- data.table(GBIFID=c(recordSelection$GBIFID_Carabidae, recordSelection$GBIFID_Fungus))
    treemap_tbl <- treemap_tbl[GBIFID != -1][, .(value = .N) , by = GBIFID]
    
    setkey(treemap_tbl, GBIFID)
    setkey(tax_tab, GBIFID)
    treemap_tbl <- treemap_tbl[tax_tab, nomatch=0]
    treemap_tbl <- treemap_tbl[,c("value","kingdom", "phylum", "class", "order", "family", "genus", "species")]
    treemap_tbl <- unique(treemap_tbl)
    
    #  branching_count <- treemap_tbl%>% select(-value) %>% group_by(kingdom) %>% summarise_all(n_distinct)
    # A tibble: 2 x 7
    # kingdom  phylum class order family genus species
    #   <chr>     <int> <int> <int>  <int> <int>   <int>
    # 1 Animalia      1     1     2      6   113     315
    # 2 Fungi         4    11    24     37    60     109
    
    treemap_json <- d3_nest(treemap_tbl, root="Animalia/Fungi", value_cols=c("value"))
    
    # to meet the expected format: ... JSON-like
    txtMod_lkup <- data.frame(
        find=c(paste0('\\"',c("name","children","colname","value"),'\\"'),'\\\"'),
        replace=c("name","children","colname","value","\\\'"), stringsAsFactors=F)
    
    for (i in seq(1,nrow(txtMod_lkup))){
        treemap_json <- gsub(pattern=txtMod_lkup[i,"find"], replacement=txtMod_lkup[i,"replace"], x=treemap_json)
    }
    
    return(paste0("[",treemap_json,"]"))
}

# tmp <- treemap_tbl %>% filter(kingdom=="Fungi")
# tmp %>% select(-value) %>% select_if(.,summarise_all(.,.funs=n_distinct)>2) %>% apply(MARGIN=1,FUN=paste0, collapse="/")
# 
# treemap_tbl %>% 
#     select(-value) %>% 
#     group_by(kingdom) %>% 
#     select_if(.,summarise_all(.,.funs=n_distinct)>2) %>% apply(MARGIN=1,FUN=paste0, collapse="/") %>% gsub(pattern="/NA",replacement="")


# ============================================= #
# plotting function for leaflet                 #
# ============================================= #

# preparations for getting ready to plot the leaflet map
getMapDataObject <- function(shp, Carabidae_selection){
    # shp               shape file
    # Carabidae_selection   tibble holding the Carabidae-fungus records: column names: "GBIFID_Carabidae", "GBIFID_Fungus", "Carabidae_name", "Fungus_name", "Inter_type", "References", "Country", "Year"
    
    if (nrow(Carabidae_selection) > 0) {
        # appending the shape table with the country count of the current selection
        Carabidae_count <- Carabidae_selection[,.(Freq=.N), by="Country"]
        shp$Freq <- Carabidae_count[fastmatch::fmatch(shp$NAME, Carabidae_count[,Country]),Freq]
        
        # adding count of subregions    
        Freq_subregion <- tapply(shp$Freq, shp$SUBREGION, sum, na.rm=T)
        Freq_subregion <- Freq_subregion[names(Freq_subregion) != "900"]
        Freq_subregion[Freq_subregion == 0] <- NA
        shp$Freq[fastmatch::fmatch(names(Freq_subregion), shp$SubRegSum)] <- Freq_subregion
        
        rm(Freq_subregion, Carabidae_count)
        
        countryIDs <- shp$SUBREGION!=900
        regionIDs <- !countryIDs
        
        pal_country <- colorBin(brewer.pal(9,"Purples")[-1], domain = shp$Freq[countryIDs], bins = 7, na.color = "#AAAAAA")
        tooltip_labels_country <- paste0("<b>Region:</b> ", shp$NAME[countryIDs],"<br/><b>No of reports:</b> ", shp$Freq[countryIDs])
        tooltip_labels_country <- lapply(tooltip_labels_country, htmltools::HTML)
        
        pal_regions <- colorBin(brewer.pal(9,"YlOrBr")[-1], domain = shp$Freq[regionIDs], bins = 7, na.color = "#DDDDDD")
        tooltip_labels_region <- paste0("<b>World region:</b> ", shp$NAME[regionIDs],"<br/><b>No of reports:</b> ", shp$Freq[regionIDs])
        tooltip_labels_region <- lapply(tooltip_labels_region, htmltools::HTML)
        
        return(list(shp=shp,
                    countryIDs=countryIDs,
                    regionIDs=regionIDs,
                    pal_country=pal_country,
                    pal_regions=pal_regions, 
                    tooltip_labels_country=tooltip_labels_country,
                    tooltip_labels_region=tooltip_labels_region))
    }else{
        return(NULL)
    }
}

# get map w/o polygon layer
getPlainMap <- function(mapView, activeLayer){
    # this function adds a layer control for future layers. W/o layer control, no layer is initially ticked after adding them per update function.
    worldMap <- leaflet() %>%
        setView(lng =  mapView$lng, lat = mapView$lat, zoom = mapView$zoom) %>%
        addMapPane(name = "polygons", zIndex = 410) %>%
        addMapPane(name = "maplabels", zIndex = 420) %>%
        setMapWidgetStyle(list(background= "#0a3b59")) %>% # #0a3b59 darkblue; "transparent"
        # addProviderTiles("GeoportailFrance.orthos",
        #                  options = providerTileOptions(minZoom = 0, maxZoom = 12)) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels",
                         options = leafletOptions(pane = "maplabels"), group = "map labels") %>%
        addEasyButton(easyButton(id="zoom_btn",
                                 icon="fa-globe",
                                 title="Zoom to Level 1",
                                 onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
        addLayersControl(overlayGroups = c("country resolution", "world region resolution"),
                         options = layersControlOptions( collapsed = T ),
                         position = "topright")
    if (activeLayer=="world region resolution"){
        worldMap <- worldMap %>%
            hideGroup("country resolution") #%>%
        #removeControl(layerId = "countryResolutionLegend") # remove from view not the object
    }else{
        worldMap <- worldMap %>% 
            hideGroup("world region resolution")# %>%
        # removeControl(layerId = "worldRegionResolutionLegend")
    }
    return(worldMap)
}

# add/update polygon layer to map
updatePolygons <- function(MDO, worldMap, activeLayer=activeLayer, mapView=initView) {
    
    worldMap <- worldMap %>%
        clearShapes() %>%
        clearControls() %>%
        removeLayersControl() %>%
        setView(lng =  mapView$lng, lat = mapView$lat, zoom = mapView$zoom) 
    
    if (!is.null(MDO)) {
        worldMap <- worldMap %>%
            addPolygons(data=MDO$shp[MDO$regionIDs,],
                        fillColor = ~MDO$pal_regions(Freq),
                        stroke=FALSE,
                        fillOpacity = 0.8,
                        color = "#BDBDC3",
                        weight = 1,
                        label = MDO$tooltip_labels_region,
                        layerId = MDO$shp$NAME[MDO$regionIDs],
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal;", padding = "3px 8px;"),
                            textsize = "13px;",
                            direction = "auto"),
                        highlight = highlightOptions(
                            fillColor="#000000",
                            fillOpacity = 0.7,
                            bringToFront = T),
                        options = leafletOptions(pane = "polygons"),
                        group = "world region resolution") %>%
            addPolygons(data=MDO$shp[MDO$countryIDs,],
                        fillColor = ~MDO$pal_country(Freq),
                        fillOpacity = 0.8,
                        stroke=T,
                        color = "#222D32",
                        weight = .3,
                        label = MDO$tooltip_labels_country,
                        layerId = MDO$shp$NAME[MDO$countryIDs],
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "13px",
                            direction = "auto"),
                        highlight = highlightOptions(
                            fillColor="#000000",
                            fillOpacity = 0.7,
                            bringToFront = T),
                        options = leafletOptions(pane = "polygons"),
                        group = "country resolution") %>%
            addLegend(data=MDO$shp[MDO$regionIDs,], 
                      pal=MDO$pal_regions, 
                      values=~Freq, 
                      opacity=0.9, 
                      title = "Reports", 
                      position = "bottomleft",
                      na.label = "no record",
                      layerId = "worldRegionResolutionLegend", 
                      group = "world region resolution") %>%
            addLegend(data=MDO$shp[MDO$countryIDs,], 
                      pal=MDO$pal_country,
                      values=~Freq,
                      opacity=0.9,
                      title = "Reports",
                      position = "bottomleft",
                      na.label = "no record",
                      layerId = "countryResolutionLegend",
                      group = "country resolution") %>%
            addLayersControl(overlayGroups = c("country resolution", "world region resolution"),
                             options = layersControlOptions( collapsed = T ),
                             position = "topright")
        
        if (activeLayer=="world region resolution"){
            worldMap <- worldMap %>%
                hideGroup("country resolution") %>%
                removeControl(layerId = "countryResolutionLegend") # remove from view not the object
        }else{
            worldMap <- worldMap %>% 
                hideGroup("world region resolution") %>%
                removeControl(layerId = "worldRegionResolutionLegend")
        }
    }
    return(worldMap)
}


# # renders entire map (= getPlainMap() + updatePolygons()) using the MDO object
# # this function is not in use anymore
# renderMap <- function(MDO, activeLayer, mapView){
#   
#   shp = MDO$shp
#   countryIDs = MDO$countryIDs
#   regionIDs = MDO$regionIDs
#   pal_country = MDO$pal_country
#   pal_regions = MDO$pal_regions 
#   tooltip_labels_country = MDO$tooltip_labels_country
#   tooltip_labels_region = MDO$tooltip_labels_region
#   
#   worldMap <- leaflet() %>%
#     setView(lng =  mapView$lng, lat = mapView$lat, zoom = mapView$zoom) %>%
#     addMapPane(name = "polygons", zIndex = 410) %>%
#     addMapPane(name = "maplabels", zIndex = 420) %>%
#     setMapWidgetStyle(list(background= "#0a3b59")) %>% # #0a3b59 darkblue; "transparent"
#     addProviderTiles("GeoportailFrance.orthos",
#                      options = providerTileOptions(minZoom = 0, maxZoom = 12)) %>%
#     addProviderTiles("CartoDB.PositronOnlyLabels",
#                      options = leafletOptions(pane = "maplabels"), group = "map labels") %>%
#     addEasyButton(easyButton(id="zoom_btn", 
#                              icon="fa-globe", 
#                              title="Zoom to Level 1",
#                              onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
#     addPolygons(data=shp[regionIDs,],
#                 fillColor = ~pal_regions(Freq),
#                 stroke=FALSE,
#                 fillOpacity = 0.8,
#                 color = "#BDBDC3",
#                 weight = 1,
#                 label = tooltip_labels_region,
#                 layerId = shp$NAME[regionIDs],
#                 labelOptions = labelOptions(
#                   style = list("font-weight" = "normal;", padding = "3px 8px;"),
#                   textsize = "13px;",
#                   direction = "auto"),
#                 highlight = highlightOptions(
#                   fillColor="#000000",
#                   fillOpacity = 0.7,
#                   bringToFront = T),
#                 options = leafletOptions(pane = "polygons"),
#                 group = "world region resolution") %>%
#     addPolygons(data=shp[countryIDs,],
#                 fillColor = ~pal_country(Freq),
#                 fillOpacity = 0.8,
#                 stroke=T,
#                 color = "#222D32",
#                 weight = .3,
#                 label = tooltip_labels_country,
#                 layerId = shp$NAME[countryIDs],
#                 labelOptions = labelOptions(
#                   style = list("font-weight" = "normal", padding = "3px 8px"),
#                   textsize = "13px",
#                   direction = "auto"),
#                 highlight = highlightOptions(
#                   fillColor="#000000",
#                   fillOpacity = 0.7,
#                   bringToFront = T),
#                 options = leafletOptions(pane = "polygons"),
#                 group = "country resolution") %>%
#     addLegend(data=shp[regionIDs,], 
#               pal=pal_regions, 
#               values=~Freq, 
#               opacity=0.9, 
#               title = "Reports", 
#               position = "bottomleft",
#               na.label = "no record",
#               layerId = "worldRegionResolutionLegend", 
#               group = "world region resolution") %>%
#     addLegend(data=shp[countryIDs,], 
#               pal=pal_country,
#               values=~Freq,
#               opacity=0.9,
#               title = "Reports",
#               position = "bottomleft",
#               na.label = "no record",
#               layerId = "countryResolutionLegend",
#               group = "country resolution") %>%
#     addLayersControl(overlayGroups = c("country resolution", "world region resolution"),
#                      options = layersControlOptions( collapsed = T ),
#                      position = "topright")
#   
#   if (activeLayer=="world region resolution"){
#     worldMap <- worldMap %>% hideGroup("country resolution")
#   }else{
#     worldMap <- worldMap %>% hideGroup("world region resolution")
#   }
#   
#   return(worldMap)
# }