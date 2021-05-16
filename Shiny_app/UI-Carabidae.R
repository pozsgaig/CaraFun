ui <- dashboardPagePlus(
    collapse_sidebar = FALSE,
    sidebar_fullCollapse = T,
    skin="green",
    
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    
    useShinyjs(),
    
    # ====   H E A D E R   ====================================================================================================================
    header = dashboardHeaderPlus(
        title = HTML('<i class="fa fa-bug" style = "color:#F39C12;" aria-hidden="true"></i> 
                      <b style = "color:#f1faf8">Carabidae - Fungi interactions</b>'),
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
                id = "radNet_tab_btn",
                class="btn btn-default action-button btn-green",
                img(src='tree-solid.svg', style='width:1em;height:1em;'),
                type="button",
                style="min-width: 40px;"
            ),
            
            bsTooltip( id = "main_tab_btn", title = "main" ),
            bsTooltip( id = "table_tab_btn", title = "table" ),
            bsTooltip( id = "about_tab_btn_acc", title = "about" ),
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
            menuItem("About", tabName = "accTab", icon = icon("readme")),
            HTML("<li><a href='#shiny-tab-radNet' data-toggle='tab' data-value='radNet'><img src='tree-solid.svg' style='width:1em;height:1em;'/><span>Tree</span></a></li>")
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
                    pickerInput(
                        inputId = "ShapeIpt_Carabidae",
                        label = "Carabidaes",
                        selected = "square",
                        choices = list('label outside'=shapeTab$returnVal[1:6], 'label inside'=shapeTab$returnVal[7:10]),
                        choicesOpt = list(content = shapeTab$img))),
                
                div(id="ShapeIpt_Fungus_div", style="display:inline-block;width:49%;text-align: center;",
                    pickerInput(
                        inputId = "ShapeIpt_Fungus",
                        label = "Fungi",
                        selected = "triangle",
                        choices = list('label outside'=shapeTab$returnVal[1:6], 'label inside'=shapeTab$returnVal[7:10]),
                        choicesOpt = list(content = shapeTab$img)))
            )
        )
    ),
    
    
    # ====   B O D Y   ========================================================================================================================
    body = dashboardBody(
        # we have 2 of those 'favicon' lines because different browser need a different setup ... well, do they need this at all or just a file?
        tags$head(tags$link(rel="icon", type="image/x-icon", href="favicon.ico")),
        tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

#         tags$script("imageZoom('myimage', 'myresult');"),
#         
#         tags$script(HTML("function imageZoom(imgID, resultID) {
#   var img, lens, result, cx, cy;
#   img = document.getElementById(imgID);
#   result = document.getElementById(resultID);
#   /*create lens:*/
#   lens = document.createElement('DIV');
#   lens.setAttribute('class', 'img-zoom-lens');
#   /*insert lens:*/
#   img.parentElement.insertBefore(lens, img);
#   /*calculate the ratio between result DIV and lens:*/
#   cx = result.offsetWidth / lens.offsetWidth;
#   cy = result.offsetHeight / lens.offsetHeight;
#   /*set background properties for the result DIV:*/
#   result.style.backgroundImage = 'url(\'' + img.src + '\')';
#   result.style.backgroundSize = (img.width * cx) + 'px ' + (img.height * cy) + 'px';
#   /*execute a function when someone moves the cursor over the image, or the lens:*/
#   lens.addEventListener('mousemove', moveLens);
#   img.addEventListener('mousemove', moveLens);
#   /*and also for touch screens:*/
#   lens.addEventListener('touchmove', moveLens);
#   img.addEventListener('touchmove', moveLens);
#   function moveLens(e) {
#     var pos, x, y;
#     /*prevent any other actions that may occur when moving over the image:*/
#     e.preventDefault();
#     /*get the cursor's x and y positions:*/
#     pos = getCursorPos(e);
#     /*calculate the position of the lens:*/
#     x = pos.x - (lens.offsetWidth / 2);
#     y = pos.y - (lens.offsetHeight / 2);
#     /*prevent the lens from being positioned outside the image:*/
#     if (x > img.width - lens.offsetWidth) {x = img.width - lens.offsetWidth;}
#     if (x < 0) {x = 0;}
#     if (y > img.height - lens.offsetHeight) {y = img.height - lens.offsetHeight;}
#     if (y < 0) {y = 0;}
#     /*set the position of the lens:*/
#     lens.style.left = x + 'px';
#     lens.style.top = y + 'px'';
#     /*display what the lens 'sees':*/
#     result.style.backgroundPosition = '-' + (x * cx) + 'px -' + (y * cy) + 'px';
#   }
#   function getCursorPos(e) {
#     var a, x = 0, y = 0;
#     e = e || window.event;
#     /*get the x and y positions of the image:*/
#     a = img.getBoundingClientRect();
#     /*calculate the cursor's x and y coordinates, relative to the image:*/
#     x = e.pageX - a.left;
#     y = e.pageY - a.top;
#     /*consider any page scrolling:*/
#     x = x - window.pageXOffset;
#     y = y - window.pageYOffset;
#     return {x : x, y : y};
#   }
# }")),
        
        # # error handling
        # tags$style(type = "text/css",
        #            ".shiny-output-error { visibility: hidden; }",
        #            ".shiny-output-error:before { visibility: visible;
        #                                          content: 'An error occurred. Please contact the admin.'; }"),
        



        # ### Those tooltips lost their target objects over time, turning them BS-Tooltips
        # bsTooltip(id = "findCountry",
        #           title = 'Center the current view of the world map to the selected country.<br><br> Zoom level might need to be adjusted manually.',
        #           placement = "bottom",
        #           trigger = "hover",
        #           options = list(container = "body")),
        # bsTooltip(id = "exportTab",
        #           title = 'Download filtered Carabidae-Fungus interaction records.',
        #           placement = "bottom",
        #           trigger = "hover",
        #           options = list(container = "body")),
        
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
                                sidebar_start_open = FALSE,
                                sidebar_content = tagList(
                                    selectInput(
                                        inputId="findCarabidae_node",
                                        label=NULL,
                                        choices=c("<Carabidae>"="-", unname(unlist(nameSet$CarabidaeSpecies))),
                                        selectize=T),
                                    selectInput(
                                        inputId="findFungus_node",
                                        label=NULL,
                                        choices=c("<Fungus>"="-", unname(unlist(nameSet$FungusSpecies))),
                                        selectize=T)),
                                withLoader(
                                    visNetworkOutput("network", width = "100%"),
                                    type="html",
                                    loader="loader6"),
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
                                    )
                                ) # end: fluidRow 
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
                                enable_label = F,
                                withLoader(
                                    plotlyOutput(outputId="SunburstPolt"),
                                    type="html",
                                    loader="loader6")
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
                                        withLoader(
                                            leafletOutput(outputId="MapPlot"),
                                            type="image",
                                            loader='Loading-bug2.gif'
                                        )
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
            ),
            # ----- Second tab content -----
            tabItem(tabName = "tableTab",
                    titlePanel("Filtered Data"),
                    dataTableOutput(outputId='CarabidaeTable'),
                    tags$p(),
                    downloadButton(outputId="exportTabCSV", " Export Table (.csv)"),
                    downloadButton(outputId="exportTabXLSX", " Export Table (.xlsx)"),
                    hidden(
                        div(id="ProgBar_div", style="display:inline-block;width:100%;text-align: center;",
                            progressBar(id = "ProgBar", value = 0, status = "info", display_pct = TRUE, striped = TRUE))
                    )
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
                            HTML("<p>Fungi and carabids inhabit the same habitats, and they potentially can interact in many ways. The soil environment carabids often inhabit is a reservoir of fungal propagules where the beetles can feed on spores, hyphae or fruiting bodies (Thiele, 1977). They may also be responsible for dispersal of spores of certain fungal species (Traugott et al., 2005). Parasitic or entomopathogenic fungi may be in an obligatory relationship with their beetle hosts (Haelewaters et al., 2021), therefore, the population decline of a ground beetle species could potentially lead to overlooked extinction cascades (Brodie et al., 2014). Nevertheless, we do not have a comprehensive understanding of the fungal-carabid interactions. The knowledge is still limited on how frequently these interactions are established and on how their exact nature affect the parties involved. Indeed, to the best of our knowledge, no scientific effort was taken to catalogue the ways carabids and fungi interact, neither by using traditional taxonomy based on morphology or utilizing identification based on molecular markers. Thus, these interactions have not yet been organized into a comprehensive database.
In order to have a detailed overview of the interactions between Carabidae and the fungal kingdom, we collated a database containing previously reported associations between these taxa. Carabid and fungal species involved in the interaction, the type of the interaction (e. g. parasitism or trophic interactions), the country the interaction was reported from, and the publication source combined with detailed notes to each questionable entry comprised one record.</p>"
                        )),
                        accordionItem(
                            id = "AccItem_2",
                            title = "Data description",
                            color = "info",
                            collapsed = TRUE,
                            HTML("<p>Both data table collection and the importable SQL database consist of five tables: 1) collecting the unique interactions between Carabidae and Fungi from a particular country, 2) listing carabid and fungal taxonomy, 3) listing the references used in the data collection, 4) geographic data for the countries from where the data were recorded, and 5) a metadata table for annotation (Table 1). The metadata table contains information on edits and uncertainties within the database.</p>"
                        )),
                        accordionItem(
                            id = "AccItem_3",
                            title = "Bibliography",
                            color = "info",
                            collapsed = TRUE,
                            HTML("<p>Brodie, J.F., Aslan, C.E., Rogers, H.S., Redford, K.H., Maron, J.L., Bronstein, J.L., Groves, C.R., 2014. Secondary extinctions of biodiversity. Trends in Ecology & Evolution 29, 664-672. https://doi.org/10.1016/j.tree.2014.09.012<br>
Haelewaters, D., Blackwell, M., Pfister, D.H., 2021. Laboulbeniomycetes: Intimate Fungal Associates of Arthropods. Annu. Rev. Entomol. 66, annurev-ento-013020-013553. https://doi.org/10.1146/annurev-ento-013020-013553<br>
                            Thiele, H.U., 1977. Carabid beetles in their environments: a study on habitat selection by adaptations in physiology and behaviour. Springer, Berlin.<br>
                            Traugott, M., Weissteiner, S., Strasser, H., 2005. Effects of the entomopathogenic fungus Beauveria brongniartii on the non-target predator Poecilus versicolor (Coleoptera: Carabidae). Biological Control 33, 107-112. https://doi.org/10.1016/j.biocontrol.2005.01.011</p>"
                        )),
                        accordionItem(
                            id = "AccItem_4",
                            title = "How to cite",
                            color = "info",
                            collapsed = TRUE,
                            HTML("<p>Pozsgai et al., 2021</p><p>The association of ground beetles (Coleoptera: Carabidae) with fungi compiled from a global 200-year review</p>")
                        )
                    ),
                    accordion(
                        inputId = "AccFunding",
                        accordionItem(
                            id = "AccItem_5",
                            title = "Funding",
                            color = "teal",
                            collapsed = FALSE,
                            HTML("<p>G.P., I.B.F. Z.J. and M.Y. were supported by a grant of '111 project' in China. G.P. was supported by a postdoctoral fellowship by the State Key Laboratory of Ecological Pest Control for Fujian and Taiwan Crops, and M.R.M by the Statutory funds of Institute for Agricultural and Forest Environment of Polish Academy of Sciences.</p>")
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
            # ----- Fifth tab content -----
            tabItem(tabName = "radNet",
                fluidRow(
                 #   tags$div(class="img-zoom-container", HTML("<img id='myimage' src='suit-673697_1920.jpg' width='300' height='240'><div id='myresult' class='img-zoom-result'></div>"))

                    column(width = 6,
                        # withLoader(
                        diagonalNetworkOutput("treeCarabidae", width = "100%", height = "2500px"),
                        #   type="image",
                        #   loader='Loading-bug2.gif'
                        # )
                    ),
                    column(width = 6,
                           diagonalNetworkOutput("treeFungus", width = "100%", height = "2500px")
                    )
                    # radialNetworkOutput("radial", height = "1000px")
                )
            )
        )
    )
)