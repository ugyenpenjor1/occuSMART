################################################################################
# SMART Data Analysis & Visualisation Dashboard
# Author: Ugyen Penjor
# Update: January 2026
################################################################################

library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(sf)
library(leaflet)
library(terra)
library(corrplot)
library(unmarked)
library(ggplot2)
library(plotly)
library(fresh)
library(manipulateWidget)
library(progress)
library(shinyjs)
library(shinymaterial)
library(shinyBS)
library(bslib)
library(MuMIn)

# Colour schemes and some customisation
css <- "
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #008080;
  color: white;
  border: 1px solid #ff000000;
  padding: 10px;
  font-size: 15px;
  #font-style: italic;
  text-align: justify;
  margin-left: 0;
  max-width: 500px;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
"

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#4898a8"
  )
)

ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(title = div("Analytics Dashboard", style = "color: black;")),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("question-circle")),
      menuItem("Data Processing", tabName = "data", icon = icon("cogs")),
      menuItem("Model Specifications", tabName = "model", icon = icon("network-wired")),
      menuItem("Visualisation", tabName = "visual", icon = icon("chart-line"))
    ),
    tags$footer(
      p(
        "Developed by ",
        a(
          href = "https://www.linkedin.com/in/ugyen-penjor-3283b2167/",
          "Ugyen Penjor"
        )
      ),
      align = "left",
      style = "position: absolute;
    bottom: 0;
    width: 100%;
    height: 50px; /* Height of the footer */
    color: white;
    padding: 10px;
    background-color: black;
    z-index: 1000;"
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    # Sidebar position
    tags$head(
      tags$style(HTML("
    /* Make the sidebar fixed */
    .main-sidebar {
      position: fixed;
      top: 0;
      left: 0;
      height: 100%;
      overflow-y: auto;
    }

    /* Push main content to the right */
    .content-wrapper, .main-footer {
      margin-left: 230px; /* default sidebar width */
    }

    /* Prevent sidebar menu from overlapping footer */
    .sidebar-menu {
      margin-bottom: 60px; /* enough space above footer */
    }
    
    /* Action button colours */
    
    /* DEFAULT: All action buttons - darker teal with transparency */
    .btn-primary,
    .btn-success,
    .btn-info,
    .btn-default,
    .btn.action-button {
      background-color: rgba(41, 153, 153, 0.85) !important;
      border-color: rgba(41, 153, 153, 0.85) !important;
      color: white !important;
    }
    
    /* Hover state */
    .btn-primary:hover,
    .btn-success:hover,
    .btn-info:hover,
    .btn-default:hover,
    .btn.action-button:hover {
      background-color: rgba(41, 153, 153, 1) !important;
      border-color: rgba(41, 153, 153, 1) !important;
      color: white !important;
    }
    
    /* Active/focus state */
    .btn-primary:active,
    .btn-success:active,
    .btn-info:active,
    .btn-default:active,
    .btn.action-button:active,
    .btn-primary:focus,
    .btn-success:focus,
    .btn-info:focus,
    .btn-default:focus,
    .btn.action-button:focus {
      background-color: #1f7a7a !important;
      border-color: #1f7a7a !important;
      color: white !important;
      box-shadow: 0 0 0 0.2rem rgba(41, 153, 153, 0.5) !important;
    }
    
    /* Download buttons */
    .btn.shiny-download-link {
      background-color: rgba(41, 153, 153, 0.85) !important;
      border-color: rgba(41, 153, 153, 0.85) !important;
      color: white !important;
    }
    
    .btn.shiny-download-link:hover {
      background-color: rgba(41, 153, 153, 1) !important;
      border-color: rgba(41, 153, 153, 1) !important;
    }
    
    /* Browse button (coloured default grey) */
    
    .btn-file,
    input[type='file'] + .btn,
    .input-group-btn .btn {
      background-color: #f4f4f4 !important;
      border-color: #ccc !important;
      color: #333 !important;
    }
    
    .btn-file:hover,
    input[type='file'] + .btn:hover,
    .input-group-btn .btn:hover {
      background-color: #e8e8e8 !important;
      border-color: #adadad !important;
      color: #333 !important;
    }
    
    /* Specific button colours */
    
    /* Deselect All - Dark Red */
    #env_deselect_all {
      background-color: #990000 !important;
      border-color: #990000 !important;
      color: white !important;
    }
    
    #env_deselect_all:hover {
      background-color: #cc0000 !important;
      border-color: #cc0000 !important;
    }
    
    #env_deselect_all:active,
    #env_deselect_all:focus {
      background-color: #770000 !important;
      border-color: #770000 !important;
      box-shadow: 0 0 0 0.2rem rgba(153, 0, 0, 0.5) !important;
    }
    
    /* Harmonise Spatial Data - Orange */
    #harmonise_btn {
      background-color: #F18200FF !important;
      border-color: #F18200FF !important;
      color: white !important;
    }
    
    #harmonise_btn:hover {
      background-color: #EE6900FF !important;
      border-color: #EE6900FF !important;
    }
    
    #harmonise_btn:active,
    #harmonise_btn:focus {
      background-color: #cc7a00 !important;
      border-color: #cc7a00 !important;
      box-shadow: 0 0 0 0.2rem rgba(255, 153, 0, 0.5) !important;
    }
    
    /* Clear all models - Dark Red */
    #clear_models_btn {
      background-color: #990000 !important;
      border-color: #990000 !important;
      color: white !important;
    }
    
    #clear_models_btn:hover {
      background-color: #cc0000 !important;
      border-color: #cc0000 !important;
    }
    
    #clear_models_btn:active,
    #clear_models_btn:focus {
      background-color: #770000 !important;
      border-color: #770000 !important;
      box-shadow: 0 0 0 0.2rem rgba(153, 0, 0, 0.5) !important;
    }
    
    /* Build custom model - Darker teal */
    #update_model_btn {
      background-color: #1a7070 !important;
      border-color: #1a7070 !important;
      color: white !important;
    }
    
    #update_model_btn:hover {
      background-color: #0f5555 !important;
      border-color: #0f5555 !important;
    }
    
    #update_model_btn:active,
    #update_model_btn:focus {
      background-color: #0a4040 !important;
      border-color: #0a4040 !important;
      box-shadow: 0 0 0 0.2rem rgba(26, 112, 112, 0.5) !important;
    }
    
    /* Long buttons */
    
    .btn {
      white-space: normal !important;
      word-wrap: break-word !important;
      text-align: center !important;
      padding: 8px 12px !important;
      min-height: 34px !important;
      height: auto !important;
      line-height: 1.4 !important;
    }
    
    .action-button,
    .btn-primary,
    .btn-success,
    .btn-info {
      width: 60% !important;
      display: block !important;
    }
    
    /* Browse buttons don't wrap */
    .btn-file,
    .input-group-btn .btn {
      white-space: nowrap !important;
      width: auto !important;
    }
    
    /* Download button widths */

    /* Download button width = 20% */
    #download_correlation_plot,
    #download_gof_plot,
    #download_model_selection,
    #download_model_averaging,
    #download_auc_individual,
    #download_auc_modavg,
    #download_individual_occ,
    #download_individual_det,
    #download_modavg_occ,
    #download_modavg_det {
      width: 20% !important;
    }
    
    /* Download button width = 10% */
    #download_btn1 {
      width: 50% !important;
    }
    
    /* Download button width = 70% */
    #download_individual_threat_map,
    #download_modavg_map_data,
    #download_env_data {
      width: 70% !important;
    }
    
    /* Headings */
    
    h1 {
      font-weight: bold !important;
    }
    
    h2 {
      font-weight: bold !important;
    }
    
    .box-title {
      font-weight: bold !important;
    }
  "))
    ),
    tabItems(
      # Overview
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            h1("occuSMART"),
            "This dashboard is exclusively designed to analyse SMART patrol data.
            SMART stands for Spatial Monitoring and Reporting Tool,
            a tool to collect, map, and analyse data on patrols, threats, and 
            wildlife to improve management effectiveness and curb illegal activities. . 
            This dashboard allows users to explore, 
            quantitatively analyse and visualise the output. It employs a 
            standard static occupancy model
            (",
            a(
              href =
                "https://doi.org/10.1890/0012-9658(2002)083[2248:ESORWD]2.0.CO;2",
              "MacKenzie et al. 2002"
            ),
            ") under a maximum likelihood estimation framework using the R
            package 'unmarked'
            (",
            a(
              href = "https://www.jstatsoft.org/article/view/v043i10",
              "Fiske & Chandler, 2011"
            ),
            "). Users are not expected to have
            prior experience or knowledge of the occupancy model (but would
            be an advantage if you have)."
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Input data and design",
            status = "primary",
            solidHeader = TRUE,
            "It is relatively easy to prepare data for the dashboard. You need
            to upload three different file types: 1) a CSV file (.csv), 2) a
            shapefile (.shp), and 3) raster file(s) (.tif). The input
            data is the standard SMART file converted to a comma separated
            value file (.csv). However, it should have mandatory columns such
            as Patrol ID, Team identity, Waypoint Date, Waypoint Time, X and Y
            coordinates (prefereably projected to UTM), and Observation category
            (record of threats - snare, gun shell, cartridges, etc.). You also
            need to provide a shapefile of the patrol area or the area of
            interest (preferably projected using the same coordinate
            reference system (CRS) as that of XY coordinates). Finally, you can
            either provide raster layers of the predictor variables you would like
            to use in the modelling or use download feature of the dashboard. 
            Please upload raster layers that have
            been clipped to the study area or a little larger is acceptable but
            the not excessively large file (e.g., to analyse SMART data of 1000
            km2 area, please DO NOT provide the raster layer
            extent of the whole country). The pixel size of raster layers need
            not have to be the same (if they are, it is better!). Please ensure
            that CRS of shapefile, raster file, and XY coordinates are the same. 
            The dashboard also offers CRS transformation service.
            When the user uploads a shapefile of the area of interest, the dashboard
            automatically creates square grids of 2 km x 2 km in size and
            extracts threat records inside these grid cells. This is done to
            allow sensible threat prediction and for the ease of defining the
            unit of management (practically it is easier to manage a 2 km x 2
            km grid than to manage 30 m x 30 m pixel). In occupancy model, the 
            grids are referred to as 'sites'. The dashboard also
            automatically converts threat records into binary data (1
            for threat detection and 0 otherwise). All the covariate
            values are extracted within these grids. It then employs occupancy
            models. The users have the flexibility to create their models and
            inspect how each performs and produces threat prediction. And it
            all happens at the click of a button. Additional features like model 
            averaging, AUC calculation and probability map creation are available",
            "The idea of employing space-for-time substitution is inspired by
            (",
            a(
              href =
                "https://shop.elsevier.com/books/occupancy-estimation-and-modeling/mackenzie/978-0-12-088766-8",
              "Mackenzie et al., 2006"
            ), ") and
              (",
            a(
              href =
                "https://www.sciencedirect.com/book/9780124071971/occupancy-estimation-and-modeling",
              "Mackenzie et al., 2017"
            ), "). To account for detection
            heterogeneity, repeated assessments of sites can be achieved by
            1) assessing a site at different times over a short
            period of time, 2) a site could be assessed simultaneously and
            independently by different investigators or detection
            methods and finally, 3) separate locations within the site could
            be surveyed, thus substituting spatially partitioned assessments
            for temporally partitioned assessments."
          )
        ),
        fluidRow(
          box(
            title = "Funding and collaboration",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            "This dashboard is developed by Ugyen Penjor, Fauna & Flora.
            Initial test data were provided by Neus Estela & Alex Tamo of Fauna & Flora,
            Liberia."
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(h2("Data Processing"), width = 12)
        ),
        fluidRow(
          theme = bs_theme(version = 4),
          tags$head(
            tags$style(HTML(css)),
            tags$script(HTML(js))
          ),
          box(
            # Selector for CSV file
            title = tagList(icon("upload"), "Upload Data"),
            status = "primary",
            solidHeader = TRUE,
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Upload SMART data in .csv format",
                icon("info-circle")
              )
            ),
            tags$b("Choose CSV file:"),
            fileInput("file", "", accept = ".csv"),
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Select observation column with threat records",
                icon("info-circle")
              )
            ),
            tags$b("Select column to process:"),
            selectInput("column_to_convert", "", ""),
            actionButton("process_btn", "Process observation"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("table"), "Original Data"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            useShinyjs(),
            actionButton("toggle_table", "Expand/Collapse Table"),
            br(),
            br(),
            tags$div(
              id = "table_container",
              style = "display: none;",
              DTOutput("modified_table")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            # Selector for map (shapefile load option)
            title = tagList(icon("file-arrow-up"), "Upload Boundary Shapefile"),
            status = "primary",
            solidHeader = TRUE,
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Please select .shp, .dbf, .sbn, .sbx, .shx, and .prj files",
                icon("info-circle")
              )
            ),
            tags$b("Select shapefile:"),
            fileInput(
              inputId = "filemap",
              label = "",
              multiple = TRUE,
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            combineWidgetsOutput(
              "track_det_threat_count",
              width = "100%",
              height = "400px"
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            combineWidgetsOutput(
              "transect_length_patrol_days",
              width = "100%",
              height = "400px"
            )
          )
        ),
        # Outlier removal
        fluidRow(
          box(
            width = 6,
            title = tagList(icon("broom"), "Clean Outliers (Optional)"),
            status = "primary",
            solidHeader = TRUE,
            checkboxInput("remove_outliers", "Clean outliers and compare with raw data", FALSE),
            actionButton("process_outlier_btn", "Process outlier cleaning")
          )
        ),
        # Plot that compares clean vs raw (only show when remove_outliers is checked)
        conditionalPanel(
          condition = "input.remove_outliers == true",
          fluidRow(
            box(
              width = 12,
              combineWidgetsOutput(
                "track_det_threat_clean_unclean",
                width = "100%",
                height = "400px"
              )
            )
          )
        ),
        # Environmental Data Processing
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("globe"), "Environmental Data Processing"),
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            
            p("Select the environmental data layers you want to download and process:"),
            p(style = "color: red;", strong("Note: Processing time depends on the number of layers and area size.")),
            
            fluidRow(
              # Left column - Dataset selection
              column(
                width = 6,
                
                # Select All / Deselect All
                fluidRow(
                  column(6, actionButton("env_select_all", "Select All", class = "btn-info btn-sm", style = "width: 100%;")),
                  column(6, actionButton("env_deselect_all", "Deselect All", class = "btn-warning btn-sm", style = "width: 100%;"))
                ),
                br(),
                
                # DATASET 1: OSM
                tags$div(
                  style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  h5(tags$b("1. OpenStreetMap (OSM) Features")),
                  checkboxInput("env_osm", HTML("<b>Enable OSM Data</b>"), value = TRUE),
                  conditionalPanel(
                    condition = "input.env_osm",
                    div(
                      style = "margin-left: 20px;",
                      checkboxInput("env_osm_building", "Distance to Buildings", value = TRUE),
                      checkboxInput("env_osm_highway", "Distance to Highways", value = TRUE),
                      checkboxInput("env_osm_waterway", "Distance to Waterways", value = TRUE)
                    )
                  )
                ),
                
                # DATASET 2: CHM
                tags$div(
                  style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  h5(tags$b("2. Canopy Height Model (CHM)")),
                  checkboxInput("env_chm", HTML("<b>Enable Canopy Height</b>"), value = TRUE)
                ),
                
                # DATASET 3: DEM
                tags$div(
                  style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  h5(tags$b("3. Digital Elevation Model (DEM)")),
                  checkboxInput("env_dem", HTML("<b>Enable DEM & Terrain</b>"), value = TRUE),
                  conditionalPanel(
                    condition = "input.env_dem",
                    div(
                      style = "margin-left: 20px;",
                      checkboxInput("env_dem_elevation", "Elevation", value = TRUE),
                      checkboxInput("env_dem_slope", "Slope", value = TRUE),
                      checkboxInput("env_dem_aspect", "Aspect", value = TRUE),
                      checkboxInput("env_dem_tri", "Terrain Ruggedness Index (TRI)", value = TRUE),
                      checkboxInput("env_dem_tpi", "Topographic Position Index (TPI)", value = TRUE),
                      checkboxInput("env_dem_roughness", "Roughness", value = TRUE)
                    )
                  )
                ),
                
                # DATASET 4: GFC
                tags$div(
                  style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  h5(tags$b("4. Global Forest Change (GFC)")),
                  checkboxInput("env_gfc", HTML("<b>Enable Forest Cover</b>"), value = TRUE),
                  conditionalPanel(
                    condition = "input.env_gfc",
                    div(
                      style = "margin-left: 20px;",
                      checkboxInput("env_gfc_treecover", "Tree Cover (%)", value = TRUE),
                      checkboxInput("env_gfc_loss", "Forest Loss", value = TRUE),
                      checkboxInput("env_gfc_gain", "Forest Gain", value = TRUE)
                    )
                  )
                ),
                
                # DATASET 5: HII
                tags$div(
                  style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  h5(tags$b("5. Human Impact Index (HII)")),
                  checkboxInput("env_hii", HTML("<b>Enable Human Impact</b>"), value = TRUE)
                )
              ),
              
              # Right column - Process button & options
              column(
                width = 6,
                tags$div(
                  style = "border: 1px solid #3c8dbc; padding: 15px; border-radius: 5px; background-color: #f9f9f9;",
                  h5(tags$b("Processing options")),
                  checkboxInput("env_mask", "Mask data to AOI boundary", value = TRUE),
                  hr(),
                  actionButton(
                    "process_env_btn",
                    "Process selected data",
                    icon = icon("cloud-download-alt"),
                    class = "btn-primary btn-lg",
                    style = "width: 100%;"
                  ),
                  br(), br(),
                  conditionalPanel(
                    condition = "output.env_ready",
                    downloadButton("download_env_data", "Download Environmental Data",
                                   class = "btn-success", style = "width: 100%;"
                    )
                  ),
                  hr(),
                  h5(tags$b("Selected layers summary:")),
                  verbatimTextOutput("env_selection_summary", placeholder = TRUE)
                )
              )
            ),
            
            hr(),
            conditionalPanel(
              condition = "output.env_ready",
              h4("Environmental Data Layers:"),
              plotOutput("env_data_plot", height = "800px")
            )
          )
        ),
        fluidRow(
          box(
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "This will help reduce overdispersion and help achieve model convergence.",
                icon("info-circle")
              )
            ),
            tags$b("Number of columns to collapse:"),
            br(),
            "(",
            a(
              href =
                "https://doi.org/10.1111/j.1365-2664.2009.01732.x",
              "Kendall & White, 2009"
            ),
            ")",
            selectInput("num_columns", "",
                        choices = c(
                          "No Collapse" = 0,
                          "5" = 5,
                          "7" = 7,
                          "10" = 10,
                          "14" = 14,
                          "30" = 30
                        ),
                        selected = 0
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            DTOutput("collapsed_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            DTOutput("final_df_table")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("file-arrow-up"), "Upload Covariate Rasters"),
            status = "primary",
            solidHeader = TRUE,
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Ensure the raster has the same CRS as shapefile.
              The raster file should be clipped to the study area boundary. But 
              even if you provide a slightly larger than AOI, it will auto-crop to your AOI boundary.
              You can select multiple raster files at once.",
                icon("info-circle")
              )
            ),
            fileInput(
              "raster_files",
              "Choose Raster File(s)",
              accept = c(".tif", ".tiff"),
              multiple = TRUE,
              width = "100%"
            ),
            uiOutput("uploaded_rasters_list"),
            br(),
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Combines spatial covariates from Environmental Data Processing and Upload Covariate Rasters sections. This allows you to merge and review all your raster data together.",
                icon("info-circle")
              )
            ),
            actionButton(
              "harmonise_btn",
              "Harmonise spatial data",
              icon = icon("layer-group"),
              class = "btn-warning",
              style = "width: 100%;"
            ),
            width = 6
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotOutput(outputId = "stacked_plot", height = "800px"),
          )
        ),
        fluidRow(
          box(
            DTOutput("occupancy_data"),
            downloadButton("download_btn1", "Download occupancy data .csv"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("code"), "Create input data for modelling"),
            status = "primary",
            solidHeader = TRUE,
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Detection columns (V1, V2, etc.) are automatically selected",
                icon("info-circle")
              )
            ),
            tags$b("Detection columns (auto-selected):"),
            selectizeInput(
              "col_range_detn",
              "",
              choices = NULL,
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                onInitialize = I("function() { this.disable(); }")
              )
            ),
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "This is what you have uploaded as raster files or downloaded in the Environmental Data Processing section",
                icon("info-circle")
              )
            ),
            tags$b("Select site covariates:"),
            selectizeInput(
              "col_range_site_cov",
              "",
              choices = NULL,
              multiple = TRUE
            ),
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "This is either transect length or patrol days or both. It is created for you using
              the raw data and study area shapefile.",
                icon("info-circle")
              )
            ),
            tags$b("Select observation covariates:"),
            selectizeInput(
              "col_range_det_cov",
              "",
              choices = NULL,
              multiple = TRUE
            ),
            actionButton("update_btn", "Check data structure"),
            width = 6
          ),
          box(
            width = 6,
            title = tagList(icon("magnifying-glass-chart"), "Data structure for occupancy model"),
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("umf_structure")
          )
        ),
        fluidRow(
          box(
            title = HTML('
          <span style="vertical-align: middle;">\\(\\psi\\)</span>
          <span style="vertical-align: middle; padding-left: 0.5px;">Naive occupancy</span>
          '),
            status = "primary",
            solidHeader = TRUE,
            uiOutput("naive_occ_text"),
            "Naive occupancy is calculated as the number of grids with threat
          detection divided by the total number of surveyed grids. It is a
          crude way to guage what proportion of the grids have threats been
          detected at least once.",
            withMathJax(
              helpText("Naive \\(\\psi = \\frac{\\text{sites with threat detected}}{\\text{total surveyed sites}}\\)")
            ),
            br(),
            tabsetPanel(
              tabPanel(
                "Correlation plot",
                box(
                  plotlyOutput(
                    "correlation_plot1",
                    width = 600,
                    height = 600
                  ),
                  br(),
                  downloadButton("download_correlation_plot", "Download", class = "btn-info", style = "width: 100%;")
                )
              ),
              tabPanel(
                "Site covariates (standardised)",
                DTOutput("site_cov_standardized_table")
              ),
              tabPanel(
                "Detection covariate (standardised)",
                DTOutput("det_cov_standardized_table")
              )
            ),
            width = 12
          )
        )
      ),
      tabItem(
        tabName = "model",
        # Section title
        fluidRow(
          box(h2("Model Construction & Selection"), width = 12)
        ),
        # Global Model Builder
        fluidRow(
          box(
            title = tagList(icon("globe"), "Global Model Builder"),
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Select ALL occupancy covariates for global model",
                icon("info-circle")
              )
            ),
            tags$b("Occupancy covariates:"),
            selectizeInput(
              "global_psi_covs",
              "",
              choices = NULL,
              multiple = TRUE
            ),
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Select ALL detection covariates for global model",
                icon("info-circle")
              )
            ),
            tags$b("Detection covariates:"),
            selectizeInput(
              "global_p_covs",
              "",
              choices = NULL,
              multiple = TRUE
            ),
            actionButton("build_global_btn", "Build Global model",
                         icon = icon("cogs"), class = "btn-primary"
            )
          )
        ),
        # GoF
        fluidRow(
          box(
            title = "Goodness-of-Fit Settings (on Global Model)",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            numericInput(
              "nsim_input",
              "Number of bootstrap simulations:",
              value = 100,
              min = 100,
              max = 10000,
              step = 50
            ),
            actionButton(
              "run_gof_btn",
              "Run Goodness-of-Fit simulation",
              icon = icon("check-circle"),
              class = "btn-success"
            ),
            hr(),
            verbatimTextOutput("gof_results")
          ),
          box(
            title = "GoF Bootstrap Distribution",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("gof_plot", height = "400px"),
            br(),
            downloadButton("download_gof_plot", "Download", class = "btn-info", style = "width: 100%;")
          )
        ),
        # Custom Model Builder & Model Selection
        fluidRow(
          box(
            title = tagList(icon("list"), "Custom Model Builder"),
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Select covariates for detection probability",
                icon("info-circle")
              )
            ),
            tags$b("Detection model:"),
            selectizeInput(
              "first_part",
              "",
              choices = c("1", ""),
              multiple = TRUE,
              selected = "1"
            ),
            span(
              "",
              span(
                `data-toggle` = "tooltip", `data-placement` = "right",
                title = "Select covariates for occupancy probability",
                icon("info-circle")
              )
            ),
            tags$b("Occupancy model:"),
            selectizeInput(
              "second_part",
              "",
              choices = c("1", ""),
              multiple = TRUE,
              selected = "1"
            ),
            actionButton("update_model_btn", "Build custom model", icon = icon("plus")),
            br(), br(),
            actionButton("add_model_btn", "Add to model list",
                         icon = icon("list-ul"), class = "btn-success"
            ),
            br(),
            actionButton("add_null_model_btn", "Add Null model",
                         icon = icon("ban"), class = "btn-info"
            )
          ),
          box(
            title = tagList(icon("balance-scale"), "Model Selection"),
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            p("Rank models by AIC once you have built multiple models."),
            actionButton("rank_models_btn", "Rank models",
                         icon = icon("sort-amount-down"), class = "btn-primary"
            ),
            br(),
            sliderInput("delta_aic_threshold",
                        "Δ AIC threshold for averaging:",
                        min = 0, max = 10, value = 2, step = 0.5
            ),
            actionButton("model_avg_btn", "Model average",
                         icon = icon("calculator"), class = "btn-success"
            )
          )
        ),
        # Model Storage
        fluidRow(
          box(
            title = tagList(icon("database"), "Model Storage"),
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_list_display"),
            br(),
            tags$b("Select a model to view details:"),
            selectInput("selected_model_name",
                        "",
                        choices = NULL,
                        width = "100%"
            ),
            actionButton("view_model_btn", "View model details",
                         icon = icon("eye"), class = "btn-info"
            ),
            br(),
            actionButton("clear_models_btn", "Clear all models",
                         icon = icon("trash"), class = "btn-danger"
            )
          )
        ),
        # Selected model formula
        fluidRow(
          box(
            title = HTML('
      <span style="vertical-align: middle;">\\(\\oint\\)</span>
      <span style="vertical-align: middle; padding-left: 0.5px;">Selected Model Formula</span>
      '),
            width = 12,
            uiOutput("model_name_output")
          )
        ),
        # Selected Model Summary
        fluidRow(
          box(
            title = tagList(icon("clipboard-list"), "Selected Model Summary"),
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_summary")
          )
        ),
        # Model Selection Results
        fluidRow(
          box(
            title = tagList(icon("table"), "Model Selection Results"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("model_selection_table"),
            br(),
            downloadButton("download_model_selection", "Download (.csv)", class = "btn-info", style = "width: 100%;")
          )
        ),
        # Model Averaging Results
        fluidRow(
          box(
            title = tagList(icon("chart-bar"), "Model Averaging Results"),
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_avg_summary"),
            DTOutput("model_avg_coef"),
            br(),
            downloadButton("download_model_averaging", "Download (.csv)", class = "btn-info", style = "width: 100%;")
          )
        ),
        # AUC
        fluidRow(
          box(
            title = tagList(icon("check-circle"), "Area Under ROC Curve"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "Individual model AUC",
                br(),
                tags$b("Select a model to calculate AUC:"),
                selectInput("auc_model_select",
                            "",
                            choices = NULL,
                            width = "100%"
                ),
                p("Two curves will be displayed:"),
                tags$ul(
                  tags$li(tags$b("Blue curve:"), " Using actual observed detections (conservative)"),
                  tags$li(tags$b("Red curve:"), " Using latent occupancy estimates (detection-corrected)")
                ),
                uiOutput("auc_text_individual_dual"),
                br(),
                "More information on AUC (",
                a(
                  href = "https://doi.org/10.1890/11-1936.1",
                  "Zipkin et al. 2012"
                ), ").",
                br(),
                plotlyOutput("auc_plot_individual_dual", height = "35em"),
                br(),
                downloadButton("download_auc_individual", "Download", class = "btn-info", style = "width: 100%;")
              ),
              tabPanel(
                "Model-averaged AUC",
                br(),
                p("Calculate model-averaged AUC using models within the Δ AIC threshold."),
                p("Two curves will be displayed:"),
                tags$ul(
                  tags$li(tags$b("Blue curve:"), " Using actual observed detections (conservative)"),
                  tags$li(tags$b("Red curve:"), " Using latent occupancy estimates (detection-corrected)")
                ),
                sliderInput("auc_delta_threshold_modavg",
                            "Δ AIC threshold for averaging:",
                            min = 0, max = 10, value = 2, step = 0.5
                ),
                actionButton("calc_auc_modavg_btn",
                             "Calculate model-averaged AUC",
                             icon = icon("calculator"),
                             class = "btn-success"
                ),
                br(), br(),
                uiOutput("auc_text_modavg_dual"),
                br(),
                plotlyOutput("auc_plot_modavg_dual", height = "35em"),
                br(),
                downloadButton("download_auc_modavg", "Download", class = "btn-info", style = "width: 100%;")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "visual",
        fluidRow(
          box(h2("Visualisation"), width = 12)
        ),
        # Individual model threat map
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("map-marked-alt"), "Individual Model Threat Map"),
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 4,
                tags$b("Select a model to visualise:"),
                selectInput("threat_map_model_select",
                            "",
                            choices = NULL,
                            width = "100%"
                ),
                br(),
                selectInput(
                  inputId = "variable1",
                  label = "Select variable to display:",
                  choices = c(
                    "Predicted" = "Predicted",
                    "Standard Error" = "SE",
                    "Lower 95% CI" = "lower",
                    "Upper 95% CI" = "upper"
                  ),
                  selected = "Predicted"
                ),
                p("Select a model from the list to generate its spatial threat map."),
                br(),
                downloadButton("download_individual_threat_map",
                               "Download map (.tif)",
                               class = "btn-info",
                               style = "width: 100%;"
                )
              ),
              column(
                width = 8,
                leafletOutput(outputId = "map_threat_individual", height = "600px")
              )
            )
          )
        ),
        
        # Model-averaged threat map
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("layer-group"), "Model-averaged Threat Map"),
            solidHeader = TRUE,
            status = "success",
            fluidRow(
              column(
                width = 4,
                p(sprintf("Uses models within Δ AIC threshold from Model Selection (currently: %.1f)", 2)),
                p(
                  style = "color: red;",
                  strong("Note: This may take several minutes for large areas or many models.")
                ),
                actionButton("generate_modavg_map_btn",
                             "Generate model-averaged map",
                             icon = icon("map"),
                             class = "btn-success btn-lg",
                             style = "width: 100%;"
                ),
                br(), br(),
                conditionalPanel(
                  condition = "output.modavg_map_ready",
                  selectInput(
                    inputId = "variable2",
                    label = "Select variable to display:",
                    choices = c(
                      "Predicted" = "Predicted",
                      "Standard Error" = "SE",
                      "Lower 95% CI" = "lower",
                      "Upper 95% CI" = "upper"
                    ),
                    selected = "Predicted"
                  ),
                  downloadButton("download_modavg_map_data",
                                 "Download map (.tif)",
                                 class = "btn-info",
                                 style = "width: 100%;"
                  )
                ),
                uiOutput("modavg_map_info")
              ),
              column(
                width = 8,
                conditionalPanel(
                  condition = "output.modavg_map_ready",
                  leafletOutput(outputId = "map_threat_modavg", height = "600px")
                )
              )
            )
          )
        ),
        # Individual model covariate plots
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("chart-line"), "Individual Model Covariate Relationships"),
            solidHeader = TRUE,
            status = "primary",
            
            fluidRow(
              column(
                width = 3,
                selectInput(
                  "covariate_plot_model_select",
                  "Select model:",
                  choices = NULL,
                  width = "100%"
                ),
                hr(),
                h5("Model summary:"),
                verbatimTextOutput("selected_model_summary", placeholder = TRUE),
                style = "max-height: 500px; overflow-y: auto;"
              ),
              
              column(
                width = 4,
                h5("Occupancy covariates"),
                selectInput(
                  "individual_occ_var",
                  "Select occupancy variable:",
                  choices = NULL,
                  width = "100%"
                ),
                plotlyOutput("individual_occ_plot", height = "400px"),
                br(),
                downloadButton("download_individual_occ", "Download", class = "btn-info btn-sm", style = "width: 100%;")
              ),
              
              column(
                width = 4,
                h5("Detection covariates"),
                selectInput(
                  "individual_det_var",
                  "Select detection variable:",
                  choices = NULL,
                  width = "100%"
                ),
                plotlyOutput("individual_det_plot", height = "400px"),
                br(),
                downloadButton("download_individual_det", "Download", class = "btn-info btn-sm", style = "width: 100%;")
              )
            )
          )
        ),
        
        # Model-averaged covariate plots
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("layer-group"), "Model-averaged Covariate Relationships"),
            solidHeader = TRUE,
            status = "success",
            
            fluidRow(
              column(
                width = 3,
                p("Averages predictions across top models within Δ AIC threshold."),
                p(style = "color: red;", strong("Note: May take a couple of minutes to generate.")),
                actionButton(
                  "generate_modavg_plots_btn",
                  "Generate model-averaged plots",
                  icon = icon("chart-line"),
                  class = "btn-success btn-lg",
                  style = "width: 100%;"
                ),
                br(), br(),
                conditionalPanel(
                  condition = "output.modavg_plots_ready",
                  h5("Plot information:"),
                  verbatimTextOutput("modavg_plots_info", placeholder = TRUE)
                )
              ),
              
              column(
                width = 4,
                conditionalPanel(
                  condition = "output.modavg_plots_ready",
                  h5("Occupancy covariates"),
                  selectInput(
                    "modavg_occ_var",
                    "Select occupancy variable:",
                    choices = NULL,
                    width = "100%"
                  ),
                  plotlyOutput("modavg_occ_plot", height = "400px"),
                  br(),
                  downloadButton("download_modavg_occ", "Download", class = "btn-info btn-sm", style = "width: 100%;")
                )
              ),
              
              column(
                width = 4,
                conditionalPanel(
                  condition = "output.modavg_plots_ready",
                  h5("Detection covariates"),
                  selectInput(
                    "modavg_det_var",
                    "Select detection variable:",
                    choices = NULL,
                    width = "100%"
                  ),
                  plotlyOutput("modavg_det_plot", height = "400px"),
                  br(),
                  downloadButton("download_modavg_det", "Download", class = "btn-info btn-sm", style = "width: 100%;")
                )
              )
            )
          )
        )
      )
    )
  )
) # end