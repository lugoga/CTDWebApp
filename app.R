
require(shiny)
require(shinydashboard)
require(sf)
require(shinyWidgets)
require(tidyverse)
require(highcharter)
require(vistime)
require(DT)
require(tmap)
require(magrittr)
require(metR)




theme_set(theme_bw(base_size = 14))

## basemaps -----
pemba.sf = st_read("data/pemba_sf.shp")

# Data ----

## sapphire data ----
sap = read_csv("data/ctd_dsfa_sapphire.csv")

sap %>% 
  mutate(project = if_else(project == "DPFA", "SAPPHIRE", project)) %>% 
  distinct(lon, lat, month, .keep_all = TRUE) %>% 
  group_by(month, project) %>% 
  count()

sap = sap %>%   
  mutate(project = if_else(project == "DPFA", "SAPPHIRE", project)) %>% 
  mutate(station = NA, year = lubridate::year(time), depth = NA) %>% 
  select(program = project, station, lon,lat, time, month, year, pressure, depth, 
         temperature, conductivity, fluorescence, oxygen, salinity )

sap %>% 
  # mutate(project = if_else(project == "DPFA", "SAPPHIRE", project)) %>% 
  distinct(lon, lat, month, .keep_all = TRUE) %>% 
  group_by(month, program) %>% 
  count()


## Other CTD data ----

ctd = read_csv("data/ctd_combined.csv")

ctd = ctd %>% 
  # distinct(program)
  filter(!program %in% c("SAPPHIRE", "DSFA"))

### combine 

ctd.all = sap %>% select(-month) %>% 
  bind_rows(ctd%>% select(-month)) %>% 
  select(-c(station, depth)) %>% 
  mutate(month = lubridate::month(time, abbr = FALSE, label = FALSE),
         season = if_else(month %in% c(6:9), "SE", "NE")) %>% 
  mutate(type = if_else(program %in% c("Algoa", "IIOE-2"), "Expeditions", "Locals"),
         lat = if_else(lat >0,-1*lat,lat))

  casts = ctd.all %>% 
    distinct(program,lon,lat,time, .keep_all = TRUE) 
  
  casts.counts = casts %>% 
    group_by(program, season, type) %>% 
    count() %>% 
    ungroup()

  casts.sf = casts %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) 
  
  programs = casts.counts %>% distinct(program) %>% pull()
  
  local.expeditions = ctd.all %>% 
    filter(!program %in% c("Algoa", "IIOE-2")) %>% 
    # mutate(program = str_to_upper(program)) %>% 
    distinct(program) %>% pull()
  
  ## water samples ----- 
  insitu.data = read_csv("data/water_samples.csv")
  
  water.season = insitu.data %>% 
    distinct(season) %>% pull()
  
  water.variable = insitu.data %>% 
    distinct(variable) %>% 
    pull()
  
 ## Expedition processed ----
  algoa = ctd.all %>% 
    filter(program == "Algoa") %>% 
    mutate(transect = case_when(lat > -6 ~ "North",
                                between(lat, -8,-6) ~ "Middle",
                                # lat > -8 & lat < -6 ~ "Middle",
                                lat > -10 & lat < -8~ "South")) 
  
  agulhas = ctd.all %>% 
    filter(program == "IIOE-2" & year == 2017) %>% 
    mutate(transect = case_when(between(lat, -7.2,-7) ~ "Middle",
                                between(lat,-6.7,-6.5) ~ "North",
                                lat < -9 ~ "South"))
  
  
  expeditions = algoa %>% 
    bind_rows(agulhas)%>% 
    pivot_longer(cols = temperature:salinity, names_to = "variable", values_to = "data")
  
  expedition.program = expeditions %>% distinct(program) %>% pull()
  transects = expeditions %>% filter(!is.na(transect)) %>% distinct(transect) %>% pull()
  varCtd = expeditions %>% distinct(variable) %>% pull()
  
  
  zzz = tibble(var = varCtd, number = 1:5)
  
  
  ## define colors----
  mycolor2 = c("#040ED8", "#2050FF", "#4196FF", "#6DC1FF", "#86D9FF", "#9CEEFF", "#AFF5FF", "#CEFFFF", "#FFFE47", "#FFEB00", "#FFC400", "#FF9000", "#FF4800", "#FF0000", "#D50000", "#9E0000")
  mycolor = c("#7f007f", "#0000ff",  "#007fff", "#00ffff", "#00bf00", "#7fdf00",
              "#ffff00", "#ff7f00", "#ff3f00", "#ff0000", "#bf0000")
  odv_color = c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa") %>% rev()
  pal = wesanderson::wes_palette("Zissou1", 21, type = "continuous")
  mycolor3 = c("#9000B0", "#C900B0", "#C760AF", "#1190F9", "#60C8F8", "#90C9F8", "#F8F8F8", "#F8F800",  "#F8D730", "#f8b030", "#f8602f", "#f80000")
  mycolor4 = hcl.colors(n = 30, palette = "blues")
  mycolor5 = hcl.colors(n = 30, palette = "reds")
  
  drawing.colors = list(mycolor3, mycolor2, mycolor, mycolor4, mycolor5 )
  
  
  ## river discharge -----
  rivers = read_csv("data/river_flow_master_long.csv") %>% 
    mutate(station = if_else(is.na(station), "Kibungo", station)) 
  
  
  site.rivers = c("Wami", "Pangani River", "Ruvu Kibungo")
  
# UI ----
ui = navbarPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "journal"), 
  tags$img(src = "coat.png", width = "25%", height = "25%"),
  dashboardHeader(title = "", disable = TRUE),
  useShinydashboard(),
  ## link CSS 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "ocean.css")
  ),
  windowTitle = "The IMS' Hydrographic DataHub",
  
  ## about ----
  tabPanel(title = "About",
           fluidRow(
             column(width = 1),
             column(width = 5,tags$h4("The CTD Data Center in Tanzania"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$p("The need for use of oceanographic data in management of the marine resources is very clear. However, in the case of Tanzania the source of such data has always been a big challenge. As a contribution to solving this problem the Institute of Marine Sciences, Tanzania applied for funding under SAPPHIRE Project to conduct oceanographic surveys of the Pemba channel and compile existing data from the Republic of South African Cruises, mainly Agulhas II (2017 and 2018) that were made in the Tanzanian waters and set up the initial data base and make it readily accessible. ")
             ),
             column(width = 3,
                    tags$p("By studying the oceanographic parameters of the waters, IMS was also expected to establish the oceanographic parameters influencing the timing and location of the spawning of tuna and tuna-like species in the Tanzania Northern waters forming a demonstration project under SAPPHIRE. At the end of the two activities, two follow-up activities were carried out which was to conduct a workshop to inform fishermen on the oceanographic conditions of the Pemba Channel and publication of the findings through policy briefs and peer review publications. ")
             ),
             column(width = 4,
                    tags$p("As a major recommendation on oceanographic data in Tanzania, scientists carrying out oceanographic work in the Tanzanian waters should be encouraged to supply their data to a database such as the National Oceanographic Data Centre housed at the Institute of Marine Sciences of the University of Dar es Salaam. From the data that were collected from the previous cruises and the field campaigns, It can be said that the temperature, dissolved oxygen, and nutrients attaining in the study area are supportive for tuna fishery. However, the fishermen should be supported to improve their fishing vessels and gears to be able to profitably fish the tuna and tuna-like species.")
             ),
             column(width = 1)
           ),
           fluidRow(column(width = 1), column(width = 10, tags$hr())),
           fluidRow(
             column(width = 1),
             column(width = 5, tags$h4("Infobox"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2, infoBoxOutput(outputId = "casts", width = NULL)),
             column(width = 2, infoBoxOutput(outputId = "locals",width = NULL)),
             column(width = 2, infoBoxOutput(outputId = "expeditions",width = NULL)),
             column(width = 2, infoBoxOutput(outputId = "zones",width = NULL)),
             column(width = 2,  infoBoxOutput(outputId = "seasons",width = NULL))
             ),
           fluidRow(column(width = 1), column(width = 10, tags$hr())),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$h4("Facts & Facts"),
                    tags$p("SWIOFISH through Octopus fishery made 122 casts, followed by Estuarize-WIO in Ruvu and Rufiji River with 78 casts. In the Pemba Channel, the SAPPHIRE cast 51 CTDs, while the Whale and Shark Projects cast 40 CTDs in the Mafia Channel. TAFIRI, with the assistance of WWF, deployed CTD into 40 casts in the Mafia Channel. The PEACE projects collected 17 casts in the Pemba channel and 10 casts in the Bagamoyo area.")
                    ),
             column(width = 3, highchartOutput(outputId = "CtdNumberId")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
             column(width = 2, 
                    helpText("Simply by toggle the season of the bar plot you will be able to visualize the CTD Cast sampled during the Northeast (NE) or Southeast (SE) monsoon season. Furthermore, the number of each program have locations, to indicate where the casts were deployed along the coastal and marine waters of Tanzania. You can further choose the expeditions and map the positions of each CTD casts"),
                    selectInput(inputId = "programsId", label = "", choices = programs, selected = programs[5]),
                    helpText("These points are clustered and shown in numbers. Therefore, to unfold the position you simply click the cluster to zoom in into individual casts")
                    ),
             column(width = 3, tmapOutput(outputId = "castsMap")%>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
           ),
           tags$br(),
           fluidRow(
             column(width = 1),
             column(width = 5, tags$h4("Acknowledgements"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 3,
                    helpText("The SAPPHIRE project under the Institute of Marine Sciences of the University of Dar es Salaam could not have carried out this research without the help of a large number of people and institutions in kind and financial supports.")
                    ),
             column(width = 3,
                    helpText("The United Nations Environment Programme is commended for funding the Western Indian Ocean Large Marine Ecosystems Strategic Action Programme Policy Harmonisation and Institutional Reforms project (WIO LME SAPPHIRE).")
             ),
             column(width = 3,
                    helpText("The UNDP-led SAPPHIRE project is being carried out by the Nairobi Convention Secretariat with funding from the Global Environmental Facility (GEF)")
             )
           ),
           tags$br(),
           fluidRow(
             column(width = 1),
             column(width = 2,imageOutput(outputId = "logo1", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
             column(width = 2,imageOutput(outputId = "logo2", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
             column(width = 2,imageOutput(outputId = "logo3", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
             column(width = 2,imageOutput(outputId = "logo4", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
             column(width = 2,imageOutput(outputId = "logo5", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br())
           ),
           tags$br(),
           tags$br(),
           tags$br()
           ),
  ## CTD Sapphire ----
  tabPanel(title = "CTD",
           fluidRow(
             column(width = 1),
             column(width = 8, 
                    tags$h4("CTD Data Acquisaition")
                    )
             ),
           fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$p("To complement the previous expeditions, the oceanographic field campaign aimed to collect profiles using CTD and other water quality parameters including chlorophyll-a, suspended solids, salinity, dissolved oxygen (DO) and pH. In this report field campaigns during the NE and SE monsoon seasons were carried out from February 2020 to April 2020 and between July and October 2020 sampling the three transects across the Pemba Channel. The March 2020 campaign included CTD casts, water sampling and ADCP deployment")
                    ),
             column(width = 3, 
                    tags$p("The area between Pangani and Tanga was selected to capture the oceanographic variables within the western side of the Pemba Channel (off Tanga) and between Mkoani and Wete in Pemba Island was selected to capture the field campaign on the western side of Pemba island (Figure 3).  The field campaign was planned between February and April in order to capture the oceanographic conditions during the Northeast monsoon season, which starts in October/November and ends in March/April. The southeastern monsoon period was covered between July and October.")
                    ),
             column(width = 3,imageOutput(outputId = "pembaMap", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()
                    ),
             # column(width = 4, 
             #        tags$p("The Pemba Channel is the least explored among the three channels found in Tanzania. The reason are myriad ......"))
           ),
           fluidRow(column(width = 1), column(width = 11, tags$hr())),
           fluidRow(
             column(width = 1),
             column(width = 6,
                    tags$h4("Surface Maps in the Pemba Channel")
                    )
           ), fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$p("Seasonal contour maps are used to represent the general features of NE vs. SE distributions of surface parameters in the Pemba Channel. Hydrographic profile data for selected depth were binned and organized by coordinate position into a matrix of 10 kilometer grid squares.")
                    ),
             column(width = 3,
                    tags$p("You can map surface of any variable of for each season - here defined as May-September for southeast (SE) and October-March for NE monsoon season based on parameter similarities in between-month analyses of variance.")
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 1,
                    dropdown(
                      
                      pickerInput(inputId = 'seasonId1',
                                  label = "",
                                  choices = c("NE", "SE"), selected = "SE",
                                  options = list(`style` = "btn-warning")),
                      
                      pickerInput(inputId = "zvar1", 
                                  label = "", 
                                  choices = varCtd, 
                                  selected = varCtd[1], 
                                  # choices = c("temperature", "conductivity", "fluorescence", "oxygen", "salinity"), 
                                  # selected = "temperature",
                                  options = list(style = "btn-info")),
                      
                      sliderInput(inputId = 'depthId1',
                                  label = "",
                                  value = 10,
                                  min = 0,
                                  max = 100, 
                                  step = 10),
                      
                      sliderInput(inputId = 'depthId2',
                                  label = "",
                                  value = 50,
                                  min = 50,
                                  max = 150, 
                                  step = 10),
                      
                      style = "unite", icon = icon("gear"),
                      status = "danger", width = "300px",
                      animate = animateOptions(
                        enter = animations$fading_entrances$fadeInLeftBig,
                        exit = animations$fading_exits$fadeOutRightBig
                      )
                    ),
                    helpText("Hover on the gear icon to see the hidden drop downmenu that allows you to select the variable and specify depth of surface plots. Then, after computation a profile plot and surface map of the first depth and second depth will display on separate windows")
             ),
             column(width = 2,
                    plotOutput(outputId = "profileId")
                    ),
             
             column(width = 3,
                    plotOutput(outputId = 'plot2') %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                    ),
             column(width = 3, 
                    plotOutput(outputId = 'plot3') %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                    )
           ),
           tags$br(),
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("The Raw Profile Data")
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 1,
                    tags$p("The queried profile data that used to map the surface distribution at specified depth is alsopresented in this table. With the standard menu, you can download the data in four different format depend which one suits you")),
             column(width = 9, DT::DTOutput(outputId = "sapphireTable") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
           ),
           tags$br(),
           tags$br(),
           tags$br()
           ),
  ## water samples -----
  tabPanel(title = "Water Samples",
    fluidRow(
      column(width = 1),
      column(width = 5, tags$h4("Isosurface Plots"),
             tags$p("Water samples were collected using Naskin bottles at each cast location along the CTD deployment. This dataset was collected at the same geographical locations as CTD data. Temperature, chlorophyll-a, dissolved oxygen, nitrate, and phosphate levels in the water were measured at three depths: the surface, 15 meters, and 30 meters.")
             )
      ),
    fluidRow(
      column(width = 1),
      column(width = 3, 
             tags$h6("What is an isosurface plot?"),
             tags$p("An isosurface plot depicts the variation of a single variable across a plane. A isosurface plot is not always a graphic depicting any variable at the ocean's surface, though it can be. Isosurface plots depict any variable as it would appear on a three-dimensional sheet within (or on, or just beneath) the sea, defined by a constant value of another variable. For instance a temperature across the Pemba Channel at 50 meters from the surface.")
             ),
      column(width = 3, 
             tags$h6("Why Isosurface?"),
             tags$p("When it was discovered that mixing in the sea occurs along iso-density surfaces, or sheets of constant density, this type of plot became well-known and widely used. Such plots are now created and interpreted for a wide range of variables, beginning with density surfaces.")),
      column(width = 3, 
             tags$h6("Interpreting Isosurface Plots"),
             tags$p("You can now demonstrate how something varies on longitude and latitde plane. What does nitrate in the Pemba Channel at depth zero tell you? Remember that you can create isosurface plots for Chl and phosphate in the same channel. Creating an isosurface plot similar to the one shown above, but for different depths, may help you understand what is going on in the water."))
    ),
    fluidRow(
      column(width = 1),
      column(width = 1,
             tags$h6(""),
             helpText("Select a variable to map isosurface"),
             radioGroupButtons(inputId = "varwaterId", label = "", choices = water.variable, selected = water.variable[1], direction = "vertical", status = "primary"),
             helpText("Choose either NE and SE  season"),
             radioGroupButtons(inputId = "waterSeasonId", label = "", choices = water.season, selected = water.season[1], direction = "horizontal", status = "info")
      ),
      column(width = 3, plotOutput(outputId = "plotSuface") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
      column(width = 3, plotOutput(outputId = "plotMiddle") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
      column(width = 3, plotOutput(outputId = "plotBottom") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
    ),
    fluidRow(
      column(width = 1),
      column(width = 1,
             tags$h6("Raw Data"), 
             helpText("The raw data for each variable selected for isosurfaces plots are displayed in the table. You can download the data in the four different format by simply click the standard menu at the top of each table")),
      column(width = 3, DT::DTOutput(outputId = "tableSurface") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
      column(width = 3, DT::DTOutput(outputId = "tableMiddle") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
      column(width = 3, DT::DTOutput(outputId = "tableBottom") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
    ),
    tags$br(),
    tags$br()
    
  ),
  ## expeditions ----
  tabPanel(title = "Expeditions",
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("The Major Expeditions"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("Though several expeditions crossed and sampled coastal and marine waters of Tanzania, however only few data are available that are made public"),
                    tags$li("The First is the Algoa of 2004"),
                    tags$li("The second is the Agulhas of 2017"),
                    tags$li("The third is the Agulhas of 2018"),
                    tags$br()
                    ),
             column(width = 3,
                    tags$p("Several expeditions that were conducted in the Indian Ocean crossed the coastal and marine waters of Tanzania and casted several CTD. The three major ones for which they made their data available to the public include the Alga Expedition, which was conducted in August 2004 and the Agulhas II under the International Indian Ocean Expedition II, which was conducted in October 2017 and June 2018. These expeditions aimed to collect hydrographic profiles of temperature, oxygen, salinity, and conductivity to better understand the physical oceanography of the area.")
                    ),
             column(width = 3,
                    tags$p("They generated a substantial amount of data that serve as an oceanographic knowledge baseline of the coastal and marine waters of the to help manage and ensure the sustainable growth of the Blue. This WebApp presents the analysis of 790 water profiles of temperature, salinity and dissolved oxygen concentration sampled at 276 stations over the course of a 15-year oceanographic program (2004-2018). These profiles were analyzed using along-channel vertical sections plots (i.e., transects), surface property maps and water masses statistics.")
                    ),
             column(width = 1)
           ),
           fluidRow(column(width = 1), column(width = 10, tags$hr())),
           fluidRow(
             column(width = 1),
             column(width = 1,
                    helpText("Select the expedition. Choose either Algoa or IIOE-II"),
                    radioButtons(inputId = "expedId", label = "", choices = expedition.program, selected = expedition.program[1]),
                    helpText("select a transect. The coastal was divided into three longitudinal transects"),
                    radioButtons(inputId = "transectId", label = "", choices = transects, selected = transects[1])),
             column(width = 1,
                    helpText("Choose a variable of interest. Salinity is the derived variable"),
                    radioButtons(inputId = "variableId", label = "", choices = varCtd, selected = varCtd[1]),
                    helpText("Slide to specify a depth"),
                    sliderInput(inputId = "depthId", label = "", min = 0, max = 1000, value = c(0,200), step = 10, round = 0,ticks = TRUE)
                    
                    ),
             column(width = 3, plotOutput(outputId = "sectionPlot")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
             # column(width = 1),
             column(width = 5, DTOutput(outputId = "table1") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
             column(width = 1)
           )
           
           ),
  ## RIver Discharge ----
  tabPanel(title = "River Flow",
           fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$h6("Pangani RIver"),
                    tags$p("The Pangani River rises on Kilimanjaro and flows southeast for some 250 miles (400 km) to enter the Pemba Channel of the Indian Ocean, northwest of the island of Zanzibar.")),
             column(width = 3,
                    tags$h6("Wami River"),
                    tags$p("The Wami River is a river located in Pwani Region, Tanzania. The headwaters of the Wami and its tributaries originate in the Eastern Arc Mountains of Morogoro Region. The river then flows northeastward through of Pwani Region to empty into the Indian Ocean west of Zanzibar Island")
                    ),
             column(width = 3,
                    tags$h6("Ruvu River"),
                    tags$p("The Ruvu River originates in the southern Uluguru Mountains and flows eastwards to empty into the Indian Ocean near Bagamoyo.")
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    helpText("Simply specify the date range and choose the river you wish to make plot. Furthermore, you can select a polygon of the plot to visualize the raw data in table"),
                    sliderInput(inputId = "flowDate", label ="", min = lubridate::ymd("20101001"), max = lubridate::ymd("20201001"), value = c(lubridate::ymd("20101001"), lubridate::ymd("20201001")), dragRange = TRUE),
                    radioGroupButtons(inputId = "riverId", label = "", choices = site.rivers,selected = site.rivers[2], direction = "vertical", status = "primary")
                    
                    ),
             column(width = 4,
                    plotOutput(outputId = "riverDischarge", brush = "plot_brush", width = 500, height = 350) %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                    ),
             column(width = 4,
                    DT::DTOutput(outputId = "selectedRiversData")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")
             )
           ),
           fluidRow(),
           tags$br(),
           tags$br()
           ),
  ## argo ----
  tabPanel(title = "Argo"),
  ## developer ----
  tabPanel(title = "Developer",
           fluidRow(
             column(width = 1),
             column(width = 6,tags$h4("About the Developer"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,imageOutput(outputId = "member14", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                    ),
             column(width = 3,
                    tags$p(
                      "This Web Application was developed by Semba,  a data scientist at the Nelson Mandela African Institution of Science and Techlogy, where I focus on",
                      "data analysis, statistical and machine learning and present the findings in visual form like plots, interactive reporting,",
                      ". I enjoy building data tools that are capable of solving problem facing the society,",
                      "like this dashboard. You can find more of the things I like to build on my webpage",
                      HTML(paste0(tags$a(href = "https://semba-blog.netlify.app/", "semba blog", target = "_blank"), "."))
                    )),
             column(width = 3,
                    tags$p(
                      "Semba use data science tools to support research with",
                      tags$a(href = "https://hinger.netlify.app/", "hinger", target = "_blank"),
                      "I'm lucky to get to use R and RStudio tools on a daily basis to help others",
                      "learn from clients, I have succumb myself into a myriad of data that evolved my skills to handle both spatial and non-spatial data",
                      " and report them through dashboards and interactive reports like this one."
                    ),),
             column(width = 2,
                    tags$p(
                      "Get in touch with me on Github at",
                      HTML(paste0("(", tags$a(href = "https://github.com/lugoga", "@semba", target = "_blank"), "),")),
                      "online at",
                      HTML(paste0(tags$a(href = "https://hinger.netlify.app/", "", target = "_blank"), ",")),
                      "or by email at",
                      HTML(paste0(tags$a(href = "mailto:lugosemba@gmail.com", "Semba"), "."))
                    )),
             column(width = 1),
           ),
           tags$br(),
           tags$br()
          
           )
  
  
)

# server ----
server = function(input, output, session){
  
  ## about ----
  
    
  output$CtdNumberId = renderHighchart({
    
    casts.counts %>% 
      # filter(type == "Locals") %>%
      arrange(desc(n)) %>% 
      # filter(program == programs[4]) %>%
      hchart(type = "bar", hcaes(x = program, y = n, group = season)) %>% 
      hc_colors(colors = hcl.colors(n = 2, palette = "Berlin") %>% rev()) %>% 
      hc_xAxis(title = FALSE) %>% 
      hc_yAxis(title = list(text = "Number of Casts"))%>%
      # hc_legend(
      #   align = "top",
      #   verticalAlign = "top",
      #   layout = "vertical",
      #   x = 0,
      #   y = 100
      # )%>%
      hc_credits(
        enabled = TRUE,
        text = "Semba Blog",
        href = "https://hinger.netlify.app/"
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  output$castsMap = renderTmap({
    tm_basemap(server = c("Open Street" = "OpenStreetMap")) +
      tm_shape(shp = casts.sf%>% filter(program == input$programsId),name = input$programsId) +
      tm_markers(clustering = TRUE)
  })
  
  output$pembaMap = renderImage({
    list(src = "img/pemba.png",
         contentType = 'image/png', height = 260,
         alt = "The Map of Pemba Channel")
  }, deleteFile = FALSE)
  
  ## expeditions ----
 
  ## water samples ----
  water.surface = reactive({
    insitu.data  %>% 
      filter(variable == input$varwaterId & season == input$waterSeasonId & strata == "S")  %$%
      akima::interp(x = lon,y = lat,z = data, 
                    linear = FALSE,
                    jitter = 200, 
                    jitter.random = TRUE,
                    extrap = FALSE,
                    duplicate = "strip") %>% 
      akima::interp2xyz() %>% 
      as_tibble() 
    
  })
  
  water.middle = reactive({
    insitu.data  %>% 
      filter(variable == input$varwaterId & season == input$waterSeasonId & strata == "M")  %$%
      akima::interp(x = lon,y = lat,z = data, 
                    linear = FALSE,
                    jitter = 200, 
                    jitter.random = TRUE,
                    extrap = FALSE,
                    duplicate = "strip") %>% 
      akima::interp2xyz() %>% 
      as_tibble() 
  })
  
  water.bottom = reactive({
    insitu.data  %>% 
      filter(variable == input$varwaterId & season == input$waterSeasonId & strata == "B")  %$%
      akima::interp(x = lon,y = lat,z = data, 
                    linear = FALSE,
                    jitter = 200, 
                    jitter.random = TRUE,
                    extrap = FALSE,
                    duplicate = "strip") %>% 
      akima::interp2xyz() %>% 
      as_tibble() 
  })
  
  output$plotSuface = renderPlot({
    
    water.surface() %>%  
      ggplot(aes(x = x, y = y , z = z))+
      metR::geom_contour_fill(bins = 12)+
      metR::geom_contour2(aes(label = ..level..), skip = 1) +
      scale_fill_gradientn(colours = mycolor2, 
                           guide = guide_colorbar(title = expression(paste(PO[4]," ", "(\u00B5g/L)",sep=""))),
                           trans = scales::modulus_trans(p = .1))+
      ggspatial::layer_spatial(data = pemba.sf, fill = "#bea896", col = 1)+
      coord_sf(xlim = c(39.0, 39.4), ylim = c(-5.45,-5.14))+
      scale_y_continuous(breaks = c(-5.4, -5.2), labels = metR::LatLabel(c(-5.4,-5.2)))+
      scale_x_continuous(breaks = c(39.05, 39.35), labels = metR::LonLabel(c(39.1,39.4)))+
      theme_bw()+
      theme(axis.title = element_blank(), legend.position = "none" )+
      labs(title = paste("The isofurface of", input$varwaterId, "sampled 2 cm from the surface"))
    
  })
  
  output$tableSurface = renderDataTable({
    
    water.surface() %>% 
      filter(!is.na(z)) %>%   
      mutate(across(is.numeric, round, 3)) %>% 
      rename(Longitude = x, Latitude = y, input = z)%>% 
      # brushedPoints(brush = input$brushSurface) %>%
      slice(1:10) %>% 
      DT::datatable(
        rownames = NA,
        extensions = "Buttons", 
        options = list(paging = TRUE,
                       scrollX=TRUE, 
                       searching = TRUE,
                       ordering = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       pageLength=5, 
                       lengthMenu=c(3,5,10)))
    
  })
  
  output$plotMiddle = renderPlot({
    
    water.middle() %>%  
      ggplot(aes(x = x, y = y , z = z))+
      metR::geom_contour_fill(bins = 12)+
      metR::geom_contour2(aes(label = ..level..), skip = 1) +
      scale_fill_gradientn(colours = mycolor2, 
                           guide = guide_colorbar(title = expression(paste(PO[4]," ", "(\u00B5g/L)",sep=""))),
                           trans = scales::modulus_trans(p = .1))+
      ggspatial::layer_spatial(data = pemba.sf, fill = "#bea896", col = 1)+
      coord_sf(xlim = c(39.0, 39.4), ylim = c(-5.45,-5.14))+
      scale_y_continuous(breaks = c(-5.4, -5.2), labels = metR::LatLabel(c(-5.4,-5.2)))+
      scale_x_continuous(breaks = c(39.05, 39.35), labels = metR::LonLabel(c(39.1,39.4)))+
      theme_bw()+
      theme(axis.title = element_blank(), legend.position = "none" )+
      labs(title = paste("The isofurface of", input$varwaterId, "sampled 15m from the surface"))
    
  })
  
  output$tableMiddle = renderDataTable({
    
    water.middle() %>% 
      filter(!is.na(z)) %>% 
      mutate(across(is.numeric, round, 3)) %>% 
      rename(Longitude = x, Latitude = y, input = z)%>% 
      # brushedPoints(brush = input$brushMiddle) %>%
      slice(1:10) %>% 
      DT::datatable(
        rownames = NA,
        extensions = "Buttons", 
        options = list(paging = TRUE,
                       scrollX=TRUE, 
                       searching = TRUE,
                       ordering = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       pageLength=5, 
                       lengthMenu=c(3,5,10)))
    
  })
  
  output$plotBottom = renderPlot({
    
    water.bottom() %>%  
      ggplot(aes(x = x, y = y , z = z))+
      metR::geom_contour_fill(bins = 12)+
      metR::geom_contour2(aes(label = ..level..), skip = 1) +
      scale_fill_gradientn(colours = mycolor2, 
                           guide = guide_colorbar(title = expression(paste(PO[4]," ", "(\u00B5g/L)",sep=""))),
                           trans = scales::modulus_trans(p = .1))+
      ggspatial::layer_spatial(data = pemba.sf, fill = "#bea896", col = 1)+
      coord_sf(xlim = c(39.0, 39.4), ylim = c(-5.45,-5.14))+
      scale_y_continuous(breaks = c(-5.4, -5.2), labels = metR::LatLabel(c(-5.4,-5.2)))+
      scale_x_continuous(breaks = c(39.05, 39.35), labels = metR::LonLabel(c(39.1,39.4)))+
      theme_bw()+
      theme(axis.title = element_blank(), legend.position = "none" )+
      labs(title = paste("The isofurface of", input$varwaterId, "sampled 30m from the surface"))
    
  })
  
  output$tableBottom = renderDataTable({
    
    water.bottom() %>% 
      filter(!is.na(z)) %>% 
      mutate(across(is.numeric, round, 3)) %>% 
      rename(Longitude = x, Latitude = y, input = z) %>% 
      # brushedPoints(brush = input$brushBottom) %>%
      slice(1:10) %>% 
      DT::datatable(
        rownames = NA,
        extensions = "Buttons", 
        options = list(paging = TRUE,
                       scrollX=TRUE, 
                       searching = TRUE,
                       ordering = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       pageLength=5, 
                       lengthMenu=c(3,5,10)
        )
      )
  })
  expd.selected = reactive({
    expeditions %>% 
    filter(program == input$expedId &
             transect==input$transectId &  
             variable == input$variableId & 
             between(pressure,input$depthId[1],input$depthId[2])
    ) %>% 
    select(time,lon,lat, pressure, transect, variable, data) 
  
  })
  
  
  output$sectionPlot = renderPlot({
    
    make.contour = expeditions %>% 
      filter(program == input$expedId,transect==input$transectId &  variable == input$variableId) %>% 
      pull(data) %>% 
      quantile(na.rm = TRUE) %>% 
      unique() %>% round(2)
    
    expd.selected() %>% 
      filter(data >=0) %>% 
      ggplot(aes(x = lon, y = pressure, z = data))+
      metR::geom_contour_fill(bins = 120, kriging = FALSE)+
      metR::geom_contour2(aes(label = stat(level)), breaks = make.contour, skip = 0)+
      scale_y_reverse(expand = c(0,NA), name = "Pressure [m]") + 
      scale_fill_gradientn(colours = drawing.colors[[zzz %>% filter(var ==input$variableId) %>% pull(number)]], 
                           trans = scales::boxcox_trans(p = 0.1),
                           guide = guide_colorbar(title = "Values", 
                                                  reverse = TRUE, 
                                                  breaks = scales::pretty_breaks(n = 8),
                                                  title.position = "top", 
                                                  label.position = "right",
                                                  title.hjust = .5,
                                                  title.theme = element_text(angle = 0), 
                                                  barwidth = unit(.6, "cm"),
                                                  barheight = unit(8, "cm")))+
      metR::scale_x_longitude(breaks = scales::breaks_pretty(n = 3))+
      theme_bw(base_size = 14) + 
      theme(legend.position = "right")+
      metR::scale_x_longitude(ticks = 0.2)+
      labs(subtitle = paste("The",input$variableId, "section"))
    
  })
  
  output$table1 = renderDataTable({
    
    expd.selected() %>% 
      mutate(variable = str_to_title(variable),
             date = lubridate::as_date(time),
             hour = lubridate::hour(time), 
             across(is.numeric, round, 4)) %>% 
      relocate(c(variable,date, hour, transect), .before = lon) %>% 
      select(-time, -transect) %>% 
      filter(pressure %in% seq(0,1000,50)) %>% 
      slice(1:10) %>% 
      DT::datatable(colnames = c("Variable","Date", "Hour", "Lon", "Lat","Pressure", "Data"), 
                    rownames = NA,
                    extensions = "Buttons", 
                    options = list(paging = TRUE,
                                   scrollX=TRUE, 
                                   searching = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf'),
                                   pageLength=5, 
                                   lengthMenu=c(3,5,10)
                    )
      )
    
  })
  
  
  
  ## local CTD ----
  ctd.interp = reactive({
    ctd.all %>% 
      pivot_longer(cols = temperature:salinity, 
                   values_to = "data", 
                   names_to = "variable") %>%
      filter(variable == input$zvar1 & 
               program == local.expeditions[1] & 
               pressure ==input$depthId1 & 
               season == input$seasonId1) %$%
      akima::interp(x = lon, y = lat, z = data, nx = 100, ny = 100) %>% 
      akima::interp2xyz()%>% 
      as_tibble() %>% 
      mutate(across(is.numeric, round, 2))
  })
  
  
  ctd.interp2 = reactive({
    ctd.all %>% 
      pivot_longer(cols = temperature:salinity, 
                   values_to = "data", 
                   names_to = "variable") %>%
      filter(variable == input$zvar1 & 
               program == local.expeditions[1] & 
               pressure ==input$depthId2 & 
               season == input$seasonId1) %$%
      akima::interp(x = lon, y = lat, z = data, nx = 100, ny = 100) %>% 
      akima::interp2xyz()%>% 
      as_tibble() %>% 
      mutate(across(is.numeric, round, 2))
  })
  
  
  output$plot2 = renderPlot({
    ctd.interp()  %>% 
      ggplot(aes(x = x, y = y, z = z))+
      metR::geom_contour_fill(kriging = FALSE)+
      metR::geom_contour2(aes(label = stat(level)), skip = 1, label.placer = label_placer_n(n = 1))+
      scale_fill_gradientn(colours = drawing.colors[[zzz %>% filter(var ==input$zvar1) %>% pull(number)]], 
                           trans = scales::boxcox_trans(p = 0.1),
                           guide = guide_colorbar(title = "Values", 
                                                  reverse = TRUE, 
                                                  breaks = scales::pretty_breaks(n = 8),
                                                  title.position = "top", 
                                                  label.position = "right",
                                                  title.hjust = .5,
                                                  title.theme = element_text(angle = 0), 
                                                  barwidth = unit(.6, "cm"),
                                                  barheight = unit(8, "cm")))+
      metR::scale_x_longitude(ticks = .15, position = "top")+
      metR::scale_y_latitude(ticks = .06)
  })
  
  output$plot3 = renderPlot({
    ctd.interp2() %>% 
      ggplot(aes(x = x, y = y, z = z))+
      metR::geom_contour_fill(kriging = FALSE)+
      metR::geom_contour2(aes(label = stat(level)), skip = 1, label.placer = label_placer_n(n = 1))+
      scale_fill_gradientn(colours = drawing.colors[[zzz %>% filter(var ==input$zvar1) %>% pull(number)]], 
                           trans = scales::boxcox_trans(p = 0.1),
                           guide = guide_colorbar(title = "Values", 
                                                  reverse = TRUE, 
                                                  breaks = scales::pretty_breaks(n = 8),
                                                  title.position = "top", 
                                                  label.position = "right",
                                                  title.hjust = .5,
                                                  title.theme = element_text(angle = 0), 
                                                  barwidth = unit(.6, "cm"),
                                                  barheight = unit(8, "cm")))+
      metR::scale_x_longitude(ticks = .15, position = "top")+
      metR::scale_y_latitude(ticks = .06)
  })
  
  output$mapIso = renderTmap({
    
    ctd.interp() %>% 
      raster::rasterFromXYZ(crs = 4326, digits = 2) %>% 
      tm_shape() +
      tm_raster(n = 6, style = "kmeans")
  })
  
  output$profileId = renderPlot({
    ctd.all %>% 
      pivot_longer(cols = temperature:salinity, 
                   values_to = "data", 
                   names_to = "variable") %>%
      filter(variable == input$zvar1 & 
               program == local.expeditions[1] & 
               # pressure ==input$depthId1 & 
               season == input$seasonId1) %>% 
      group_by(pressure) %>% 
      summarise(data = median(data, na.rm = TRUE)) %>% 
      ungroup() %>% 
      ggplot(aes(x = data, y = pressure)) +
      geom_path()+
      scale_y_reverse(name = "Pressure [m]", breaks = seq(0,150,20))+
      scale_x_continuous(name = input$zvar1 %>% str_to_title() ,
                         position = "top", breaks = scales::pretty_breaks(n = 3))+
      geom_hline(yintercept = c(input$depthId1, input$depthId2), linetype = 2, size = 1.2, color = "red")
    
  })
  
  output$sapphireTable = renderDataTable({
    ctd.all %>% 
      pivot_longer(cols = temperature:salinity, 
                   values_to = "data", 
                   names_to = "variable") %>%
      filter(variable == input$zvar1 & 
               program == local.expeditions[1] & 
               # pressure ==input$depthId1 & 
               season == input$seasonId1) %>% 
      mutate(across(is.numeric, round, 4)) %>% 
      slice(1:10) %>% 
      DT::datatable(
        # colnames = c("Variable","Date", "Hour", "Lon", "Lat","Pressure", "Data"), 
                    rownames = NA,
                    extensions = "Buttons", 
                    options = list(paging = TRUE,
                                   scrollX=TRUE, 
                                   searching = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf'),
                                   pageLength=5, 
                                   lengthMenu=c(3,5,10)
                    )
      )
    
    
  })
  
  ## River flow ----
  
  river.selected = reactive({
    rivers %>% 
      filter(river %in% input$riverId) %>% 
      filter(!is.na(date) & date > input$flowDate[1] & date < input$flowDate[2]) %>% 
      # glimpse()
      mutate(date = lubridate::as_date(date)) 
  })
  
  
  output$riverDischarge = renderPlot({
    
    river.selected() %>% 
      ggplot(aes(x = date, y = flow))+
      geom_line()+
      geom_smooth(method = "lm", color = "red", fill = "red", alpha =.2)+
      theme(axis.title.x = element_blank())+
      scale_x_date(labels = scales::label_date_short())+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 4), name = expression(Discharge~(m^{-3}~s^{-1})))
    
  })
  
  output$selectedRiversData = renderDataTable({
    
    river.selected() %>% 
      brushedPoints(brush = input$plot_brush) %>%
      slice(1:10) %>% 
      DT::datatable(
        # colnames = c("Variable","Date", "Hour", "Lon", "Lat","Pressure", "Data"), 
        rownames = NA,
        extensions = "Buttons", 
        options = list(paging = TRUE,
                       scrollX=TRUE, 
                       searching = TRUE,
                       ordering = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       pageLength=5, 
                       lengthMenu=c(3,5,10)
        )
      )
  })
  
  ## developer ----
  output$member14 = renderImage({
    list(src = "img/semba.png",
         contentType = 'image/png', height = 180,
         alt = "Semba")
  }, deleteFile = FALSE)
  
  
  ## acknowledgement ---- 
  output$logo1 = renderImage({
    list(src = "img/coat.png",
         contentType = 'image/png', height = 140,
         alt = "Coat of arm")
  }, deleteFile = FALSE)
  
  output$logo2 = renderImage({
    list(src = "img/Nairobi_Convention_Logo.jpg",
         contentType = 'image/jpg', height = 140,
         alt = "Nairobi Convention")
  }, deleteFile = FALSE)
  
  output$logo3 = renderImage({
    list(src = "img/unep-vector-logo.png",
         contentType = 'image/png', height = 140,
         alt = "UNEP")
  }, deleteFile = FALSE)
  
  output$logo4 = renderImage({
    list(src = "img/udsm_logo.png",
         contentType = 'image/png', height = 140,
         alt = "University of Dar es Salaam")
  }, deleteFile = FALSE)
  
  output$logo5 = renderImage({
    list(src = "img/wiomsa.png",
         contentType = 'image/png', height = 140,
         alt = "Western Indian Ocean Marine Science Association")
  }, deleteFile = FALSE)
  
  
  
  ## inforboxes ----
  output$casts = renderInfoBox({
    infoBox(title = "Casts",
            value = 178, 
            subtitle = "Number of casts",
            icon = icon("arrow-up"), 
            fill = TRUE, 
            color = "green"
    )
  })
  
  
  output$locals = renderInfoBox({
    infoBox(title = "Locals",
            value = 178, 
            subtitle = "Number of casts",
            icon = icon("angle-double-right"), 
            fill = TRUE, 
            color = "red"
    )
  })
  
  output$expeditions = renderInfoBox({
    infoBox(title = "Expedition",
            value = "Two", 
            subtitle = "Algoa & Agulhas",
            icon = icon("globe"), 
            fill = TRUE, 
            color = "maroon"
    )
  })
  
  output$zones = renderInfoBox({
    infoBox(title = "Zones",
            value = "Terr & EEZ", 
            subtitle = "The Zone Sampled",
            icon = icon("globe"), 
            color = "orange", 
            fill = TRUE)
  })
  
  output$seasons = renderInfoBox({
    infoBox(title = "Seasons",
            value = "NE and SE", 
            subtitle = "Monsoon seasons",
            icon = icon("globe"), 
            fill = TRUE)
    
  })
  
}



shinyApp(ui = ui, server = server)
