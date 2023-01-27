library(shiny)
library(shinydashboard)
library(fresh)
library(DT)
library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(terra)
library(sf)
library(ggpubr)
library(highcharter)
library(grid)
library(corrplot)
library(rvest)
library(readxl)
library(tidyr)

# functions
# https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Datasets
MoH_Covid_Case_Counts_Info <- read.csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/covid-case-counts.csv", header = TRUE)
MoH_Covid_Case_Counts_Locations_List <- read_excel_allsheets("Data/covid-cases-counts-location.xlsx")
Stats_Data_Portal_Data <- read_excel("Data/covid_19_data_portal.xlsx", sheet='data')



NZ_Covid_Map_Data_URL <- read_html("https://www.health.govt.nz/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-current-cases")
NZ_Covid_Map_Data_Table <- NZ_Covid_Map_Data_URL %>% html_nodes("table") %>% .[6] %>% html_table(fill = TRUE)
NZ_Covid_Map_Data <- data.frame(NZ_Covid_Map_Data_Table[[1]])

# Reading in the shape file (note: I have converted it from json to shape using sf package)
# https://github.com/deldersveld/topojson/tree/master/countries/new-zealand
nz_spdf <- readOGR( 
  dsn= paste0(getwd(),"/nz-district-health-boards-locations/") , 
  layer="nz-district-health-boards-2012",
  verbose=FALSE
)

# Fixing District Names to match Covid Data by Districts
nz_spdf@data$NAME[nz_spdf@data$NAME == "Hutt"] = "Hutt Valley"
nz_spdf@data$NAME[nz_spdf@data$NAME == "Midcentral"] = "Mid Central"
nz_spdf@data$NAME[nz_spdf@data$NAME == "Tairawhiti"] = "Tairāwhiti"
nz_spdf@data$NAME[nz_spdf@data$NAME == "Waitemata"] = "Waitematā"

# Fixing the Covid Case Count Location Datasets
MoH_Covid_Case_Counts_Locations <- bind_rows(MoH_Covid_Case_Counts_Locations_List, .id = "Location")
# Renaming Column from Report_Date to Date
colnames(MoH_Covid_Case_Counts_Locations)[2] = "Date"
# Remove "Capital, Coast & Hutt", "Canterbury & West Coast"
MoH_Covid_Case_Counts_Locations <- MoH_Covid_Case_Counts_Locations %>%
  filter(Location != "Capital, Coast & Hutt" & Location != "Canterbury & West Coast")
# Rename "Capital and Coast only" -> "Capital and Coast", "Hutt Valley only" -> "Hutt Valley", 
#         "Canterbury only" -> "Canterbury",  "West Coast only" -> "West Coast"
MoH_Covid_Case_Counts_Locations$Location[MoH_Covid_Case_Counts_Locations$Location == "Capital and Coast only"] = "Capital and Coast"
MoH_Covid_Case_Counts_Locations$Location[MoH_Covid_Case_Counts_Locations$Location == "Hutt Valley only"] = "Hutt Valley"
MoH_Covid_Case_Counts_Locations$Location[MoH_Covid_Case_Counts_Locations$Location == "Canterbury only"] = "Canterbury"
MoH_Covid_Case_Counts_Locations$Location[MoH_Covid_Case_Counts_Locations$Location == "West Coast only"] = "West Coast"

# Daily Covid Case Counts since First Case
MoH_Covid_Case_Counts_Daily_Totals <- MoH_Covid_Case_Counts_Locations %>% filter(Location == "Total")
MoH_Covid_Case_Counts_Daily_Totals <- MoH_Covid_Case_Counts_Daily_Totals[order(as.Date(MoH_Covid_Case_Counts_Daily_Totals$Date, format = "%Y-%m-%d")), ]
MoH_Covid_Case_Counts_Daily_Totals <- head(MoH_Covid_Case_Counts_Daily_Totals, - 1)              # Removing the last row which is a total row


# Daily Counts of Recovered and Deceased
Stats_Covid_Case_Counts_ARD <- filter(Stats_Data_Portal_Data, Stats_Data_Portal_Data$ResourceID == "CPCOV2")
Stats_Covid_Case_Counts_ARD <- subset(Stats_Covid_Case_Counts_ARD, select = -c(ResourceID,Geo,Label2,Label3,Unit,Measure,Multiplier))

Stats_Covid_Case_Counts_ARD <- Stats_Covid_Case_Counts_ARD %>% 
  rename("Date" = "Period",
         "Case_Status" = "Label1",
         "Cases_Count" = "Value"
  )

Covid_Deceased <- Stats_Covid_Case_Counts_ARD %>% filter(Stats_Covid_Case_Counts_ARD$Case_Status == "Deceased")
Covid_Recovered <- Stats_Covid_Case_Counts_ARD %>% filter(Stats_Covid_Case_Counts_ARD$Case_Status == "Recovered")

Timeline_Covid_Case_Counts <- MoH_Covid_Case_Counts_Daily_Totals %>%
  left_join(Covid_Recovered, by='Date') %>%
  left_join(Covid_Deceased, by='Date')

Covid_Reported_Cases <- Timeline_Covid_Case_Counts$Cases
Covid_Recovered_Cases <- Timeline_Covid_Case_Counts$Cases_Count.x
Covid_Deceased_Cases <- Timeline_Covid_Case_Counts$Cases_Count.y

Timeline_Covid_Case_Counts <- data.frame(Timeline_Covid_Case_Counts$Date, Covid_Reported_Cases, Covid_Recovered_Cases, Covid_Deceased_Cases)

Timeline_Covid_Case_Counts <- Timeline_Covid_Case_Counts %>% 
  rename("Date" = "Timeline_Covid_Case_Counts.Date",
         "Reported" = "Covid_Reported_Cases",
         "Recovered" = "Covid_Recovered_Cases",
         "Deceased" = "Covid_Deceased_Cases"
  )

Timeline_Covid_Case_Counts$Total <- cumsum(Timeline_Covid_Case_Counts$Reported)

# Fill NA's
Timeline_Covid_Case_Counts <- fill_(Timeline_Covid_Case_Counts, names(Timeline_Covid_Case_Counts))

# Calculating Active Cases
# Timeline_Covid_Case_Counts$Active <- (Timeline_Covid_Case_Counts$Total_Cases - Timeline_Covid_Case_Counts$Deceased - Timeline_Covid_Case_Counts$Recovered)

# Timeline_Covid_Case_NZ <- gather(Timeline_Covid_Case_Counts, Case_Status, Case_Count, Reported:Active, factor_key = TRUE)
Timeline_Covid_Case_NZ <- gather(Timeline_Covid_Case_Counts, Case_Status, Case_Count, Reported:Total, factor_key = TRUE)




# Covid Vaccine Data
Stats_Covid_Vaccine_Data <- filter(Stats_Data_Portal_Data, Stats_Data_Portal_Data$ResourceID == "CPCOV9")

Covid_Vaccine_Data <- subset(Stats_Covid_Vaccine_Data, select = -c(ResourceID,Geo,Label2,Label3,Unit,Measure,Multiplier))

Covid_Vaccine_Data <- Covid_Vaccine_Data %>% 
  rename("Date" = "Period",
         "Vaccine_Administered" = "Label1",
         "Vaccine_Count" = "Value"
  )




# Leaflet NZ Map with NZ Covid Data merge with it.
NZ_Covid_Map <- sp::merge(nz_spdf, NZ_Covid_Map_Data, by.x="NAME", by.y="Location", all.x=TRUE, duplicateGeoms = TRUE) 

# Saving the Total of each Column (Active, Recovered...) in the NZ Covid Map Data 
NZ_Covid_Map_Data_Total <- NZ_Covid_Map_Data %>% filter(Location == "Total")

header <- dashboardHeader(
  title = "New Zealand Covid-19 Dashboard",
  dropdownMenu(badgeStatus = NULL, icon = icon("link"), headerText = "Sources to the Data and My Github Repository.",
               messageItem(
                 from = "Ministry of Health NZ Covid Data",
                 message = "Link to MoH Covid Data Repository",
                 icon = icon("database"),
                 href = "https://github.com/minhealthnz/nz-covid-data"
               ),
               messageItem(
                 from = "Stats NZ Data Portal",
                 message = "Link to Stats NZ Data Portal",
                 icon = icon("database"),
                 href = "https://www.stats.govt.nz/experimental/covid-19-data-portal/"
               ),
               messageItem(
                 from = "My Github Repository",
                 message = "Link to my code for the Shiny App",
                 icon = icon("github"),
                 href = "https://github.com/Ana-ishSodhi/R-Shiny-NZ-Covid-19-Dashboard"
               )
  ),
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header .logo {width: 400px; text-align: left; padding-left: 15px; padding-right: 0px;}
                       .main-header .navbar {margin-left: 330px;}"
            )
            
    )
  
)

sidebar <- dashboardSidebar(
  disable = TRUE
)

body <- dashboardBody(
  use_theme(create_theme(
    adminlte_global(
      content_bg = "#353c42"
    )
  )),
  
  h4("Please wait 10 seconds for everything to load.", align = "center", style = "color:white"),
  h5("Warning: Data may be outdated.", align = "center", style = "color:white"),
  
  fluidRow(
    valueBoxOutput("Total_Cases", width = 3),
    valueBoxOutput("Active_Cases", width = 3),
    valueBoxOutput("Recovered_Cases", width = 3),
    valueBoxOutput("Deceased_Cases", width = 3)
  ),

  fluidRow(
    column(width = 6,
           box(
               title = "Covid Cases Timeline", status = "primary", solidHeader = TRUE,
               collapsible = FALSE, width = NULL,
               highchartOutput("Covid_Timeline_NZ", width = "800px", height = "333px")
           ),
           
           box(
             title = "Covid Vaccine Timeline", status = "primary", solidHeader = TRUE,
             collapsible = FALSE, width = NULL,
             highchartOutput("Covid_Vaccine_Timeline_NZ", width = "800px", height = "333px")
           )
    ),
    column(width = 6,
           box(
             title = "Map of New Zealand", status = "primary", solidHeader = TRUE,
             collapsible = FALSE, width = NULL,
             leafletOutput("Map", width = "100%", height="750px")
           )
    )
  ),
  
  
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  header,
  sidebar,
  body
)


server <- function(input, output) {
  
  
  # Number of Total Cases Display
  output$Total_Cases <- renderValueBox({
    valueBox(
      value = format(NZ_Covid_Map_Data_Total$Total, big.mark = ","),
      subtitle = "The Total Number of Cases",
      color = "blue"
    )
  })
  
  # Number of Active Cases Display
  output$Active_Cases <- renderValueBox({
    valueBox(
      value = format(NZ_Covid_Map_Data_Total$Active, big.mark = ","),
      subtitle = "The Number of Active Cases",
      color = "yellow"
    )
  })
  
  # Number of Recovered Cases Display
  output$Recovered_Cases <- renderValueBox({
    valueBox(
      value = format(NZ_Covid_Map_Data_Total$Recovered, big.mark = ","),
      subtitle = "The Total Number of Recovered",
      color = "green"
    )
  })
  
  # Number of Deceased Cases Display
  output$Deceased_Cases <- renderValueBox({
    valueBox(
      value = format(NZ_Covid_Map_Data_Total$Deceased, big.mark = ","),
      subtitle = "The Total Number of Deceased",
      color = "red"
    )
  })
  
  # Time Series Covid Timeline
  output$Covid_Timeline_NZ <- renderHighchart({
    
    Timeline_Covid_Case_NZ %>% 
      hchart('line', 
              hcaes(x = as.Date(Date), y = Case_Count, group = Case_Status, color = c(Total_Cases = "blue", Active_Cases = "orange", Recovered_Cases = "green", Deceased_Cases = "red")[Case_Status])
             ) %>%
      hc_title(
        text = "Covid Cases Timeline in New Zealand",
        style = list(fontWeight = "bold", fontSize = "15px"),
        align = "center"
      ) %>%
      hc_xAxis(title = list(text = "Dates"), fontSize = "10px") %>%
      hc_yAxis(title = list(text = "No. of Covid-19 Cases"), fontSize = "10px") %>%
      hc_legend(
        align = "right",
        verticalAlign ="middle",
        layout = "vertical"
      ) %>%
      hc_colors(colors=c("blue","orange","green","red"))
    
  })
  
  
  # Time Series Covid Vaccine Timeline
  output$Covid_Vaccine_Timeline_NZ <- renderHighchart({
    
    Covid_Vaccine_Data %>% 
      hchart('line', hcaes(x = as.Date(Date), y = Vaccine_Count, group = Vaccine_Administered)) %>%
      hc_title(
        text = "Covid Vaccine Timeline in New Zealand",
        style = list(fontWeight = "bold", fontSize = "15px"),
        align = "center"
      ) %>%
      hc_xAxis(title = list(text = "Dates"), fontSize = "10px") %>%
      hc_yAxis(title = list(text = "No. of Vaccination Administered"), fontSize = "10px")
    
  })
  
  
  
  # NZ Covid Map
  output$Map <- renderLeaflet({
    
    # Leaflet Tooltips:
    mytext <- paste(
      "District: ", NZ_Covid_Map@data$NAME,"<br/>", 
      "Active Cases: ", NZ_Covid_Map@data$Active,"<br/>",
      "New Cases in Last Week: ", NZ_Covid_Map@data$"New.cases.in.the.last.week","<br/>",
      "Recovered Cases: ", NZ_Covid_Map@data$Recovered,"<br/>",
      "Deceased Cases: ", NZ_Covid_Map@data$Deceased,"<br/>",
      "Total Cases: ", NZ_Covid_Map@data$Total,
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(nz_spdf) %>% 
      addTiles(
        options = providerTileOptions(minZoom = 5, maxZoom = 9)
      )  %>% 
      setView(lat= -41, lng= 173 , zoom= 5) %>% #sets default map pan
      setMaxBounds(lat1 = -11, lng1 = 143, #stops the map being dragged out of bounds
                   lat2 = -71, lng2 = 203) %>%
      addPolygons( 
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        ),
        
        highlight = highlightOptions(
          weight = 1, #line width
          color = "black",
          fillOpacity = 1.0,
          opacity = 1.0,
          bringToFront = TRUE)
        
      )
    
  })

  
}

shinyApp(ui = ui, server = server)
