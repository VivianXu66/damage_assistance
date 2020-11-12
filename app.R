
library(shiny)
library(readr)
library(knitr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(geojsonio)
library(stringr)
library(leaflet)
library(tigris)
options(tigris_use_cache = TRUE)
options(timeout = 40000000000000000000000000)

disaster <- read.csv("hurricane.csv")
disaster %<>% select(-1)
hurricane <- read.csv("hurricane_1.csv")
geo <- geojsonio::geojson_read("gz_2010_us_050_00_500k.json",what="sp")
geo$GEO_ID <- substr(geo$GEO_ID,10,14)
test <- hurricane %>% group_by(GEO_ID,damageCategory) %>% summarise(Count=n())
test_1 <- hurricane %>% group_by(GEO_ID,damageCategory) %>% summarise(totalproject=sum(projectAmount))
test_2 <- hurricane %>% group_by(GEO_ID,damageCategory) %>% summarise(totalfederal=sum(federalShareObligated))

ui <- fluidPage(
    title = "Hurricane damage and assistance",
    sidebarLayout(
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "disaster"')
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Public Assistance Funded Projects Details about Hurricane",
                         fluidRow( column(4,
                                          selectInput("year",
                                                      "Year:",
                                                      c("All",
                                                        unique(disaster$year)))
                         )
                         ),
                         DT::dataTableOutput("table1")
                ),
                tabPanel("The Information of Hurricane per County Mapping",
                         leafletOutput("map", width="100%", height="100%"),
                         fluidRow(
                             column(4,
                                    selectInput("category",
                                                "Category:",
                                                unique(as.character(test$damageCategory))))
                         ),
                         leafletOutput("plot1"),
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 330, height = "auto"
                         )
                )
                
                
                
            )
        )
    )
)


server <- function(input, output) {
    
    output$table1 <- DT::renderDataTable(DT::datatable({
        data <- disaster
        if (input$year != "All") {
            data <- data[disaster$year == input$year,]
        }
        data
    }))
    output$plot1 <- renderLeaflet({
        count <- test %>% filter(damageCategory==input$category)
        countleaf <- geo_join(geo, count, by = c("GEO_ID"), how = "inner")
        project <- test_1 %>% filter(damageCategory==input$category)
        federal <- test_2 %>% filter(damageCategory==input$category)
        
        pal <- colorNumeric(palette = "Blues", domain = countleaf$Count)
        Encoding( x = countleaf$NAME ) <- "UTF-8"
        countleaf$NAME <-iconv( x =countleaf$NAME, from = "UTF-8", to = "UTF-8", sub = "" )
        popup <- paste0("CountyID: ", countleaf$GEO_ID, "<br>", 
                        "County: ", as.character(countleaf$NAME), "<br>", 
                        "Count of hurricane: ", countleaf$Count, "<br>",
                        "Total project amount: ", project$totalproject, "<br>",
                        "Total federal obligated: ", federal$totalfederal)
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.75)) %>%
            setView(-89.275673, 37.098, zoom = 4) %>%
            addPolygons(data = countleaf, 
                        fillColor = ~pal(Count), 
                        color = "#BDBDC3",
                        fillOpacity = 1, 
                        weight = 1, 
                        smoothFactor = 0.2,
                        popup = popup) %>%
            addLegend(pal = pal, 
                      values = countleaf$Count, 
                      position = "bottomright", 
                      title = "Count of hurricane")
    })
    
}


shinyApp(ui = ui, server = server)
