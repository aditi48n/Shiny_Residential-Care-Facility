#Import required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(leaflet)
library(RColorBrewer)


#Read in the raw data file and further process data columns
raw = read.csv("data/gsr_residential_care_raw.csv")
data = as.data.frame(raw[,-(12:23)])
options = as.vector(unique(data$CITY))

ui <- fluidPage(
                  div(class = "logo"),
#Feature 1: Added a CSS file to make App look nice by changing background, font colour and style. 
                  includeCSS("www/style.css"),
#Reference: https://stackoverflow.com/questions/24652658/suppress-warning-message-in-r-console-of-shiny/40980103
                  tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                            ),
                  titlePanel(p("Residential care facilities in British Columbia, Canada", style = "color:#121151")),
                  sidebarLayout(
                                  sidebarPanel(width = 4,
                                  h4("Looking for residential care facilities?", tags$br(),
                                  "This application provides information currently displayed on the Residential Care Programs Map to assist you pick the right kind of facility.", tags$br(), tags$br(),
                                  "Use the filters below to make your selection!", style = "color:0a0a0a"),
                                  tags$br(), tags$br(),
#Feature 2: Allowed the user to search for multiple entries simultaneously by adding both checkboxGroupInput and multiple = TRUE in selectInput since the user may be interested in more than one type of facility across different cities in BC.
                                  checkboxGroupInput('typeInput', 'Facility type ', choices = c("Community Living", "Mental Health", "Long Term Care", "Acquired Injury", "Hospice", "Residential Care"), 
                                                    selected = "Community Living"),
                                  tags$br(), 
                                  selectInput("cityInput", "City", choice = options, selected = "Vancouver", multiple = TRUE),
                                  tags$br(),
                                  p("Data obtained from", a("OpenDataBC",
                                  href = "https://catalogue.data.gov.bc.ca/dataset/residential-care-facilities")) ,img(src = "BC.png", width = "150px", height = "70px"),
                                  tags$br(), tags$br(),
                                  em(
                                      span("Created by", a("Aditi Nagaraj Nallan")),
                                      HTML("&bull;"),
                                      span("Code available", a(href = "https://github.com/aditi48n/Shiny_Residential-Care-Facility", "on GitHub"))
                                    )
                              ),
                  mainPanel(
                      tabsetPanel(
                      tabPanel("Map", 
                          h3(textOutput("summaryText")), 
                          leafletOutput("plot", width = "100%"), 
                          DT::DTOutput(outputId = "table"),
                          downloadButton('downloadData', 'Download Table', style="color: #fff; background-color: navy; border-color: Black;")
                              )
                                    )
                                          )

                ))

server <- function(input, output) {
  selected_point <- reactiveVal(NULL)
                                    filtered = reactive({
                                                          data %>%
                                                          filter(TYPE %in% input$typeInput,
                                                          CITY %in% input$cityInput)
                                                        })
                                    # Define a color palette
                                    colors <- brewer.pal(6, "Set2")
                                    color_palette = colorFactor(colors, 
                                                                 levels = c("Community Living", "Mental Health", "Long Term Care", "Acquired Injury", "Hospice", "Residential Care"))
 #Feature 3: Incorporated R leaflet package to make maps more interactive within the application.                                    
                                    output$plot = renderLeaflet({
                                      leaflet(data = filtered()) %>% 
                                        addProviderTiles(providers$OpenStreetMap) %>% 
                                        addCircleMarkers(
                                          ~LONGITUDE, 
                                          ~LATITUDE, 
                                          color = ~color_palette(TYPE), 
                                          radius = 5, 
                                          label = ~paste(BUSINESS_NAME, TYPE, sep=" Type-"),
                                          popup = ~BUSINESS_NAME
                                        ) 
                                      })
                                    
  
#Feature 4: Using the DT package turned a static table into an interactive table for better user experience. 
                                    output$table = DT::renderDT({
                                      # Check if a point has been selected
                                      selected_data = selected_point()
                                      
                                      # If no point is selected, show all entries
                                      if(is.null(selected_data)) {
                                        data_to_display = filtered()
                                      } else {
                                        data_to_display = selected_data
                                      }
                                      
                                      DT::datatable(data_to_display %>% select(BUSINESS_NAME, STREET_ADDRESS, CITY, POSTAL_CODE, BUSINESS_PHONE))
                                    })
                                    
  
#Feature 5: Added a download button to allow the user to download the filtered data for future reference.
                                    output$downloadData = downloadHandler(
                                                                            filename = function() { 
                                                                                                    paste("filtered", Sys.Date(), ".csv", sep="")
                                                                                                  },
                                                                            content = function(file) {
                                                                                                        filtered()
                                                                                                        write.csv(filtered(), file)
                                                                                                      }
                                                                            )
  
#Feature 6: Output the number of results found whenever the filters changed based on user input. 
                                    output$summaryText = renderText({
                                                                        numOptions = nrow(filtered())
                                                                        if (is.null(numOptions)) {
                                                                        numOptions = 0
                                                                      }
                                                                        paste0("We found ", numOptions, " options for you")
                                                                      }
                                                                     )
                                    observeEvent(input$plot_marker_click, {
                                      click_data = input$plot_marker_click
                                      clicked_point = filtered() %>% 
                                        filter(LONGITUDE == click_data$lng & LATITUDE == click_data$lat)
                                      if(nrow(clicked_point) > 0) {
                                        selected_point(clicked_point)
                                      }
                                    })
  
                                }
shinyApp(ui = ui, server = server)
