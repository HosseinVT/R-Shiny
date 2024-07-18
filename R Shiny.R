# Load packages
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(lubridate)
library(dplyr)

# Define function to clean and load data
clean_and_load_data <- function(url) {
  datafile <- tempfile()
  download.file(url, datafile)
  
  all_lines <- readLines(datafile)
  lines_to_keep <- all_lines[-c(1, 3, 4)]
  
  tempfile_clean <- tempfile()
  writeLines(lines_to_keep, tempfile_clean)
  
  data <- read.table(tempfile_clean, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  data$TIMESTAMP <- ymd_hms(data$TIMESTAMP)
  return(data)
}

# URLs for the data files
file_url1 <- "https://raw.githubusercontent.com/HosseinVT/R-Shiny/main/BMPTripod1_Min_15_Raw.dat"
file_url2 <- "https://raw.githubusercontent.com/HosseinVT/R-Shiny/main/BMPTripod2_Min_15_Raw.dat"

# Load the data
T1data <- clean_and_load_data(file_url1)
T2data <- clean_and_load_data(file_url2)

# Shiny app UI
ui <- page_sidebar(
  title = "StREAM Lab Live Data",
  theme = bs_theme(
    bg = "#33FFF1",
    fg = "#FFF",
    primary = "#E69F00",
    secondary = "#0072B2",
    success = "#009E73",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  
  sidebar = sidebar(
    # Select Tower
    radioButtons("towerID", "Select Tripod:",
                 c("Tripod 1" = "t1" ,
                   "Tripod 2" = "t2")),
    
    # Date range selector
    dateRangeInput("dateRange", 
                   label = "Select Date Range:",
                   start = min(c(T1data$TIMESTAMP, T2data$TIMESTAMP)), 
                   end = max(c(T1data$TIMESTAMP, T2data$TIMESTAMP))),
    
    # Select variable for y-axis
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = colnames(T1data),
      selected = "Conductivity"
    ),
    # Select variable for x-axis
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = colnames(T1data),
      selected = "TIMESTAMP"
    )
  ),
  # Output: Show scatterplot and table
  card(
    tabsetPanel(
      tabPanel("Plot", plotOutput(outputId = "scatterplot")),
      tabPanel("Table", DT::dataTableOutput(outputId = "table"))
    )
  )
)

# Define server
server <- function(input, output, session) {
  selectedData <- reactive({
    if (input$towerID == "t1") {
      T1data
    } else {
      T2data
    }
  })
  
  filteredData <- reactive({
    selectedData() %>%
      filter(TIMESTAMP >= input$dateRange[1] & TIMESTAMP <= input$dateRange[2]) %>%
      select(TIMESTAMP, !!sym(input$y))
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data = filteredData(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "black")
      )
  })
  
  output$table <- DT::renderDataTable({
    filteredData()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
