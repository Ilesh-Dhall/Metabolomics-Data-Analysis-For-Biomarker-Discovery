library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(stats)
library(dplyr)

sample_data <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/A_Targeted_Metabolomics-Based_Assay_Using_Human_Induced_Pluripotent_Stem_Cell-Derived_Cardiomyocytes_rawdata.csv")

ui <- page_navbar(
    title = "Metabolomics Data Analysis For Biomarker Discovery",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#1a1818",
        secondary = "#6c757d",
        font_scale = 1
    ),
    nav_spacer(),
    nav_panel(
    title = "Data Viewer",
    page_sidebar(
      # Sidebar for Data Viewer
      sidebar = sidebar(
        title = "Data Selection",
        selectInput(
          "columns",
          "Select Columns:",
          choices = names(sample_data),
          multiple = TRUE,
          selected = names(sample_data)
        ),
        numericInput(
          "row_start",
          "Start Row:",
          1,
          min = 1,
          max = nrow(sample_data)
        ),
        numericInput(
          "row_end",
          "End Row:",
          22984,
          min = 1,
          max = nrow(sample_data)
        )
      ),
      # Main content for Data Viewer
      card(
        card_header("Data Table"),
        DTOutput("data_table")
      )
    )
  )
)

server <- function(input, output) {
    # Data Viewer Tab
  filtered_data <- reactive({
    sample_data[input$row_start:input$row_end, input$columns, drop = FALSE]
  })

  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}
shinyApp(ui = ui, server = server)