library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(stats)
library(dplyr)

data <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/A_Targeted_Metabolomics-Based_Assay_Using_Human_Induced_Pluripotent_Stem_Cell-Derived_Cardiomyocytes_rawdata.csv")

data_wide <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/data_wide.csv")
data_wide_selected <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/data_wide_selected_processed.csv")

generateSidebar <- function(title, dataset, id_prefix) {
  sidebar(
    title = title,
    selectInput(
      paste0("columns_", id_prefix),
      "Select Columns:",
      choices = names(dataset),
      multiple = TRUE,
      selected = names(dataset)
    ),
    numericInput(
      paste0("row_start_", id_prefix),
      "Start Row:",
      1,
      min = 1,
      max = nrow(dataset)
    ),
    numericInput(
      paste0("row_end_", id_prefix),
      "End Row:",
      nrow(dataset),
      min = 1,
      max = nrow(dataset)
    ),
    actionButton(paste0("action_", id_prefix), "Apply")
  )
}



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
    navset_card_tab(
      height = 300,
      full_screen = TRUE,
      title = "Tabbed Content",
      nav_panel(
        "Original Dataset",
        layout_sidebar(
          sidebar = generateSidebar("Data Selection", data, "data"),
          card_title("Data Table"),
          DTOutput("data_table")
        )
      ),
      nav_panel(
        "Data Wide",
        layout_sidebar(
          sidebar = generateSidebar("Data Selection", data_wide, "data_wide"),
          card_title("Data Wide Transformation"),
          DTOutput("data_wide")
        )
      ),
      nav_panel(
        "Data Wide Selected",
        layout_sidebar(
          sidebar = generateSidebar("Selected Wide Transformation Features", data_wide_selected, "data_wide_selected"),
          card_title("Selected Wide Transfromation Features"),
          DTOutput("data_wide_selected")
        )
      )
    ),
  )
)

server <- function(input, output) {
  # Reactive for first tab (data)
  filtered_data <- eventReactive(input$action_data, {
    data[
      input[["row_start_data"]]:input[["row_end_data"]],
      input[["columns_data"]],
      drop = FALSE
    ]
  })

  # Reactive for Tab 1 (data_wide)
  filtered_data_wide <- eventReactive(input$action_data_wide, {
    data_wide[
      input[["row_start_data_wide"]]:input[["row_end_data_wide"]],
      input[["columns_data_wide"]],
      drop = FALSE
    ]
  })

  # Reactive for Tab 2 (data_wide_selected)
  filtered_data_wide_selected <- eventReactive(input$action_data_wide_selected, {
    data_wide_selected[
      input[["row_start_data_wide_selected"]]:input[["row_end_data_wide_selected"]],
      input[["columns_data_wide_selected"]],
      drop = FALSE
    ]
  })

  # Outputs
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })

  output$data_wide <- renderDT({
    datatable(filtered_data_wide(), options = list(pageLength = 10))
  })

  output$data_wide_selected <- renderDT({
    datatable(filtered_data_wide_selected(), options = list(pageLength = 10))
  })
}


shinyApp(ui = ui, server = server)
