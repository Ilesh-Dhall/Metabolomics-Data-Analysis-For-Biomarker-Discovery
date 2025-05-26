library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(stats)
library(dplyr)
library(caret)
library(plotly)

data <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/A_Targeted_Metabolomics-Based_Assay_Using_Human_Induced_Pluripotent_Stem_Cell-Derived_Cardiomyocytes_rawdata.csv")

data_wide <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/data_wide.csv")

data_wide_selected <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/data_wide_selected_processed.csv")

# Get feature names from data_wide_selected (excluding 'labels')
feature_names <- names(data_wide_selected)[-which(names(data_wide_selected) == "labels")]

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
          card_title("Selected Wide Transformation Features"),
          DTOutput("data_wide_selected")
        )
      )
    )
  ),
  nav_panel(
    title = "EDA",
    navset_card_tab(
      height = 400,
      full_screen = TRUE,
      title = "Exploratory Data Analysis",
      nav_panel(
        "Visualizations",
        layout_sidebar(
          sidebar = sidebar(
            title = "EDA Controls",
            selectInput(
              "eda_dataset",
              "Select Dataset:",
              choices = c("Original Dataset" = "data", "Data Wide" = "data_wide", "Data Wide Selected" = "data_wide_selected"),
              selected = "data"
            ),
            selectInput(
              "eda_columns",
              "Select Columns:",
              choices = NULL, # Populated reactively in server
              multiple = TRUE
            ),
            selectInput(
              "eda_plot_type",
              "Plot Type:",
              choices = c("Histogram", "Boxplot", "Scatterplot"),
              selected = "Histogram"
            ),
            numericInput(
              "eda_row_start",
              "Start Row:",
              1,
              min = 1
            ),
            numericInput(
              "eda_row_end",
              "End Row:",
              10,
              min = 1
            ),
            actionButton("eda_apply", "Apply")
          ),
          card(
            card_header("Interactive Plot"),
            plotlyOutput("eda_plot", height = "400px")
          )
        )
      ),
      nav_panel(
        "Summary Statistics",
        layout_sidebar(
          sidebar = sidebar(
            title = "Summary Controls",
            selectInput(
              "summary_dataset",
              "Select Dataset:",
              choices = c("Original Dataset" = "data", "Data Wide" = "data_wide", "Data Wide Selected" = "data_wide_selected"),
              selected = "data"
            ),
            selectInput(
              "summary_columns",
              "Select Columns:",
              choices = NULL, # Populated reactively in server
              multiple = TRUE
            ),
            numericInput(
              "summary_row_start",
              "Start Row:",
              1,
              min = 1
            ),
            numericInput(
              "summary_row_end",
              "End Row:",
              10,
              min = 1
            ),
            actionButton("summary_apply", "Apply")
          ),
          card(
            card_header("Summary Statistics"),
            DTOutput("summary_table")
          )
        )
      )
    )
  ),
  nav_panel(
    "Statistical Testing"
  ),
  nav_panel(
    "Predictive Modelling",
    navset_card_tab(
      height = 300,
      full_screen = TRUE,
      title = "Select Machine Learning Model",
      nav_panel(
        "Logistic Regression",
        layout_sidebar(
          sidebar = sidebar(
            title = "Logistic Regression Prediction",
            selectInput("logistic_metabolites", "Select Metabolites:", choices = feature_names, multiple = TRUE),
            uiOutput("logistic_inputs"),
            actionButton("predict_logistic", "Predict")
          ),
          card(
            card_header("Prediction Output"),
            verbatimTextOutput("logistic_prediction"),
            card_body("Explanation: This model uses Logistic Regression with LASSO regularization to classify cardiotoxicity based on selected metabolite intensities.")
          )
        )
      ),
      nav_panel(
        "Random Forest",
        layout_sidebar(
          sidebar = sidebar(
            title = "Random Forest Prediction",
            selectInput("rf_metabolites", "Select Metabolites:", choices = feature_names, multiple = TRUE),
            uiOutput("rf_inputs"),
            actionButton("predict_rf", "Predict")
          ),
          card(
            card_header("Prediction Output"),
            verbatimTextOutput("rf_prediction"),
            card_body("Explanation: This model uses a Random Forest classifier to predict cardiotoxicity based on selected metabolite intensities.")
          )
        )
      ),
      nav_panel(
        "Comparative Analysis",
        layout_sidebar(
          sidebar = sidebar(
            title = "Analysis View",
            selectInput(
              "analysis_view",
              "Select View:",
              choices = c("Info", "Confusion Matrices", "Metrics & ROC"),
              selected = "Info"
            )
          ),
          uiOutput("analysis_content")
        )
      )
    )
  ),
  nav_panel(
    "Biomarker Significance"
  )
)

server <- function(input, output, session) {
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

  # Load Models
  logistic_model <- readRDS("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/models/logistic_regression_model.rds")
  rf_model <- readRDS("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/models/random_forest_model.rds")

  # Get sanitized feature names from model training data
  logistic_sanitized_names <- names(logistic_model$trainingData)[-which(names(logistic_model$trainingData) == ".outcome")]
  rf_sanitized_names <- names(rf_model$trainingData)[-which(names(rf_model$trainingData) == ".outcome")]

  # Dynamic UI for Logistic Regression inputs
  output$logistic_inputs <- renderUI({
    req(input$logistic_metabolites)
    lapply(input$logistic_metabolites, function(met) {
      sanitized_met <- make.names(met)
      numericInput(paste0("logistic_value_", sanitized_met), paste("Value for", met), value = 0, min = 0)
    })
  })

  # Dynamic UI for Random Forest inputs
  output$rf_inputs <- renderUI({
    req(input$rf_metabolites)
    lapply(input$rf_metabolites, function(met) {
      sanitized_met <- make.names(met)
      numericInput(paste0("rf_value_", sanitized_met), paste("Value for", met), value = 0, min = 0)
    })
  })

  # Logistic Regression Prediction
  observeEvent(input$predict_logistic, {
    req(input$logistic_metabolites)
    validate(
      need(length(input$logistic_metabolites) > 0, "Please select at least one metabolite")
    )

    # Create input data frame with sanitized column names
    input_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(logistic_sanitized_names)))
    colnames(input_df) <- logistic_sanitized_names

    # Assign user inputs with validation
    for (met in input$logistic_metabolites) {
      sanitized_met <- make.names(met)
      value <- input[[paste0("logistic_value_", sanitized_met)]]
      validate(
        need(is.numeric(value), paste("Value for", met, "must be numeric")),
        need(value >= 0, paste("Value for", met, "must be non-negative"))
      )
      if (sanitized_met %in% logistic_sanitized_names) {
        input_df[[sanitized_met]] <- as.numeric(value)
      }
    }

    # Predict without preprocessing
    pred <- predict(logistic_model, input_df, type = "prob")

    output$logistic_prediction <- renderPrint({
      cat("Predicted Probabilities:\n")
      for (label in colnames(pred)) {
        cat(label, ":", round(pred[[label]], 4), "\n")
      }
      cat("\n→ Predicted Class:", colnames(pred)[which.max(pred)], "\n")
    })
  })

  # Random Forest Prediction
  observeEvent(input$predict_rf, {
    req(input$rf_metabolites)
    validate(
      need(length(input$rf_metabolites) > 0, "Please select at least one metabolite")
    )

    # Create input data frame with sanitized column names
    input_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(rf_sanitized_names)))
    colnames(input_df) <- rf_sanitized_names

    # Assign user inputs with validation
    for (met in input$rf_metabolites) {
      sanitized_met <- make.names(met)
      value <- input[[paste0("rf_value_", sanitized_met)]]
      validate(
        need(is.numeric(value), paste("Value for", met, "must be numeric")),
        need(value >= 0, paste("Value for", met, "must be non-negative"))
      )
      if (sanitized_met %in% rf_sanitized_names) {
        input_df[[sanitized_met]] <- as.numeric(value)
      }
    }

    # Predict without preprocessing
    pred <- predict(rf_model, input_df, type = "prob")

    output$rf_prediction <- renderPrint({
      cat("Predicted Probabilities:\n")
      for (label in colnames(pred)) {
        cat(label, ":", round(pred[[label]], 4), "\n")
      }
      cat("\n→ Predicted Class:", colnames(pred)[which.max(pred)], "\n")
    })
  })

  # Dynamic UI for Comparative Analysis Content
  output$analysis_content <- renderUI({
    req(input$analysis_view)
    if (input$analysis_view == "Info") {
      card(
        card_header("Model Training Information"),
        card_body(
          markdown("
            ### Model Training Overview
            The predictive models were developed to classify cardiotoxicity using metabolite intensities from human induced pluripotent stem cell-derived cardiomyocytes.

            #### Data Preprocessing
            - **Raw Data**: Metabolomics data was collected and transformed into a wide format (`data_wide`).
            - **Feature Selection**: Five key metabolites (`Arachidonic Acid`, `Thymidine`, `Thymidine.D4`, `Lactate`, `Citrulline-D4`) were selected based on statistical significance, forming `data_wide_selected`.
            - **Preprocessing**: No explicit preprocessing (e.g., log-transformation, scaling) was applied to the input data for predictions, assuming models were trained on raw intensities.

            #### Model Training
            - **Logistic Regression**: Trained with LASSO regularization using the `caret` package to select relevant features and prevent overfitting. The model predicts the probability of cardiotoxicity (`Cardiotoxic` vs. `Non`).
            - **Random Forest**: A Random Forest classifier was trained using `caret`, leveraging ensemble decision trees to capture complex relationships in the data.
            - **Ensemble**: An ensemble model combined predictions from Logistic Regression and Random Forest, using a majority voting or averaging approach to improve robustness.

            #### Evaluation
            - **Dataset**: Models were evaluated on a test set, with 2 rows removed due to NaN values.
            - **Metrics**: Performance was assessed using accuracy, F1-score, sensitivity, specificity, and PR AUC. Confusion matrices were generated to visualize classification performance.
            - **ROC Curves**: A combined ROC AUC curve was plotted for Logistic Regression and Random Forest to compare their discriminative ability.

            This analysis provides insights into the predictive power of each model for identifying cardiotoxic compounds based on metabolomics data.
          ")
        )
      )
    } else if (input$analysis_view == "Confusion Matrices") {
      card(
        card_header("Model Performance Comparison"),
        layout_column_wrap(
          width = 1 / 3,
          card(
            imageOutput("lr_cm")
          ),
          card(
            imageOutput("rf_cm")
          ),
          card(
            imageOutput("ensemble_cm")
          )
        )
      )
    } else if (input$analysis_view == "Metrics & ROC") {
      card(
        card_header("Model Performance Metrics and ROC Curve"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Metrics"),
            verbatimTextOutput("model_metrics")
          ),
          card(
            card_header("ROC AUC Curve"),
            imageOutput("roc_auc")
          )
        )
      )
    }
  })

  # Image Outputs for ROC Curve and Confusion Matrices
  output$roc_auc <- renderImage(
    {
      list(
        src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/roc_auc.png",
        alt = "Combined ROC AUC Curve (Logistic Regression and Random Forest)",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$lr_cm <- renderImage(
    {
      list(
        src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/lr_cm.png",
        alt = "Logistic Regression Confusion Matrix",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$rf_cm <- renderImage(
    {
      list(
        src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/rf_cm.png",
        alt = "Random Forest Confusion Matrix",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$ensemble_cm <- renderImage(
    {
      list(
        src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/ensemble_cm.png",
        alt = "Ensemble Confusion Matrix",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  # Comparative Analysis Metrics
  output$model_metrics <- renderPrint({
    metrics <- readLines("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/results/model_metrics.txt")
    cat(paste(metrics, collapse = "\n"))
  })

  # EDA Server Logic
  # Reactive to get selected dataset
  selected_dataset <- reactive({
    switch(input$eda_dataset,
      "data" = data,
      "data_wide" = data_wide,
      "data_wide_selected" = data_wide_selected
    )
  })

  # Update column choices for EDA visualizations
  observe({
    req(selected_dataset())
    updateSelectInput(session, "eda_columns",
      choices = names(selected_dataset()),
      # selected = names(selected_dataset())[1]
    )
  })

  # Update row_end max value based on dataset
  observe({
    req(selected_dataset())
    updateNumericInput(session, "eda_row_end",
      max = nrow(selected_dataset()),
      value = min(nrow(selected_dataset()))
    )
  })

  # Reactive for filtered dataset (visualizations)
  filtered_eda_data <- eventReactive(input$eda_apply, {
    req(input$eda_columns, input$eda_row_start, input$eda_row_end)
    validate(
      need(input$eda_row_start <= input$eda_row_end, "Start Row must be less than or equal to End Row"),
      need(input$eda_row_start >= 1 && input$eda_row_end <= nrow(selected_dataset()), "Invalid row range")
    )
    selected_dataset()[input$eda_row_start:input$eda_row_end, input$eda_columns, drop = FALSE]
  })

  # Render interactive plot
  output$eda_plot <- renderPlotly({
    req(filtered_eda_data(), input$eda_plot_type)
    df <- filtered_eda_data()

    # Ensure at least one numeric column for plotting
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    validate(
      need(length(numeric_cols) > 0, "Please select at least one numeric column for plotting")
    )

    if (input$eda_plot_type == "Histogram") {
      # Histogram for the first numeric column
      p <- ggplot(df, aes_string(x = numeric_cols[1])) +
        geom_histogram(bins = 30, fill = "#1a1818", color = "white") +
        theme_minimal() +
        labs(title = paste("Histogram of", numeric_cols[1]), x = numeric_cols[1], y = "Count")
      ggplotly(p)
    } else if (input$eda_plot_type == "Boxplot") {
      # Boxplot for all numeric columns
      df_long <- tidyr::pivot_longer(df, cols = numeric_cols, names_to = "Variable", values_to = "Value")
      p <- ggplot(df_long, aes(x = Variable, y = Value)) +
        geom_boxplot(fill = "#6c757d", color = "#1a1818") +
        theme_minimal() +
        labs(title = "Boxplot of Selected Features", x = "Feature", y = "Value") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    } else if (input$eda_plot_type == "Scatterplot") {
      # Scatterplot for first two numeric columns
      validate(
        need(length(numeric_cols) >= 2, "Please select at least two numeric columns for scatterplot")
      )
      p <- ggplot(df, aes_string(x = numeric_cols[1], y = numeric_cols[2])) +
        geom_point(color = "#1a1818", size = 3, alpha = 0.6) +
        theme_minimal() +
        labs(
          title = paste("Scatterplot of", numeric_cols[1], "vs", numeric_cols[2]),
          x = numeric_cols[1], y = numeric_cols[2]
        )
      ggplotly(p)
    }
  })

  # Reactive to get selected dataset for summary
  selected_summary_dataset <- reactive({
    switch(input$summary_dataset,
      "data" = data,
      "data_wide" = data_wide,
      "data_wide_selected" = data_wide_selected
    )
  })

  # Update column choices for summary
  observe({
    req(selected_summary_dataset())
    updateSelectInput(session, "summary_columns",
      choices = names(selected_summary_dataset()),
      # selected = names(selected_summary_dataset())[1:min(2, ncol(selected_summary_dataset()))]
    )
  })

  # Update row_end max value for summary
  observe({
    req(selected_summary_dataset())
    updateNumericInput(session, "summary_row_end",
      max = nrow(selected_summary_dataset()),
      value = min(10, nrow(selected_summary_dataset()))
    )
  })

  # Reactive for filtered dataset (summary)
  filtered_summary_data <- eventReactive(input$summary_apply, {
    req(input$summary_columns, input$summary_row_start, input$summary_row_end)
    validate(
      need(input$summary_row_start <= input$summary_row_end, "Start Row must be less than or equal to End Row"),
      need(input$summary_row_start >= 1 && input$summary_row_end <= nrow(selected_summary_dataset()), "Invalid row range")
    )
    selected_summary_dataset()[input$summary_row_start:input$summary_row_end, input$summary_columns, drop = FALSE]
  })

  # Render summary table
  output$summary_table <- renderDT({
    req(filtered_summary_data())
    df <- filtered_summary_data()

    # Compute summary statistics for numeric columns
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    summary_stats <- if (length(numeric_cols) > 0) {
      df %>%
        summarise(across(all_of(numeric_cols),
          list(
            Mean = ~ mean(.x, na.rm = TRUE),
            Median = ~ median(.x, na.rm = TRUE),
            Min = ~ min(.x, na.rm = TRUE),
            Max = ~ max(.x, na.rm = TRUE),
            SD = ~ sd(.x, na.rm = TRUE)
          ),
          .names = "{.col}_{.fn}"
        )) %>%
        tidyr::pivot_longer(everything(), names_to = c("Variable", "Statistic"), names_sep = "_", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = Statistic, values_from = Value)
    } else {
      data.frame(Variable = character(), Mean = numeric(), Median = numeric(), Min = numeric(), Max = numeric(), SD = numeric())
    }

    datatable(summary_stats, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

shinyApp(ui = ui, server = server)
