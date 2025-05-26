library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(stats)
library(dplyr)
library(caret)
library(plotly)
library(broom)
library(tidyr)

data <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/A_Targeted_Metabolomics-Based_Assay_Using_Human_Induced_Pluripotent_Stem_Cell-Derived_Cardiomyocytes_rawdata.csv")
data_wide <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/data_wide.csv")
data_wide_selected <- read.csv("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/data/data_wide_selected_processed.csv")

# Get feature names from data_wide_selected (excluding 'labels')
feature_names <- names(data_wide_selected)[-which(names(data_wide_selected) == "labels")]

# Prepare data_clean as in Testing.ipynb
data_clean <- data_wide %>%
  mutate(Effect = factor(Effect, levels = c("Non", "Cardiotoxic"))) %>%
  na.omit()

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
              choices = NULL,
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
              choices = NULL,
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
    title = "Statistical Testing",
    navset_card_tab(
      height = 400,
      full_screen = TRUE,
      title = "Statistical Analysis",
      nav_panel(
        "Info",
        card(
          card_header("Statistical Testing Overview"),
          card_body(
            markdown("
              ### Statistical Testing Overview
              This panel performs statistical analyses to identify significant differences and associations in metabolomics data related to cardiotoxicity, complementing the predictive models.

              #### Data Preprocessing
              - **Raw Data**: The dataset (`A_Targeted_Metabolomics-Based_Assay_..._rawdata.csv`) is transformed into a wide format (`data_wide`).
              - **Cleaning**: Column names are sanitized using `make.names()` (e.g., `Arachidonic Acid` → `Arachidonic.Acid`). Rows with missing values are removed using `na.omit` (381 rows removed from 2,630).
              - **Effect Variable**: The `Effect` column is converted to a factor with two levels (`Non`, `Cardiotoxic`) for t-tests, excluding `SolventControl` and `NA`.

              #### Statistical Tests
              - **T-Tests**: Compares metabolite intensities between `Non` and `Cardiotoxic` groups for selected metabolites. Significant results (p < 0.05) indicate metabolites that differ between groups.
              - **ANOVA**: Tests differences in metabolite intensities across levels of `Compound` or `Dose`. Significant results (p < 0.05) suggest group-specific effects.
              - **Chi-Square Test**: Examines associations between `Effect` (`Non` vs. `Cardiotoxic`) and categorical variables (`SampleType`, `Compound`, `Dose`). Significant p-values indicate non-random associations.

              #### Outputs
              - **Tables**: Significant results (p < 0.05) are displayed in interactive `DT` tables.
              - **Plots**: Visualizations include boxplots (t-tests), bar plots with error bars (ANOVA), and bar plots (chi-square) to illustrate differences and associations.
              - **Interpretation**: Results inform biomarker discovery by identifying key metabolites and patterns, supporting feature selection for the Logistic Regression (65% accuracy) and Random Forest (91% accuracy) models.

              This analysis enhances the understanding of metabolomics data for cardiotoxicity biomarker discovery.
            ")
          )
        )
      ),
      nav_panel(
        "T-Tests",
        layout_sidebar(
          sidebar = sidebar(
            title = "T-Test Controls",
            selectInput(
              "ttest_metabolites",
              "Select Metabolites:",
              choices = names(data_clean)[sapply(data_clean, is.numeric)],
              multiple = TRUE,
              selected = names(data_clean)[sapply(data_clean, is.numeric)][1]
            ),
            actionButton("ttest_apply", "Apply")
          ),
          card(
            card_header("T-Test Results (Non vs. Cardiotoxic)"),
            DTOutput("ttest_table"),
            card_body(
              "Compares metabolite intensities between Non and Cardiotoxic groups. Significant results (p < 0.05) are highlighted.",
              plotlyOutput("ttest_plot", height = "400px")
            )
          )
        )
      ),
      nav_panel(
        "ANOVA",
        layout_sidebar(
          sidebar = sidebar(
            title = "ANOVA Controls",
            selectInput(
              "anova_metabolites",
              "Select Metabolites:",
              choices = names(data_clean)[sapply(data_clean, is.numeric)],
              multiple = TRUE,
              selected = names(data_clean)[sapply(data_clean, is.numeric)][1]
            ),
            selectInput(
              "anova_group",
              "Group By:",
              choices = c("Compound", "Dose"),
              selected = "Compound"
            ),
            actionButton("anova_apply", "Apply")
          ),
          card(
            card_header("ANOVA Results"),
            DTOutput("anova_table"),
            card_body(
              "Tests differences in metabolite intensities across Compound or Dose. Significant results (p < 0.05) are highlighted.",
              plotlyOutput("anova_plot", height = "400px")
            )
          )
        )
      ),
      nav_panel(
        "Chi-Square Test",
        layout_sidebar(
          sidebar = sidebar(
            title = "Chi-Square Controls",
            selectInput(
              "chisq_var",
              "Select Variable:",
              choices = c("SampleType", "Compound", "Dose"),
              selected = "SampleType"
            ),
            actionButton("chisq_apply", "Apply")
          ),
          card(
            card_header("Chi-Square Test Results"),
            DTOutput("chisq_table"),
            card_body(
              "Tests association between Effect (Non vs. Cardiotoxic) and selected variable.",
              plotlyOutput("chisq_plot", height = "400px")
            )
          )
        )
      )
    )
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

  # Outputs for Data Viewer
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

    input_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(logistic_sanitized_names)))
    colnames(input_df) <- logistic_sanitized_names

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

    input_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(rf_sanitized_names)))
    colnames(input_df) <- rf_sanitized_names

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
          width = 1/3,
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
  output$roc_auc <- renderImage({
    list(
      src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/roc_auc.png",
      alt = "Combined ROC AUC Curve (Logistic Regression and Random Forest)",
      width = "100%"
    )
  }, deleteFile = FALSE)

  output$lr_cm <- renderImage({
    list(
      src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/lr_cm.png",
      alt = "Logistic Regression Confusion Matrix",
      width = "100%"
    )
  }, deleteFile = FALSE)

  output$rf_cm <- renderImage({
    list(
      src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/rf_cm.png",
      alt = "Random Forest Confusion Matrix",
      width = "100%"
    )
  }, deleteFile = FALSE)

  output$ensemble_cm <- renderImage({
    list(
      src = "/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/plots/ensemble_cm.png",
      alt = "Ensemble Confusion Matrix",
      width = "100%"
    )
  }, deleteFile = FALSE)

  # Comparative Analysis Metrics
  output$model_metrics <- renderPrint({
    metrics <- readLines("/home/ilesh-dhall/Metabolomics-Biomarker-Discovery/results/model_metrics.txt")
    cat(paste(metrics, collapse = "\n"))
  })

  # Statistical Testing Server Logic
  # T-Test Logic
  ttest_results <- eventReactive(input$ttest_apply, {
    req(input$ttest_metabolites)
    validate(
      need(length(input$ttest_metabolites) > 0, "Please select at least one metabolite")
    )
    
    results <- lapply(input$ttest_metabolites, function(met) {
      tidy(t.test(data_clean[[met]] ~ data_clean$Effect, data = data_clean)) %>%
        mutate(Metabolite = met)
    }) %>% bind_rows() %>%
      select(Metabolite, estimate, statistic, p.value, conf.low, conf.high) %>%
      mutate(p.value = round(p.value, 4))
    
    results
  })

  output$ttest_table <- renderDT({
    req(ttest_results())
    datatable(
      ttest_results() %>% filter(p.value < 0.05),
      options = list(pageLength = 10),
      caption = "Significant T-Test Results (p < 0.05)"
    )
  })

  output$ttest_plot <- renderPlotly({
    req(input$ttest_metabolites)
    validate(
      need(length(input$ttest_metabolites) > 0, "Please select at least one metabolite")
    )
    
    df_long <- data_clean %>%
      select(Effect, all_of(input$ttest_metabolites)) %>%
      pivot_longer(cols = all_of(input$ttest_metabolites), names_to = "Metabolite", values_to = "Intensity")
    
    p <- ggplot(df_long, aes(x = Effect, y = Intensity, fill = Effect)) +
      geom_boxplot() +
      facet_wrap(~Metabolite, scales = "free_y") +
      labs(title = "Metabolite Intensities by Effect", x = "Effect", y = "Intensity") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })

  # ANOVA Logic
  anova_results <- eventReactive(input$anova_apply, {
    req(input$anova_metabolites, input$anova_group)
    validate(
      need(length(input$anova_metabolites) > 0, "Please select at least one metabolite")
    )
    
    results <- lapply(input$anova_metabolites, function(met) {
      formula <- as.formula(paste(met, "~", input$anova_group))
      tidy(aov(formula, data = data_clean)) %>%
        mutate(Metabolite = met)
    }) %>% bind_rows() %>%
      filter(term != "Residuals") %>%
      select(Metabolite, term, statistic, p.value) %>%
      mutate(p.value = round(p.value, 4))
    
    results
  })

  output$anova_table <- renderDT({
    req(anova_results())
    datatable(
      anova_results() %>% filter(p.value < 0.05),
      options = list(pageLength = 10),
      caption = paste("Significant ANOVA Results (p < 0.05) by", input$anova_group)
    )
  })

  output$anova_plot <- renderPlotly({
    req(input$anova_metabolites, input$anova_group)
    validate(
      need(length(input$anova_metabolites) > 0, "Please select at least one metabolite")
    )
    
    df_summary <- data_clean %>%
      select(all_of(input$anova_metabolites), !!sym(input$anova_group)) %>%
      pivot_longer(cols = all_of(input$anova_metabolites), names_to = "Metabolite", values_to = "Intensity") %>%
      group_by(Metabolite, !!sym(input$anova_group)) %>%
      summarise(
        Mean = mean(Intensity, na.rm = TRUE),
        SE = sd(Intensity, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    p <- ggplot(df_summary, aes_string(x = input$anova_group, fill = input$anova_group)) +
      geom_bar(aes(y = Mean), stat = "identity") +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.4) +
      facet_wrap(~Metabolite, scales = "free_y") +
      labs(title = paste("Mean Intensities by", input$anova_group), x = input$anova_group, y = "Mean Intensity") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })

  # Chi-Square Logic
  chisq_results <- eventReactive(input$chisq_apply, {
    req(input$chisq_var)
    contingency_table <- table(data_clean$Effect, data_clean[[input$chisq_var]])
    chi_test <- chisq.test(contingency_table)
    data.frame(
      Statistic = chi_test$statistic,
      P.value = round(chi_test$p.value, 4),
      Df = chi_test$parameter
    )
  })

  output$chisq_table <- renderDT({
    req(chisq_results())
    datatable(
      chisq_results(),
      options = list(pageLength = 5),
      caption = paste("Chi-Square Test Results (Effect vs.", input$chisq_var, ")")
    )
  })

  output$chisq_plot <- renderPlotly({
    req(input$chisq_var)
    contingency_table <- table(data_clean$Effect, data_clean[[input$chisq_var]])
    contingency_df <- as.data.frame(contingency_table)
    colnames(contingency_df) <- c("Effect", input$chisq_var, "Count")
    
    p <- ggplot(contingency_df, aes_string(x = input$chisq_var, y = "Count", fill = "Effect")) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Effect vs.", input$chisq_var), x = input$chisq_var, y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })

  # EDA Server Logic
  selected_dataset <- reactive({
    switch(input$eda_dataset,
           "data" = data,
           "data_wide" = data_wide,
           "data_wide_selected" = data_wide_selected
    )
  })

  observe({
    req(selected_dataset())
    updateSelectInput(session, "eda_columns",
                      choices = names(selected_dataset())
    )
  })

  observe({
    req(selected_dataset())
    updateNumericInput(session, "eda_row_end",
                       max = nrow(selected_dataset()),
                       value = min(nrow(selected_dataset()))
    )
  })

  filtered_eda_data <- eventReactive(input$eda_apply, {
    req(input$eda_columns, input$eda_row_start, input$eda_row_end)
    validate(
      need(input$eda_row_start <= input$eda_row_end, "Start Row must be less than or equal to End Row"),
      need(input$eda_row_start >= 1 && input$eda_row_end <= nrow(selected_dataset()), "Invalid row range")
    )
    selected_dataset()[input$eda_row_start:input$eda_row_end, input$eda_columns, drop = FALSE]
  })

  output$eda_plot <- renderPlotly({
    req(filtered_eda_data(), input$eda_plot_type)
    df <- filtered_eda_data()

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    validate(
      need(length(numeric_cols) > 0, "Please select at least one numeric column for plotting")
    )

    if (input$eda_plot_type == "Histogram") {
      p <- ggplot(df, aes_string(x = numeric_cols[1])) +
        geom_histogram(bins = 30, fill = "#1a1818", color = "white") +
        theme_minimal() +
        labs(title = paste("Histogram of", numeric_cols[1]), x = numeric_cols[1], y = "Count")
      ggplotly(p)
    } else if (input$eda_plot_type == "Boxplot") {
      df_long <- tidyr::pivot_longer(df, cols = numeric_cols, names_to = "Variable", values_to = "Value")
      p <- ggplot(df_long, aes(x = Variable, y = Value)) +
        geom_boxplot(fill = "#6c757d", color = "#1a1818") +
        theme_minimal() +
        labs(title = "Boxplot of Selected Features", x = "Feature", y = "Value") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    } else if (input$eda_plot_type == "Scatterplot") {
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

  selected_summary_dataset <- reactive({
    switch(input$summary_dataset,
           "data" = data,
           "data_wide" = data_wide,
           "data_wide_selected" = data_wide_selected
    )
  })

  observe({
    req(selected_summary_dataset())
    updateSelectInput(session, "summary_columns",
                      choices = names(selected_summary_dataset())
    )
  })

  observe({
    req(selected_summary_dataset())
    updateNumericInput(session, "summary_row_end",
                       max = nrow(selected_summary_dataset()),
                       value = min(10, nrow(selected_summary_dataset()))
    )
  })

  filtered_summary_data <- eventReactive(input$summary_apply, {
    req(input$summary_columns, input$summary_row_start, input$summary_row_end)
    validate(
      need(input$summary_row_start <= input$summary_row_end, "Start Row must be less than or equal to End Row"),
      need(input$summary_row_start >= 1 && input$summary_row_end <= nrow(selected_summary_dataset()), "Invalid row range")
    )
    selected_summary_dataset()[input$summary_row_start:input$summary_row_end, input$summary_columns, drop = FALSE]
  })

  output$summary_table <- renderDT({
    req(filtered_summary_data())
    df <- filtered_summary_data()

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    summary_stats <- if (length(numeric_cols) > 0) {
      df %>%
        summarise(across(all_of(numeric_cols),
                         list(
                           Mean = ~mean(., na.rm = TRUE),
                           Median = ~median(., na.rm = TRUE),
                           Min = ~min(., na.rm = TRUE),
                           Max = ~max(., na.rm = TRUE),
                           SD = ~sd(., na.rm = TRUE)
                         ),
                         .names = "{.col}_{.fn}"
        )) %>%
        tidyr::pivot_longer(everything(), names_to = c("Variable", "Statistic"), names_sep = "_", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = Statistic, values_from = Value)
    } else {
      data.frame(Variable = character(), Mean = numeric(), Median = numeric(), Min = numeric(), Max = numeric(), SD = numeric())
    }

    datatable(
      summary_stats,
      options = list(pageLength = 10, autoWidth = TRUE),
      caption = if (length(numeric_cols) == 0) "No numeric columns selected" else "Summary Statistics"
    )
  })
}

shinyApp(ui = ui, server = server)