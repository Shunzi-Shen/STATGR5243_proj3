library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(caret)
library(corrplot)


# Safe null fallback operator
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Styled button generator
styled_button <- function(id, label, group_color, is_main = FALSE) {
  btn_color <- ifelse(group_color == "red", "#e74c3c", "#3498db")
  font_size <- ifelse(is_main, "18px", "14px")
  padding <- ifelse(is_main, "10px 20px", "6px 12px")
  actionButton(id, label,
               style = paste0("background-color: ", btn_color, "; color: white; font-weight: bold; font-size:", font_size, "; padding:", padding, ";"))
}


version_label <- reactiveVal("Unknown")
group_id_label <- reactiveVal("unknown")

data.cleaning.tab <- tabPanel("Data Cleaning", sidebarLayout(
  sidebarPanel(
    # Remove duplicates
    tags$p(tags$b("Handling Duplicates")),
    uiOutput("btn_apply_duplicates"),   
    
    # Select variable for preprocessing
    selectInput("selected_var", "Select Variables for Preprocessing",
                choices = NULL, selected = NULL, multiple = TRUE),
    # Select preprocessing actions
    selectInput("preprocess_options", "Select Preprocessing Actions",
                choices = list("Handle Inconsistencies" = "handle_inconsistencies",
                               "Delete Missing Values" = "delete_na",
                               "Fill Missing Values (Mean Imputation)" = "impute_mean",
                               "Fill Missing Values (Median Imputation)" = "impute_median",
                               "Remove Outliers" = "outliers",
                               "Apply Log Transformation" = "log_transform",
                               "Standardize Numeric Features" = "standardize",
                               "Normalize Data" = "normalize",
                               "Encode Categorical Features" = "encode"),
                selected = NULL),
    selectInput("fix_options", "Select Fixes",
                choices = list("Standardize Date Format" = "fix_date_format",
                               "Fix Data Types" = "fix_types",
                               "Trim Text Spaces" = "fix_spaces"),
                selected = NULL),
    uiOutput("outlier_slider"),
    uiOutput("btn_apply_preprocess")         
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Cleaned Data Preview", tableOutput("cleaned_table")),
    tabPanel("Summary Statistics", verbatimTextOutput("cleaned_summary"))
  ))
))



feature.tab <- tabPanel("Feature Engineering", sidebarLayout(
  sidebarPanel(
    # feature Engineering options
    h4("Feature Engineering"),
    
    # New feature generation
    textInput(
      "new_feature_expr",
      "New Feature Expression (e.g., column1 + column2)",
      ""
    ),
    uiOutput("btn_apply_new_feature"),  
    
    # transformations on existing features
    selectInput(
      "transform_column",
      "Select Column for Transformation",
      choices = NULL
    ),
    radioButtons(
      "transformation_type",
      "Select Transformation",
      choices = c(
        "Logarithm" = "log",
        "Square Root" = "sqrt",
        "Square" = "square"
      )
    ),
    uiOutput("styled_transformation_button"),
    
    # plotting option
    checkboxInput("show_plot", "Show Plot of Feature Impact", value = TRUE)
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Feature Engineering Results", tableOutput("feature_table")),
    tabPanel("Feature Impact Plot", plotOutput("feature_impact_plot"))
  ))
))




eda.tab <- tabPanel("Exploratory Data Analysis", sidebarLayout(
  sidebarPanel(
    h4("Exploratory Data Analysis Settings"),
    
    # Select X-axis variable (for scatter plot)
    uiOutput("x_var_select"),
    
    # Select Y-axis variable (for scatter plot)
    uiOutput("y_var_select"),
    
    # Filtering options for numeric columns
    uiOutput("numeric_filter"),
    
    # Select plot type
    selectInput(
      "plot_type",
      "Select Plot Type",
      choices = c("Scatter Plot", "Histogram", "Box Plot")
    ),
    
    # Display correlation matrix button
    uiOutput("btn_correlation") 
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Data Summary", verbatimTextOutput("data_summary")),
    tabPanel("Visualization", plotlyOutput("eda_plot")),
    tabPanel("Correlation Matrix", plotOutput("correlation_matrix")),
    tabPanel("Filtered Data", DT::dataTableOutput("filtered_data"))
  ))
)

# # Likert scales at the bottom
# absolutePanel(
#   # Title for Likert Scales
#   h4("Please rate your experience with our data analysis tool (low to high from 1 to 7)"),
#   
#   # VA Likert scale
#   radioButtons("VA", "Visual Appeal: your rating of interface aesthetics", choices = 1:7, selected = character(0), inline = TRUE),
#   
#   # ES Likert scale
#   radioButtons("ES", "Element Salience: your rating of content prominence", choices = 1:7, selected = character(0), inline = TRUE),
#   
#   # CI Likert scale
#   radioButtons("CI", "Click Intent: your inclination to explore the webpage", choices = 1:7, selected = character(0), inline = TRUE),
#   
#   # DS Likert scale
#   radioButtons("DS", "Design Harmony: your rating of layout coherence", choices = 1:7, selected = character(0), inline = TRUE),
#   
#   # Submit button to capture choices
# #   actionButton("submit", "Submit")
# )
)





# Define UI
ui <- fluidPage(
  tags$head(
    HTML("<!-- Google tag (gtag.js) -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=G-MKCVY4PM17'></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-MKCVY4PM17');
    </script>
    <script>
      function getCookie(name) {
        let match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
        if (match) return match[2];
        else return null;
      }
    

      let ab_group = getCookie('ab_group');
      if (!ab_group) {
        let colors = ['red', 'blue'];
        let sizes = ['small', 'large'];
        let color = colors[Math.floor(Math.random() * colors.length)];
        let size = sizes[Math.floor(Math.random() * sizes.length)];
        ab_group = color + '-' + size;
        document.cookie = 'ab_group=' + ab_group + '; path=/';
      }
    

  document.addEventListener('DOMContentLoaded', function() {
    setTimeout(function() {
      Shiny.setInputValue('ab_cookie', ab_group, {priority: 'event'});
    }, 500);
  });
    </script>

    ")
  ),
  
  div(class = "version-badge", textOutput("version_display")),
  
  navbarPage(
    title = "Advanced Data Analysis Tool",
    id = "main_navbar",
    
    # User Guide Page
    # User Guide Page
    tabPanel("User Guide", fluidPage(
      h3("Overview"),
      p("Welcome to our Advanced Data Analysis Tool! This Shiny application is designed to guide you through the complete data analysis workflow — from uploading datasets to cleaning, feature engineering, and exploratory analysis.
        The app supports multiple file formats including CSV, Excel, JSON, and RDS. Welcome to explore and interact with your data intuitively and efficiently!"),
      
      h3("Key Functionalities"),
      tags$ol(
        tags$li(
          strong("Loading Datasets"),
          p("Import your dataset or use one of the built-in examples."),
          tags$ul(
            tags$li(strong("Multiple File Formats:"), " Upload CSV, Excel (XLS/XLSX), JSON, and RDS files."),
            tags$li(strong("Built-in Datasets:"), " Explore using mtcars or iris without uploading your own data."),
            tags$li(strong("Preview:"), " View the first few rows of the dataset after loading.")
          )
        ),
        tags$li(
          strong("Data Cleaning & Preprocessing"),
          p("Clean your data by handling missing values, outliers, duplicates, and inconsistencies."),
          tags$ul(
            tags$li(strong("Duplicate Removal:"), " Remove duplicate rows with one click."),
            tags$li(strong("Missing-Value Handling:"), " Remove or impute missing values using mean, median, or mode."),
            tags$li(strong("Scaling:"), " Normalize, scale, or standardize numeric columns."),
            tags$li(strong("Categorical Encoding:"), " Apply one-hot encoding to character or factor columns."),
            tags$li(strong("Cleaned Data Preview:"), " View updated summary statistics after cleaning."),
            tags$li(strong("Outlier Handling:"), " Filter data using Z-score thresholds."),
            tags$li(strong("Handling Inconsistencies:"), " Fix common issues in date or numeric values.")
          )
        ),
        tags$li(
          strong("Feature Engineering"),
          p("Generate new variables and apply transformations."),
          tags$ul(
            tags$li(strong("New Feature Creation:"), " Create new columns using custom expressions like col1 + col2."),
            tags$li(strong("Column Transformations:"), " Apply log, square root, or square operations to numeric columns."),
            tags$li(strong("Interactive Plots:"), " Compare variable distributions before and after transformation side by side.")
          )
        ),
        tags$li(
          strong("Exploratory Data Analysis (EDA)"),
          p("Explore data visually and statistically."),
          tags$ul(
            tags$li(strong("Data Summary:"), " View descriptive statistics (min, mean, median, max) for each column."),
            tags$li(strong("Interactive Visualizations:"), " Generate scatter, histogram, or box plots with Plotly."),
            tags$li(strong("Filtering:"), " Filter numeric columns using sliders to explore subsets.")
          )
        )
      ),
      
      h3("How to Use the App"),
      tags$ol(
        tags$li(
          tags$b("Upload Data"), ": Start by uploading your own dataset or selecting a built-in example."
        ),
        tags$li(
          tags$b("Data Cleaning"), ": Remove duplicates, handle missing values, normalize data, and encode categorical variables."
        ),
        tags$li(
          tags$b("Feature Engineering"), ": Create new features or apply mathematical transformations."
        ),
        tags$li(
          tags$b("Explore"), ": Use the EDA tab to gain insights from your data.",
          tags$ul(
            tags$li(tags$b("Data Summary"), ": View descriptive statistics."),
            tags$li(tags$b("Visualization"), ": Generate interactive scatter, histogram, and box plots."),
            tags$li(tags$b("Correlation Matrix"), ": Discover relationships using correlation heatmaps."),
            tags$li(tags$b("Filtered Data"), ": Filter rows by value range to inspect subsets.")
          )
        )
      )
    )),
    
      
    
    # File Upload Page
    tabPanel("Upload Data", sidebarLayout(
     sidebarPanel(
       fileInput(
         "file1",
         "Choose File to upload",
         accept = c(
           "text/csv",
           "text/comma-separated-values,text/plain",
           ".csv",
           ".xls",
           ".xlsx",
           ".json",
           ".rds"
         )
       ),
       checkboxInput("header", "CSV File has Header", value = TRUE),
       radioButtons(
         "sep",
         "Separator in CSV File",
         choices = c(
           Comma = ",",
           Semicolon = ";",
           Tab = "\t"
         ),
         selected = ","
       ),
       radioButtons(
         "quote",
         "Quote in Data File",
         choices = c(
           None = "",
           "Double Quote" = "\"",
           "Single Quote" = "'"
         ),
         selected = "\""
       ),
       uiOutput("btn_upload"),
       
       selectInput(
         "builtin_dataset",
         "Select Built-in Dataset",
         choices = c("None", "mtcars", "iris"),
         selected = "mtcars"
       ),
       uiOutput("btn_load_builtin")
     ),
     mainPanel(tabsetPanel(tabPanel(
       "Data Preview", tableOutput("table")
     )))
    )),
    
    data.cleaning.tab,
    
    feature.tab,
    
    eda.tab,
    
    tabPanel("Feedback Survey", 
             fluidPage(
               h3("We value your feedback!"),
               tags$iframe(
                 src = "https://docs.google.com/forms/d/e/1FAIpQLSeIEQtxACpAQxPYlUGndtOeZ0DFtJkPS7BIcZKv8t7s2ywmuQ/viewform?embedded=true",
                 width = "100%",
                 height = "1600",
                 frameborder = "0",
                 marginheight = "0",
                 marginwidth = "0",
                 style = "border: none;"
               )
             )
    )
  )
)

server <- function(input, output, session) {
  ab_group_string <- reactiveVal("unknown")
  
  # A/B Testing Setup
  user_id <- paste0("user_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
  session_id <- session$token
  session_start_time <- Sys.time()
  last_interaction <- reactiveVal(Sys.time())
  task_completed <- reactiveVal(FALSE)
  feature_page_visited <- reactiveVal(FALSE)
  click_counts <- reactiveValues()
  page_start_time <- reactiveVal(Sys.time())
  page_durations <- reactiveValues()
  all_buttons <- c("apply_transformation")
  data <- reactiveVal(mtcars)
  
  # Group assignment for A/B testing
  group_color <- reactiveVal(sample(c("red", "blue"), 1))
  group_size <- reactiveVal(sample(c("large", "small"), 1))


  output$version_display <- renderText({
    req(version_label(), ab_group_string())
    paste0("Version: ", version_label(), " (group: ", ab_group_string(), ")")
  })
  
  observe({
    invalidateLater(1000, session)
    now <- Sys.time()
    if (difftime(now, last_interaction(), units = "secs") > 60) {
      session$sendCustomMessage("log_ga_event", list(
        event = "bounce_detected",
        group = paste(group_color(), group_size(), sep = "-")
      ))
    }
  })  
  
  
  observeEvent(input$main_navbar, {
    now <- Sys.time()
    tab <- isolate(input$main_navbar)
    duration <- difftime(now, isolate(page_start_time()), units = "secs")
    old_duration <- tryCatch(page_durations[[tab]], error = function(e) 0)
    page_durations[[tab]] <- old_duration + as.numeric(duration)
    page_start_time(now)
    
    # ✅ 完成率事件：只要进入了 EDA 页（最后一个页面）下的任意 tab，就发事件
    if (tab == "Exploratory Data Analysis") {
      session$sendCustomMessage("log_ga_event", list(
        event = "reach_last_tab",
        group = paste(group_color(), group_size(), sep = "-")
      ))
     task_completed(TRUE)
    }
    
    # 记录是否访问了 Feature Engineering 页（点击率分母）
    if (tab == "Feature Engineering") {
      session$sendCustomMessage("log_ga_event", list(
        event = "enter_feature_page",
        group = paste(group_color(), group_size(), sep = "-")
      ))
      feature_page_visited(TRUE)
    }
    
    log <- data.frame(
      timestamp = now,
      user_id = user_id,
      session_id = session_id,
      action = "tab_switch",
      tab = tab,
      duration_sec = as.numeric(duration),
      color = group_color(),
      size = group_size(),
      group = paste(group_color(), group_size(), sep = "-")
    )
    write.table(log, "ab_click_log.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  })
  
  
  
  
  observeEvent(input$ab_cookie, {
    parts <- strsplit(input$ab_cookie, "-")[[1]] 
    if (length(parts) == 2) {
      group_color(parts[1])
      group_size(parts[2])
      ab_group_string(input$ab_cookie)
      version_label(paste(toupper(parts[1]), "font", parts[2]))
    }
  })
  
  observeEvent(input$main_navbar, {
    if (input$main_navbar == "Feedback Survey") {
      session$sendCustomMessage("log_ga_event", list(
        event = "enter_feedback_survey",
        group = paste(group_color(), group_size(), sep = "-")
      ))
    }
  })
  

  observe({
    if (is.null(group_color()) || is.null(group_size())) {
      group_color(sample(c("blue", "red"), 1))
      group_size(sample(c("large", "small"), 1))
    }
  })
  
  user_interacted <- reactiveVal(FALSE)
  session_start_time <- Sys.time()
  transformation_clicks <- reactiveVal(0)
  
  all_buttons <- c("apply_transformation", "apply_new_feature", "remove_duplicates",
                   "upload", "load_builtin", "preprocess")
  
  for (btn in all_buttons) {
    observeEvent(input[[btn]], {
      last_interaction(Sys.time())
      click_counts[[btn]] <- isolate((click_counts[[btn]] %||% 0) + 1)
      
      
      log <- data.frame(
        timestamp = Sys.time(),
        user_id = user_id,
        session_id = session_id,
        action = paste0("click_", btn),
        count = click_counts[[btn]],
        color = group_color(),
        size = group_size(),
        group = paste(group_color(), group_size(), sep = "-")
      )
      write.table(log, "ab_click.csv", append = TRUE, sep = ",",
                  row.names = FALSE, col.names = !file.exists("ab_click.csv"))
    })
  }
  
  
  # Data Management
  data <- reactiveVal(mtcars)
  
  data <- reactiveVal(mtcars)
  
  # upload data
  observeEvent(input$upload, {
    req(input$file1)
    file <- input$file1$datapath
    ext <- tools::file_ext(file)
    
    if (ext %in% c("csv", "txt")) {
      df <- read_csv(
        file,
        col_names = input$header,
        col_types = NULL,
        skip = 0,
        n_max = -1,
        progress = FALSE,
        locale = default_locale(),
        na = c("", "NA"),
        comment = ""
      )
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(file, sheet = 1)
    } else if (ext == "json") {
      df <- fromJSON(file)
    } else if (ext == "rds") {
      df <- readRDS(file)
    }
    
    data(df)
  })
  
  # load built-in dataset
  observeEvent(input$load_builtin, {
    dataset <- input$builtin_dataset
    if (dataset == "mtcars") {
      data(mtcars)
    } else if (dataset == "iris") {
      data(iris)
    }
  })
  
  # data Preview
  output$table <- renderTable({
    req(data())
    head(data())
  })
  
  
  # Remove duplicates
  observeEvent(input$remove_duplicates, {
    req(data())
    df <- data()
    df <- df %>% distinct()
    data(df) 
  })
  
  # Update selected columns to preprocess
  observe({
    req(data())
    updateSelectInput(session, "selected_var", choices = names(data()))
  })
  
  # Preprocess data
  numeric_error <- function(variable, option) {
    showModal(modalDialog(
      title = "Error",
      paste("Column '", variable, "' is not numeric. '", option, "' can only be applied to numeric columns."),
      easyClose = TRUE,
      footer = NULL
    ))
  }
  
  string_error <- function(variable, option) {
    showModal(modalDialog(
      title = "Error",
      paste("Column '", variable, "' is not a string. '", option, "' can only be applied to string (character or factor) columns."),
      easyClose = TRUE,
      footer = NULL
    ))
  }
  
  preprocess_var <- function (df, var, option, threshold, fix_option) {
    
    if ("outliers" %in% option) {
      if (!is.numeric(df[[var]])) {
        numeric_error(var, "Outliers")
        return(df)
      }
      df <- filter(abs(scale(df[[var]])) < threshold)
    } else if ("log_transform" %in% option) {
      if (!is.numeric(df[[var]])) {
        numeric_error(var, "Log_transform")
        return(df)
      }
      df[[var]] <- log1p(df[[var]])
    } else if ("delete_na" %in% option) {
      df <- df %>% drop_na(var)
    } else if ("impute_mean" %in% option) {
      if (!is.numeric(df[[var]])) {
        numeric_error(var, "Impute_mean")
        return(df)
      }
      df[[var]][is.na(df[[var]])] <- mean(df[[var]], na.rm = TRUE)
    } else if ("impute_median" %in% option) {
      if (!is.numeric(df[[var]])) {
        numeric_error(var, "Impute_median")
        return(df)
      }
      df[[var]][is.na(df[[var]])] <- median(df[[var]], na.rm = TRUE)
    } else if ("standardize" %in% option) {
      if (!is.numeric(df[[var]])) {
        numeric_error(var, "Standardize")
        return(df)
      }
      df[[var]] <- scale(df[[var]])
    } else if ("normalize" %in% option) {
      if (!is.numeric(df[[var]])) {
        numeric_error(var, "Normalize")
        return(df)
      }
      df[[var]] <- (df[[var]] - min(df[[var]], na.rm = TRUE)) /
        (max(df[[var]], na.rm = TRUE) - min(df[[var]], na.rm = TRUE))
    } else if ("encode" %in% option) {
      if (!is.character(df[[var]]) && !is.factor(df[[var]])) {
        string_error(var, "Encode")
        return(df)
      }
      df[[var]] <- as.factor(df[[var]])
    } else if ("handle_inconsistencies" %in% option) {
      # Fix data types (convert strings in numeric columns)
      if ("fix_types" %in% fix_option) {
        if (!is.character(df[[var]]) && !is.factor(df[[var]])) {
          string_error(var, "Fix_types")
          return(df)
        }
        df[[var]] <- df[[var]] %>%
          mutate(across(where(~ any(suppressWarnings(!is.na(as.numeric(.))))), as.numeric))
      } # Trim Extra Spaces in Text Columns
      else if ("fix_spaces" %in% fix_option) {
        if (!is.character(df[[var]]) && !is.factor(df[[var]])) {
          string_error(var, "Fix_spaces")
          return(df)
        }
        df[[var]] <- trimws(as.character(df[[var]]))
      } # Standardize Date Format
      else if ("fix_date_format" %in% fix_option) {
        df[[var]] <- as.Date(df[[var]],
                             tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y", "%B %d, %Y"))
      }
    }
    
    return(df)
    
  }
  
  observeEvent(input$preprocess, {
    req(data())
    df <- data()
    
    selected_var <- input$selected_var
    
    if (!is.null(selected_var)) {
      for (var in selected_var) {
        df = preprocess_var(df, var, input$preprocess_options, input$outlier_threshold, input$fix_options)
      }
    }
    
    data(df)
  })

  
  # display cleaned data preview
  output$cleaned_table <- renderTable({
    req(data())
    head(data())
  })
  
  # show summary statistics
  output$cleaned_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # update available columns for transformation dynamically
  observe({
    df <- data()
    updateSelectInput(session, "transform_column", choices = colnames(df))
  })
  
  # create a new feature based on user input
  is_valid_expression <- function(df, expr) {
    columns <- all.vars(parse(text = expr))
    valid_columns <- all(columns %in% colnames(df) & sapply(df[columns], is.numeric))
    return(valid_columns)
  }
  
  observeEvent(input$apply_new_feature, {
    req(input$new_feature_expr)
    df <- data()
    
    # Evaluate the expression entered by the user to create a new feature
    new_feature_expr <- input$new_feature_expr
    
    if (!is_valid_expression(df, new_feature_expr)) {
      showModal(modalDialog(
        title = "Error",
        paste("Column not numeric."),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    feature_name <- gsub("[^[:alnum:]_]", "_", new_feature_expr)
    df <- df %>% mutate(!!feature_name := eval(parse(text = new_feature_expr)))
    
    data(df)
  })
  
  # apply mathematical transformations on selected column
  # Apply mathematical transformations on the selected column
  observeEvent(input$apply_transformation, {
    req(input$transform_column)
    df <- data()
    
    # Get the selected column and its name
    column_name <- input$transform_column
    
    # Apply the selected transformation and create a new column name
    if (input$transformation_type == "log") {
      if (!is.numeric(df[[column_name]])) {
        numeric_error(column_name, "Logarithm")
        return(NULL)
      }
      new_column_name <- paste0(column_name, "_log")
      df <- df %>% mutate(!!new_column_name := log(get(column_name) + 1))  # Add 1 to avoid log(0)
    } else if (input$transformation_type == "sqrt") {
      if (!is.numeric(df[[column_name]])) {
        numeric_error(column_name, "Square root")
        return(NULL)
      }
      new_column_name <- paste0(column_name, "_sqrt")
      df <- df %>% mutate(!!new_column_name := sqrt(get(column_name)))
    } else if (input$transformation_type == "square") {
      if (!is.numeric(df[[column_name]])) {
        numeric_error(column_name, "Square")
        return(NULL)
      }
      new_column_name <- paste0(column_name, "_square")
      df <- df %>% mutate(!!new_column_name := get(column_name) ^ 2)
    }
    
    session$sendCustomMessage("log_ga_event", list(
      event = "apply_transformation_click",
      group = paste(group_color(), group_size(), sep = "-")
    ))
    
    # Update the data with the transformed column
    data(df)
  })
  
  
  # display feature table 
  output$feature_table <- renderTable({
    req(data())
    head(data())
  })
  
  # generate plot of feature impact (before and after transformation)
  output$feature_impact_plot <- renderPlot({
    req(data())
    df <- data()
    
    if (input$show_plot) {
      feature_col <- input$transform_column
      
      # before transformation: original feature
      p_before <- ggplot(df, aes_string(x = feature_col)) + 
        geom_histogram(bins = 30, fill = "cornflowerblue") +
        labs(title = paste("Before", input$transformation_type, "on", feature_col),
             x = feature_col, y = "Frequency") +
        theme_minimal()
      
      # after transformation: transformed feature
      transformed_col <- paste0(feature_col, "_", input$transformation_type)  # Dynamic column name based on transformation
      
      # check if the transformed column exists
      if (transformed_col %in% colnames(df)) {
        p_after <- ggplot(df, aes_string(x = transformed_col)) + 
          geom_histogram(bins = 30, fill = "lightgreen") +
          labs(title = paste("After", input$transformation_type, "on", feature_col),
               x = transformed_col, y = "Frequency") +
          theme_minimal()
        
        gridExtra::grid.arrange(p_before, p_after, ncol = 2)
      } else {
        # if the transformed column does not exist yet, show only the "before" plot
        print(p_before)
      }
    }
  })
  
  
  
  output$x_var_select <- renderUI({
    df <- data()
    selectInput(
      "x_var",
      "Select X-Axis Variable",
      choices = colnames(df),
      selected = colnames(df)[1]
    )
  })
  
  # Dynamically generate Y-axis variable selection for analysis
  output$y_var_select <- renderUI({
    df <- data()
    selectInput(
      "y_var",
      "Select Y-Axis Variable",
      choices = colnames(df),
      selected = colnames(df)[2] # Default to second column for variety
    )
  })
  
  # Dynamically generate filtering options for numeric columns
  output$numeric_filter <- renderUI({
    df <- data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    sliderInput(
      "num_filter",
      "Filter Numeric Column Values",
      min = min(df[[numeric_cols[1]]], na.rm = TRUE),
      max = max(df[[numeric_cols[1]]], na.rm = TRUE),
      value = c(min(df[[numeric_cols[1]]], na.rm = TRUE), max(df[[numeric_cols[1]]], na.rm = TRUE))
    )
  })
  
  # Display summary statistics
  output$data_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Filter data based on user selection
  filtered_data <- reactive({
    df <- data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    req(input$num_filter)
    df <- df %>% filter(df[[numeric_cols[1]]] >= input$num_filter[1] &
                          df[[numeric_cols[1]]] <= input$num_filter[2])
    df
  })
  
  # Render the filtered data in a table
  output$filtered_data <- DT::renderDataTable({
    req(filtered_data())
    datatable(filtered_data())
  })
  
  # Render plot based on the selected plot type
  output$eda_plot <- renderPlotly({
    req(data())
    df <- data()
    
    if (input$plot_type == "Scatter Plot") {
      req(input$x_var, input$y_var) # Ensure both X and Y variables are selected
      p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "cornflowerblue") +
        labs(x = input$x_var, y = input$y_var, title = paste("Scatter Plot of", input$x_var, "vs", input$y_var)) +
        theme_minimal()
    } else if (input$plot_type == "Histogram") {
      req(input$x_var) # Only X variable needed
      p <- ggplot(df, aes_string(x = input$x_var)) +
        geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
        labs(x = input$x_var, y = "Count", title = paste("Histogram of", input$x_var)) +
        theme_minimal()
    } else if (input$plot_type == "Box Plot") {
      req(input$x_var) # Only X variable needed
      dat <- data.frame(value = df[[input$x_var]])
      p <- ggplot(dat, aes(x = "", y = value)) +
        geom_boxplot(fill = "cornflowerblue", color = "black") +
        labs(x = input$x_var, y = "Value", title = paste("Box Plot of", input$x_var)) +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # Render correlation matrix whenever new data is loaded
  observe({
    req(data())
    df <- data()
    
    # Compute the correlation matrix for numeric columns
    cor_matrix <- cor(df[sapply(df, is.numeric)], use = "complete.obs")
    
    # Display the correlation matrix plot
    output$correlation_matrix <- renderPlot({
      corrplot(
        cor_matrix,
        method = "circle",
        type = "upper",
        order = "hclust",
        tl.col = "black",
        tl.srt = 45
      )
    })
  })
  # observeEvent(input$correlation_btn, {
  #   req(data())
  #   df <- data()
  #   
  #   # Compute correlation matrix for numeric columns
  #   cor_matrix <- cor(df[sapply(df, is.numeric)], use = "complete.obs")
  #   
  #   output$correlation_matrix <- renderPlot({
  #     corrplot(
  #       cor_matrix,
  #       method = "circle",
  #       type = "upper",
  #       order = "hclust",
  #       tl.col = "black",
  #       tl.srt = 45
  #     )
  #   })
  # })
  
  
  
  # # Observe the submit button
  # observeEvent(input$submit, {
  #   
  #   # Get the device type information from user agent
  #   user_agent <- session$clientData
  #   device_type <- ifelse(grepl("Mobile", user_agent), "Mobile",
  #                         ifelse(grepl("Tablet", user_agent), "Tablet", "Desktop"))
  #   
  #   # Capture radio button choices
  #   VA_choice <- input$VA
  #   ES_choice <- input$ES
  #   CI_choice <- input$CI
  #   DS_choice <- input$DS
  #   
  #   # Create a data frame with the user's device type and choices
  #   user_data <- data.frame(
  #     device_type = device_type,
  #     VA_choice = VA_choice,
  #     ES_choice = ES_choice,
  #     CI_choice = CI_choice,
  #     DS_choice = DS_choice,
  #     timestamp = Sys.time(),  # Time of submission
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   # Append the user data to a CSV file
  #   write.table(user_data, "user_eval.csv", append = TRUE, sep = ",", col.names = !file.exists("user_eval.csv"), row.names = FALSE)
  #   
  #   # Print confirmation to the user (can be customized or removed)
  #   showModal(modalDialog(
  #     title = "Thank you!",
  #     "Your answers have been saved.",
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  
  
  
  
  observeEvent(input$main_navbar, {
    log <- data.frame(
      timestamp = Sys.time(),
      group = paste(group_color(), group_size(), sep = "-"),
      color = group_color(),
      size = group_size(),
      action = "tab_switch",
      tab = input$main_navbar
    )
    write.table(log, "ab_click_log.csv", append = TRUE, sep = ",",
                row.names = FALSE, col.names = FALSE)
  })
      
  # Styled buttons
  output$styled_transformation_button <- renderUI({ styled_button("apply_transformation", "Apply Transformation", group_color(), is_main = group_size() == "large") })
  output$btn_apply_duplicates <- renderUI({ styled_button("remove_duplicates", "Apply Duplicate Removal", group_color()) })
  output$btn_apply_new_feature <- renderUI({ styled_button("apply_new_feature", "Create New Feature", group_color()) })
  output$btn_upload <- renderUI({ styled_button("upload", "Upload Data", group_color()) })
  output$btn_load_builtin <- renderUI({ styled_button("load_builtin", "Load Built-in Dataset", group_color()) })
  # output$btn_correlation <- renderUI({ styled_button("correlation_btn", "Show Correlation Matrix", group_color()) })
  output$btn_apply_preprocess <- renderUI({styled_button("preprocess", "Apply Preprocessing", group_color())})
  output$outlier_slider <- renderUI({
    color <- ifelse(group_color() == "red", "#e74c3c", "#3498db")
    
    tagList(
      tags$style(HTML(
        sprintf("
          .irs--shiny .irs-bar,
          .irs--shiny .irs-single,
          .irs--shiny .irs-from,
          .irs--shiny .irs-to,
          .irs--shiny .irs-handle > i:first-child {
            background-color: %s !important;
            border-color: %s !important;
          }
        ", color, color)
      )),
      sliderInput("outlier_threshold", "Outlier Removal Threshold (Z-Score)",
                  min = 1, max = 5, value = 3, step = 0.1)
    )
  })
  output$numeric_filter <- renderUI({
    df <- data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    color <- ifelse(group_color() == "red", "#e74c3c", "#3498db")
      
    tagList(
      tags$style(HTML(sprintf("
        .irs--shiny .irs-bar,
        .irs--shiny .irs-single,
        .irs--shiny .irs-from,
        .irs--shiny .irs-to,
        .irs--shiny .irs-handle > i:first-child {
          background-color: %s !important;
          border-color: %s !important;
        }
      ", color, color))),
      sliderInput(
        "num_filter",
        "Filter Numeric Column Values",
        min = min(df[[numeric_cols[1]]], na.rm = TRUE),
        max = max(df[[numeric_cols[1]]], na.rm = TRUE),
        value = c(min(df[[numeric_cols[1]]], na.rm = TRUE), max(df[[numeric_cols[1]]], na.rm = TRUE))
      )
    )
  })
  
  session$onSessionEnded(function() {
    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, session_start_time, units = "secs"))
    log <- data.frame(
      timestamp = end_time,
      user_id = user_id,
      session_id = session_id,
      action = "session_end",
      total_session_time = total_time,
      task_completed = task_completed(),
      feature_page_visited = feature_page_visited(),
      color = group_color(),
      size = group_size(),
      group = paste(group_color(), group_size(), sep = "-")
    )
    write.table(log, "ab_click.csv", append = TRUE, sep = ",",
                row.names = FALSE, col.names = !file.exists("ab_click.csv"))
  }) 
        
        
        
      
}

shinyApp(ui = ui, server = server)