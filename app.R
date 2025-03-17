# Required Libraries ----
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("DT")) install.packages("DT")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("bslib")) install.packages("bslib")
if (!require("devtools")) install.packages("devtools")
if (!require("readxl")) install.packages("readxl")

# Load libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(bslib)
library(devtools)
library(readxl)

# Optional clinical_fd package
# Uncomment if needed:
# if (!require("clinical_fd")) devtools::install_github("sas2r/clinical_fd")
# library(clinicalfd)

# UI Definition ----
ui <- fluidPage(
  # Theme and CSS
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  
  # Header
  titlePanel(div(
    img(src = "https://img.icons8.com/color/48/000000/medical-doctor.png", 
        style = "vertical-align: middle;"),
    "Safety Narrative Generator"
  )),
  
  # Navigation Bar
  navbarPage(
    "Navigation",
    
    # Home Page ----
    tabPanel(
      "Home",
      fluidPage(
        h2("Safety Narrative Generator"),
        div(
          class = "well",
          h4("Welcome to the Safety Narrative Generator"),
          p("This application helps you generate standardized safety narratives from your clinical data."),
          p("To get started:"),
          tags$ol(
            tags$li("Upload your data in the 'Data Input' tab"),
            tags$li("Review or modify the template in the 'Template' tab"),
            tags$li("Generate and download narratives in the 'Narratives' tab")
          )
        )
      )
    ),
    
    # Description Page ----
    tabPanel(
      "Description",
      fluidPage(
        h2("About This Application"),
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h3("Purpose"),
              p("This Safety Narrative Generator is designed to automate the creation of standardized safety narratives 
                for clinical trials and medical research. It helps clinical researchers and medical writers generate 
                consistent, well-formatted narratives from structured data."),
              
              h3("Features"),
              tags$ul(
                tags$li(strong("Data Import:"), " Support for CSV files with flexible separator options"),
                tags$li(strong("Customizable Templates:"), " Edit and save narrative templates with variable placeholders"),
                tags$li(strong("Batch Processing:"), " Generate narratives for multiple subjects at once"),
                tags$li(strong("Export Options:"), " Download narratives in text format with proper formatting")
              ),
              
              h3("Required Data Format"),
              p("Your input CSV file should contain the following columns:"),
              tags$ul(
                tags$li(strong("USUBJID:"), " Subject identifier"),
                tags$li(strong("AGE:"), " Subject age"),
                tags$li(strong("SEX:"), " Subject gender"),
                tags$li(strong("AETERM:"), " Adverse event term"),
                tags$li(strong("ASTDT:"), " AE start date"),
                tags$li(strong("AESER:"), " Seriousness assessment"),
                tags$li(strong("AEREL:"), " Relatedness to study drug"),
                tags$li(strong("AEOUT:"), " Event outcome")
              ),
              
              h3("Contact"),
              p("For questions or support, please contact: pengvirginia@gmail.com")
            )
          )
        )
      )
    ),
    
    # Data Input Page ----
    tabPanel(
      "Data Input",
      fluidPage(
        h2("Upload Data"),
        fluidRow(
          # File Upload Panel
          column(
            width = 4,
            wellPanel(
              # Modified file input with clearer multiple file support
              fileInput("dataFile", 
                       label = div(
                         "Choose Files (Multiple files allowed)",
                         tags$br(),
                         tags$small("Hold Ctrl/Cmd to select multiple files")
                       ),
                       multiple = TRUE,
                       accept = c(
                         ".csv",
                         ".xls",
                         ".xlsx",
                         "text/csv",
                         "text/comma-separated-values",
                         "application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                       ),
                       buttonLabel = "Browse...",
                       placeholder = "No files selected"
              ),
              
              # Add a clear files button
              actionButton("clearFiles", "Clear All Files",
                          class = "btn-warning btn-sm btn-block",
                          style = "margin-bottom: 10px"),
              
              tags$hr(),
              
              # File type detection
              radioButtons("fileType", "File Type:",
                         choices = c(
                           "Auto-detect" = "auto",
                           "CSV" = "csv",
                           "Excel" = "excel"
                         ),
                         selected = "auto"
              ),
              
              # CSV specific options
              conditionalPanel(
                condition = "input.fileType != 'excel'",
                checkboxInput("header", "Header", TRUE),
                radioButtons("sep", "Separator",
                           choices = c(
                             Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"
                           ),
                           selected = ","),
                radioButtons("quote", "Quote",
                           choices = c(
                             None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"
                           ),
                           selected = '"')
              ),
              
              # Excel specific options
              conditionalPanel(
                condition = "input.fileType == 'excel'",
                selectInput("sheet", "Sheet:", choices = NULL)
              ),
              
              # Process button
              actionButton("processFiles", "Process Files", 
                         class = "btn-primary btn-block",
                         style = "margin-top: 10px")
            )
          ),
          
          # Data Preview Panel
          column(
            width = 8,
            wellPanel(
              h4("Uploaded Files"),
              DTOutput("fileList"),
              tags$hr(),
              # Add dataset selector
              uiOutput("datasetSelector"),
              tags$hr(),
              h4("Data Preview"),
              DTOutput("uploadedData")
            )
          )
        )
      )
    ),
    
    # Template Page ----
    tabPanel(
      "Template",
      fluidPage(
        h2("Narrative Template"),
        fluidRow(
          column(
            width = 8,
            wellPanel(
              textAreaInput(
                "template",
                "Edit Template:",
                value = paste(
                  "Subject ID: {USUBJID}, a {AGE} year old {SEX},",
                  "experienced an adverse event of {AETERM} on {ASTDT}.",
                  "The event was considered {AESER} and {AEREL} to the study drug.",
                  "The outcome was {AEOUT}."
                ),
                width = "100%",
                height = "200px"
              )
            )
          ),
          column(
            width = 4,
            wellPanel(
              h4("Available Variables"),
              tags$ul(
                tags$li("{USUBJID} - Subject ID"),
                tags$li("{AGE} - Age"),
                tags$li("{SEX} - Gender"),
                tags$li("{AETERM} - Adverse Event Term"),
                tags$li("{ASTDT} - AE Start Date"),
                tags$li("{AESER} - Seriousness"),
                tags$li("{AEREL} - Relatedness"),
                tags$li("{AEOUT} - Outcome")
              )
            )
          )
        )
      )
    ),
    
    # Narratives Page ----
    tabPanel(
      "Narratives",
      fluidPage(
        h2("Generated Narratives"),
        fluidRow(
          column(
            width = 3,
            wellPanel(
              selectInput("subjectSelect", "Select Subject:", choices = NULL),
              downloadButton("downloadNarratives", "Download All Narratives", 
                           class = "btn-primary btn-block")
            )
          ),
          column(
            width = 9,
            wellPanel(
              verbatimTextOutput("narrative")
            )
          )
        )
      )
    ),

    # Visualization Page ----
    tabPanel(
      "Visualizations",
      fluidPage(
        h2("Subject Count and Exposure-Adjusted Incidence Rate"),
        fluidRow(
          column(
            width = 12,
            wellPanel(
              # Add filters
              fluidRow(
                column(4,
                  selectInput("aeFilter", "Filter by AE Category:",
                            choices = NULL,
                            multiple = TRUE)
                ),
                column(4,
                  selectInput("serFilter", "Seriousness:",
                            choices = NULL,
                            multiple = TRUE)
                )
              ),
              
              # Table output
              h4("Subject Count and Exposure-Adjusted Incidence Rate"),
              DTOutput("aeTable"),
              
              # Plot output
              h4("Visualization"),
              plotOutput("aePlot", height = "600px")
            )
          )
        )
      )
    )
  )
)

# Server Logic ----
server <- function(input, output, session) {
  # Reactive Values
  rv <- reactiveValues(
    data_list = list(),  # Store multiple datasets
    active_data = NULL,  # Currently selected dataset
    files = data.frame(
      filename = character(),
      size = character(),
      type = character(),
      status = character(),
      stringsAsFactors = FALSE
    ),
    ae_summary = NULL  # New reactive value for AE summary
  )
  
  # File List Display ----
  output$fileList <- renderDT({
    datatable(
      rv$files,
      options = list(
        pageLength = 5,
        dom = 't',
        ordering = FALSE
      ),
      selection = 'none'
    )
  })
  
  # Sheet Names Update ----
  observeEvent(input$dataFile, {
    req(input$dataFile)
    
    # Update files table
    new_files <- data.frame(
      filename = input$dataFile$name,
      size = paste(round(input$dataFile$size / 1024, 1), "KB"),
      type = tools::file_ext(input$dataFile$name),
      status = "Pending",
      stringsAsFactors = FALSE
    )
    
    rv$files <- rbind(rv$files, new_files)
    
    # Update sheet selection if Excel files
    excel_files <- input$dataFile$datapath[tools::file_ext(input$dataFile$name) %in% c("xls", "xlsx")]
    if (length(excel_files) > 0) {
      sheets <- readxl::excel_sheets(excel_files[1])
      updateSelectInput(session, "sheet", choices = sheets)
    }
  })
  
  # Add dataset selector UI
  output$datasetSelector <- renderUI({
    req(length(rv$data_list) > 0)
    selectInput("activeDataset", 
                "Select Dataset:",
                choices = names(rv$data_list),
                selected = names(rv$data_list)[1])
  })
  
  # Process Files Handler ----
  observeEvent(input$processFiles, {
    req(input$dataFile)
    
    # Process each file
    for (i in seq_len(nrow(rv$files))) {
      tryCatch({
        file_path <- input$dataFile$datapath[i]
        file_ext <- tools::file_ext(input$dataFile$name[i])
        filename <- input$dataFile$name[i]
        
        # Read data based on file type
        if (file_ext %in% c("xls", "xlsx")) {
          data <- readxl::read_excel(file_path, sheet = input$sheet)
        } else {
          data <- read.csv(
            file_path,
            header = input$header,
            sep = input$sep,
            quote = input$quote,
            stringsAsFactors = FALSE
          )
        }
        
        # Convert all column names to uppercase
        names(data) <- toupper(names(data))
        
        # Validate required columns exist
        required_cols <- c("USUBJID", "AGE", "SEX", "AETERM", "ASTDT", "AESER", "AEREL", "AEOUT")
        missing_cols <- required_cols[!required_cols %in% names(data)]
        
        if (length(missing_cols) > 0) {
          rv$files$status[i] <- paste("Error: Missing columns:", paste(missing_cols, collapse = ", "))
          next
        }
        
        # Store in data_list
        rv$data_list[[filename]] <- data
        
        # Update status
        rv$files$status[i] <- paste("Processed -", length(unique(data$USUBJID)), "subjects")
        
      }, error = function(e) {
        rv$files$status[i] <- paste("Error:", e$message)
      })
    }
    
    # Set active data to first dataset
    if (length(rv$data_list) > 0) {
      rv$active_data <- rv$data_list[[1]]
      
      # Update subject selector based on active dataset
      updateSelectInput(session, "subjectSelect",
                       choices = unique(rv$active_data$USUBJID))
    }
    
    # Show processing summary
    showModal(modalDialog(
      title = "Data Processing Complete",
      sprintf("Processed %d files successfully.", length(rv$data_list)),
      easyClose = TRUE
    ))
  })
  
  # Update active dataset when selection changes
  observeEvent(input$activeDataset, {
    req(input$activeDataset)
    rv$active_data <- rv$data_list[[input$activeDataset]]
    
    # Update subject selector based on active dataset
    updateSelectInput(session, "subjectSelect",
                     choices = unique(rv$active_data$USUBJID))
    
    # Calculate AE summary statistics and update visualization filters
    if (!is.null(rv$active_data)) {
      # Update filter choices with sorted unique values
      updateSelectInput(session, "aeFilter",
                       choices = sort(unique(rv$active_data$AETERM)),
                       selected = NULL)
      updateSelectInput(session, "serFilter",
                       choices = sort(unique(rv$active_data$AESER)),
                       selected = NULL)
      
      # Calculate total number of subjects for EAIR calculation
      total_subjects <- n_distinct(rv$active_data$USUBJID)
      
      # Calculate summary statistics
      ae_summary <- rv$active_data %>%
        group_by(AETERM, AESER) %>%
        summarise(
          Subject_Count = n_distinct(USUBJID),
          Total_Events = n(),
          Percentage = round(Subject_Count / total_subjects * 100, 2),
          .groups = 'drop'
        ) %>%
        mutate(
          # Assuming 100 person-years of exposure - you should replace this with actual exposure time
          Exposure_Time = 100,
          # Calculate exposure-adjusted incidence rate per 100 person-years
          EAIR = round((Subject_Count / Exposure_Time) * 100, 2)
        ) %>%
        arrange(desc(Subject_Count))
      
      rv$ae_summary <- ae_summary
    }
  })
  
  # Filtered AE summary
  filtered_ae_summary <- reactive({
    req(rv$ae_summary)
    
    summary_data <- rv$ae_summary
    
    # Apply filters if selected
    if (!is.null(input$aeFilter) && length(input$aeFilter) > 0) {
      summary_data <- summary_data %>%
        filter(AETERM %in% input$aeFilter)
    }
    
    if (!is.null(input$serFilter) && length(input$serFilter) > 0) {
      summary_data <- summary_data %>%
        filter(AESER %in% input$serFilter)
    }
    
    summary_data
  })
  
  # Render AE summary table
  output$aeTable <- renderDT({
    req(filtered_ae_summary())
    
    datatable(
      filtered_ae_summary() %>%
        select(
          `Adverse Event` = AETERM,
          Seriousness = AESER,
          `Subject Count` = Subject_Count,
          `Total Events` = Total_Events,
          `Percentage (%)` = Percentage,
          `EAIR (per 100 person-years)` = EAIR
        ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        order = list(list(2, 'desc'))  # Sort by Subject Count by default
      ),
      rownames = FALSE
    )
  })
  
  # Render AE summary plot
  output$aePlot <- renderPlot({
    req(filtered_ae_summary())
    
    # Get top 20 AEs by subject count for better visualization
    plot_data <- filtered_ae_summary() %>%
      arrange(desc(Subject_Count)) %>%
      group_by(AETERM) %>%
      mutate(Total_Count = sum(Subject_Count)) %>%
      ungroup() %>%
      arrange(desc(Total_Count)) %>%
      slice_head(n = 20)
    
    ggplot(plot_data, 
           aes(x = reorder(AETERM, Subject_Count), 
               y = Subject_Count, 
               fill = AESER)) +
      geom_bar(stat = "identity", position = "stack") +
      coord_flip() +
      labs(
        title = "Top 20 Adverse Events by Subject Count",
        x = "Adverse Event Term",
        y = "Number of Subjects",
        fill = "Seriousness"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Modify Data Display to use active dataset
  output$uploadedData <- renderDT({
    req(rv$active_data)
    datatable(
      rv$active_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    )
  })
  
  # Modify Narrative Generation to use active dataset
  output$narrative <- renderText({
    req(rv$active_data, input$subjectSelect, input$template)
    
    # Get all events for the selected subject
    subject_events <- rv$active_data[rv$active_data$USUBJID == input$subjectSelect, ]
    
    if (nrow(subject_events) == 0) {
      return("No events found for selected subject.")
    }
    
    # Get subject demographics from first row
    subject_demo <- subject_events[1, ]
    
    # Create event narratives
    event_narratives <- lapply(seq_len(nrow(subject_events)), function(i) {
      event_data <- subject_events[i, ]
      event_narrative <- sprintf(
        "\n\nEvent %d: Subject experienced an adverse event of %s on %s. The event was considered %s and %s to the study drug. The outcome was %s.",
        i,
        event_data$AETERM,
        event_data$ASTDT,
        event_data$AESER,
        event_data$AEREL,
        event_data$AEOUT
      )
      return(event_narrative)
    })
    
    # Create the main narrative
    main_narrative <- sprintf(
      "Subject ID: %s, a %s year old %s experienced the following adverse events:",
      subject_demo$USUBJID,
      subject_demo$AGE,
      subject_demo$SEX
    )
    
    # Combine all narratives
    paste0(
      main_narrative,
      paste(unlist(event_narratives), collapse = "")
    )
  })
  
  # Download Handler ----
  output$downloadNarratives <- downloadHandler(
    filename = function() {
      paste0("safety-narratives-", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      req(rv$data_list, input$template)
      
      all_narratives <- sapply(names(rv$data_list), function(name) {
        data <- rv$data_list[[name]]
        
        # Process each subject's events
        subject_narratives <- data %>%
          group_by(USUBJID) %>%
          group_map(function(subject_events, key) {
            # Get subject demographics from first row
            subject_demo <- subject_events[1, ]
            
            # Create event narratives
            event_narratives <- sapply(1:nrow(subject_events), function(i) {
              event_data <- subject_events[i, ]
              event_template <- paste(
                "Event", i, ":",
                "experienced an adverse event of {AETERM} on {ASTDT}.",
                "The event was considered {AESER} and {AEREL} to the study drug.",
                "The outcome was {AEOUT}."
              )
              
              # Replace event-specific information
              event_cols <- c("AETERM", "ASTDT", "AESER", "AEREL", "AEOUT")
              for(col in event_cols) {
                if(col %in% names(event_data)) {
                  pattern_upper <- paste0("\\{", toupper(col), "\\}")
                  pattern_orig <- paste0("\\{", col, "\\}")
                  value <- as.character(event_data[[col]])[1]
                  event_template <- gsub(pattern_upper, value, event_template, fixed = TRUE)
                  event_template <- gsub(pattern_orig, value, event_template, fixed = TRUE)
                }
              }
              event_template
            })
            
            # Combine demographics with all events
            paste(
              paste("Subject ID:", subject_demo$USUBJID, ", a", subject_demo$AGE, "year old", subject_demo$SEX, 
                    "experienced the following adverse events:"),
              paste(event_narratives, collapse = "\n\n"),
              "\n\n"
            )
          }) %>%
          paste(collapse = "\n")
        
        paste0("Dataset ", name, ":\n", subject_narratives, "\n\n")
      })
      
      writeLines(all_narratives, file)
    }
  )
  
  # Clear Files Handler ----
  observeEvent(input$clearFiles, {
    rv$files <- data.frame(
      filename = character(),
      size = character(),
      type = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
    rv$data_list <- list()
    rv$active_data <- NULL
    updateSelectInput(session, "subjectSelect", choices = NULL)
  })
}

# Run Application ----
shinyApp(ui = ui, server = server)


