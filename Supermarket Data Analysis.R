# Load necessary libraries
library(ggplot2)  # For data visualization
library(dplyr)    # For data manipulation
library(shiny)    # For building the Shiny app
library(arules)   # For association rule mining
library(cluster)  # For clustering analysis
library(shinydashboard)  # For creating dashboard layouts
library(bslib)    # For enhanced Bootstrap themes
library(DT)       # For displaying tables interactively

# ---------------------- UI ----------------------

# Create the Shiny app user interface (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Grocery Analysis Dashboard"),  # Dashboard header with title
  dashboardSidebar(
    sidebarMenu( # the mame will appear,     the id will connect between other things, icon will appear     
      menuItem("Upload Data File", tabName = "file_upload", icon = icon("file-upload")),  # File Upload menu item,,tap nameh دا id بتربط بيه وبين اي حاجه عايزها بعد كدا زي الbody
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-bar")),  # Visualization tab
      menuItem("Clustering", tabName = "clustering", icon = icon("users")),  # Clustering tab
      menuItem("Association Rules", tabName = "association", icon = icon("link"))  # Association rules tab
    )
  ),
  dashboardBody(
    tabItems(
      # File Upload Tab
      tabItem(
        tabName = "file_upload",
        fluidRow(#the name will appear ,   give it a color based on the theme , the header color of the item
          box(title = "Upload Data File", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              fileInput("file_input", "Choose CSV File", accept = ".csv"), width = 12)
                        # id ,the name 
        )
      ),
      # Visualization Tab
      tabItem(
        tabName = "visualization",
        fluidRow(
          box(title = "Cash vs Credit", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("cashCreditPlot", height = 300), width = 6),
          box(title = "Age vs Total Spending", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("ageSpendingPlot", height = 300), width = 6)
        ),
        fluidRow(
          box(title = "Total Spending by City", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("citySpendingPlot", height = 300), width = 6),
          box(title = "Distribution of Total Spending", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("distributionPlot", height = 300), width = 6)
        )
      ),
      # Clustering Tab
      tabItem(
        tabName = "clustering",
        fluidRow(
          box(
            title = "Clustering Configuration",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            textInput("n_clusters", "Enter the number of clusters:", value = "3"),
            actionButton("run_clustering", "Run Clustering"),
            width = 6
          )
        ),
        dataTableOutput("clusterTable")  # Table displaying clustering results
      ),
      # Association Rules Tab
      tabItem(
        tabName = "association",
        fluidRow(
          box(
            title = "Generate Association Rules",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            numericInput("min_support", "Minimum Support:", value = 0.01, min = 0.001, max = 1, step = 0.01),
            numericInput("min_confidence", "Minimum Confidence:", value = 0.8, min = 0.001, max = 1, step = 0.01),
            actionButton("generate_rules", "Generate Rules"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Association Rules Table",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("rulesTable"),
            width = 12
          )
        ),
        verbatimTextOutput("rulesOutput")  # Display raw association rules output
      )
    )
  )
)

# ---------------------- Server ----------------------

# Define the server-side logic for the Shiny app
server <- function(input, output) {
  
  # Reactive expression to read and clean the uploaded file
  data <- reactive({
    req(input$file_input)  # Ensure the file is uploaded
    dataset <- read.csv(input$file_input$datapath)  # Read the uploaded CSV file
    cleaned_data <- dataset %>%
      distinct() %>%          # Remove duplicates
      na.omit() %>%           # Remove rows with missing values
      mutate(
        age = as.numeric(age),  # Ensure "age" is numeric
        total = as.numeric(total)  # Ensure "total" is numeric
      )
    return(cleaned_data)
  })
  
  # Create plots for data visualization
  output$cashCreditPlot <- renderPlot({
    data <- data()  
    
    ggplot(data, aes(x = paymentType, y = total, fill = paymentType)) +
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = c("Cash" = "#8D0B41", "Credit" = "#D39D55")) + 
      labs(title = "Comparison of Cash and Credit Totals", x = "Payment Type", y = "Total Spending")
  })
  
  output$ageSpendingPlot <- renderPlot({
    data <- data()  
    
    ggplot(data %>% group_by(age) %>% summarise(Total = sum(total)), aes(x = age, y = Total)) +
      geom_line(color = "#441752", size = 1) +   
      geom_point(color = "#AB4459", size = 2) +  
      labs(title = "Age vs Total Spending", x = "Age", y = "Total Spending")
  })
  
  output$citySpendingPlot <- renderPlot({
    data <- data()  
    
    ggplot(data %>% group_by(city) %>% summarise(Total = sum(total)) %>% arrange(desc(Total)), 
           aes(x = reorder(city, -Total), y = Total, fill = city)) +
      geom_bar(stat = "identity") +  
      labs(title = "Total Spending by City", x = "City", y = "Total Spending") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$distributionPlot <- renderPlot({
    data <- data()  
    
    ggplot(data, aes(x = total)) +
      geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +  
      labs(title = "Distribution of Total Spending", x = "Total Spending", y = "Frequency")
  })
  
  # Handle clustering logic
  observeEvent(input$run_clustering, {
    data <- data()  # Access the cleaned data
    customer_data <- data %>%
      group_by(customer, age) %>%
      summarise(total_spending = sum(total), .groups = "drop")
    
    n_clusters <- as.integer(input$n_clusters)  # Get number of clusters from user
    if (is.na(n_clusters) || n_clusters < 2 || n_clusters > 4) {  # Validate user input
      showNotification("Please enter a valid number of clusters (2-4).", type = "error")
    } else {
      set.seed(123)  # Ensure reproducibility
      kmeans_result <- kmeans(customer_data[, c("age", "total_spending")], centers = n_clusters)  # Perform k-means clustering
      customer_data$Cluster <- as.factor(kmeans_result$cluster)  # Assign cluster labels
      cluster_table <- customer_data %>% select(customer, age, total_spending, Cluster)  # Prepare output table
      output$clusterTable <- renderDataTable({ cluster_table })  # Display clustering results
      showNotification("Clustering completed successfully!", type = "message")
    }
  })
  
  # Handle association rules logic
  observeEvent(input$generate_rules, {
    data <- data()  # Access the cleaned data
    
    min_support <- input$min_support  # Get minimum support value from user
    min_confidence <- input$min_confidence  # Get minimum confidence value from user
    
    if (min_support < 0.001 || min_support > 1 || min_confidence < 0.001 || min_confidence > 1) {  # Validate input
      showNotification("Please enter valid support and confidence values (0.001-1).", type = "error")
    } else {
      rules <- apriori(
        data.frame(data),
        parameter = list(supp = min_support, conf = min_confidence)
      )
      
      # Convert rules to a data frame for easier display
      rules_df <- as(rules, "data.frame")
      
      # Render rules table in the UI
      output$rulesTable <- DT::renderDataTable({
        DT::datatable(rules_df, options = list(pageLength = 10, scrollX = TRUE))
      })
    }
  })
}

# ---------------------- Run App ----------------------

# Launch the Shiny app
shinyApp(ui, server)
