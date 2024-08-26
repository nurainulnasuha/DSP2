library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(scales)
library(fresh)
library(plotly)
library(shinyjs)
library(readr)
library(ggrepel)


# Load FEV and Hybrid vehicle datasets
fev_data <- read_csv("FEV_DATA.csv")
hybrid_data <- read_csv("HYBRID_DATA.csv")

fev_data$Price <- as.numeric(gsub("[RM,]", "", fev_data$Price))
hybrid_data$Price <- as.numeric(gsub("[RM,]", "", hybrid_data$Price))
fev_data$EnginePower <- as.numeric(fev_data$EnginePower)
hybrid_data$EnginePower <- as.numeric(hybrid_data$EnginePower)

# Creating a combined dataset for certain plots
fev_data$Type <- "FEV"
hybrid_data$Type <- "Hybrid"
combined_data <- bind_rows(fev_data, hybrid_data)


# Define UI for application using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Car Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Vehicles Price", tabName = "vehicles_price", icon = icon("dollar-sign")),
                menuItem("Performance", tabName = "performance", icon = icon("tachometer-alt")),
                menuItem("Safety", tabName = "safety", icon = icon("shield-alt")),
                menuItem("Our Dataset", tabName = "our_dataset", icon = icon("database")),
                menuItem("About Us", tabName = "about_us", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "vehicles_price",
              tabBox(
                id = "vehicles_price_tabs",
                width = 12,
                tabPanel("FEV Price",
                         selectInput("fev_brand", "Select Brand:", choices = unique(fev_data$Brand)),
                         plotOutput("pricePlotFEV")
                ),
                tabPanel("Hybrid Price",
                         selectInput("hybrid_brand", "Select Brand:", choices = unique(hybrid_data$Brand)),
                         plotOutput("pricePlotHybrid")
                ),
                tabPanel("Price Comparison",
                         plotOutput("sidebysideBarChart")
                ),
                tabPanel("Price Distribution",
                         plotlyOutput("priceBoxplot")
                )
              )
      ),
      tabItem(tabName = "performance",
              titlePanel("Engine Power and Price Comparison: FEV vs Hybrid"),
              mainPanel(
                tabsetPanel(
                  tabPanel("Top 5 FEV Engine Power", plotlyOutput("top5Fev")),
                  tabPanel("Top 5 Hybrid Engine Power", plotlyOutput("top5Hybrid")),
                  tabPanel("FEV Price vs Engine Power", plotlyOutput("fevScatterPlot")),
                  tabPanel("Hybrid Price vs Engine Power", plotlyOutput("hybridScatterPlot"))
                )
              )
      ),
      tabItem(tabName = "safety",
              tabPanel("Acceleration Distribution",
                       plotOutput("acceleration_boxplot")
              ),
              tabPanel("Price vs Acceleration Scatter Plot",
                       plotOutput("price_acceleration_scatterplot")
              )
      ),
      tabItem(tabName = "our_dataset",
              tabBox(
                id = "dataset_tabs",
                width = 12,
                tabPanel("FEV Dataset",
                         DT::dataTableOutput("fev_data_table")
                ),
                tabPanel("Hybrid Dataset",
                         DT::dataTableOutput("hybrid_data_table")
                )
              )
      ),
      tabItem(tabName = "about_us",
              fluidRow(
                column(12, align = "center",
                       tags$div(
                         style = "background-color: #cfe2f3; border-radius: 20px; padding: 30px;",
                         tags$h2("About Us", style = "color: #333; font-weight: bold;"),
                         tags$p(
                           "Our project delves deep into the analysis and comparison of Full Electric Vehicles (FEVs) and Hybrid Vehicles, examining crucial factors such as Brand, Model, Price, Engine Power, Battery Capacity, Range, and Safety Features. By seamlessly integrating data science techniques with a user-friendly graphical interface (GUI), we aim to illuminate the distinctions between FEVs and Hybrid Vehicles across various dimensions. Our endeavor is rooted in supporting the global transition towards sustainable transportation and meeting the surging demand for electric and hybrid vehicles. Through intuitive and visually appealing presentations, our project empowers stakeholders to make well-informed decisions about these innovative vehicle technologies."
                         )
                       )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$pricePlotFEV <- renderPlot({
    selected_brand <- input$fev_brand
    filtered_data <- fev_data[fev_data$Brand == selected_brand, ]
    plot_title <- paste("Price against Model for", selected_brand)
    
    filtered_data <- filtered_data %>% mutate(Model = reorder(Model, -Price))
    
    ggplot(filtered_data, aes(x = Model, y = Price, fill = Model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste("RM", scales::comma(Price))), vjust = -0.3, size = 3.5) +  
      labs(title = plot_title, x = "Model", y = "Price (RM)") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$pricePlotHybrid <- renderPlot({
    selected_brand <- input$hybrid_brand
    filtered_data <- hybrid_data[hybrid_data$Brand == selected_brand, ]
    plot_title <- paste("Price against Model for", selected_brand)
    
    filtered_data <- filtered_data %>% mutate(Model = reorder(Model, -Price))
    
    ggplot(filtered_data, aes(x = Model, y = Price, fill = Model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste("RM", scales::comma(Price))), vjust = -0.3, size = 3.5) +  
      labs(title = plot_title, x = "Model", y = "Price (RM)") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$sidebysideBarChart <- renderPlot({
    summary_data <- combined_data %>%
      group_by(Brand, Type) %>%
      summarise(AveragePrice = mean(Price, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(AveragePrice)) %>%
      mutate(Brand = factor(Brand, levels = unique(Brand)))
    
    ggplot(summary_data, aes(x = Brand, y = AveragePrice, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste("RM", scales::comma(AveragePrice))), angle = 90, vjust = 0.5, hjust = 1.2, size = 3.5, position = position_dodge(width = 0.9)) +  
      labs(title = "Side by Side Bar Chart for FEV and Hybrid Average Price",
           x = "Brand",
           y = "Average Price (RM)") +
      scale_y_continuous(labels = scales::comma_format(prefix = "RM")) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  output$priceBoxplot <- renderPlotly({
    ggplot(combined_data, aes(x = Type, y = Price, fill = Type)) +
      geom_boxplot() +
      labs(title = "Price Distribution: FEV vs Hybrid", x = "Type", y = "Price (RM)") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  
  output$top5Fev <- renderPlotly({
    top5_fev <- fev_data[order(-fev_data$EnginePower), ][1:5, ]
    p <- ggplot(top5_fev, aes(x = EnginePower, y = reorder(Model, EnginePower), fill = EnginePower)) +
      geom_bar(stat = "identity") +
      labs(x = "Engine Power (HP)", y = "Model") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),   # Remove major grid lines
            panel.grid.minor = element_blank()) + # Remove minor grid lines
      scale_fill_gradient(low = "lightpink", high = "hotpink4") +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  
  output$top5Hybrid <- renderPlotly({
    top5_hybrid <- hybrid_data[order(-hybrid_data$EnginePower), ][1:5, ]
    p <- ggplot(top5_hybrid, aes(x = EnginePower, y = reorder(Model, EnginePower), fill = EnginePower)) +
      geom_bar(stat = "identity") +
      labs(x = "Engine Power (HP)", y = "Model") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),   # Remove major grid lines
            panel.grid.minor = element_blank(),   # Remove minor grid lines
            panel.background = element_blank()) + # Remove background grid
      scale_fill_gradient(low = "cornsilk", high = "burlywood3") +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  
  output$fevScatterPlot <- renderPlotly({
    p <- ggplot(fev_data, aes(x = Price, y = EnginePower)) +
      geom_point(aes(color = EnginePower)) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = "FEV: Price vs Engine Power", x = "Price (RM)", y = "Engine Power (HP)") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),   # Remove major grid lines
            panel.grid.minor = element_blank(),   # Remove minor grid lines
            panel.background = element_blank()) + # Remove background grid
      scale_color_gradient(low = "lightpink", high = "hotpink4") +
      scale_x_continuous(labels = scales::comma)
    ggplotly(p)
  })
  
  
  # Generate Hybrid scatter plot for price vs engine power with trend line
  output$hybridScatterPlot <- renderPlotly({
    p <- ggplot(hybrid_data, aes(x = Price, y = EnginePower)) +
      geom_point(aes(color = EnginePower)) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = "Hybrid: Price vs Engine Power", x = "Price (RM)", y = "Engine Power (HP)") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),   # Remove major grid lines
            panel.grid.minor = element_blank(),   # Remove minor grid lines
            panel.background = element_blank()) + # Remove background grid
      scale_color_gradient(low = "cornsilk1", high = "burlywood3") +
      scale_x_continuous(labels = scales::comma)
    ggplotly(p)
  })
  
  #boxplot 
  output$acceleration_boxplot <- renderPlot({
    ggplot(combined_data, aes(x = Type, y = Acceleration, fill = Type)) +
      geom_boxplot() +
      geom_text(stat = "summary", aes(label = paste("Median:", after_stat(y))), 
                position = position_nudge(y = 0.5), vjust = -0.5) + # Add label for median
      labs(
        title = "Acceleration Comparison by Vehicle Type",
        x = "Vehicle Type",
        y = "Acceleration (0-60 mph in seconds)",
        fill = "Vehicle Type"
      ) +
      theme_minimal()
  })
  
  #scatter plot price vs acceleration
  output$price_acceleration_scatterplot <- renderPlot({
    ggplot(combined_data, aes(x = Price, y = Acceleration, color = Type)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) + # Add a trend line using linear regression
      labs(
        title = "Price vs Acceleration Scatter Plot by Car Type",
        x = "Price",
        y = "Acceleration (0-60 mph in seconds)",
        color = "Vehicle Type"
      ) +
      theme_minimal() +
      scale_x_continuous(labels = scales::number_format(accuracy = 1)) + # Format x-axis labels
      scale_y_continuous(labels = scales::number_format(accuracy = 1))   # Format y-axis labels
  })
  
  
  output$fev_data_table <- DT::renderDataTable({
    DT::datatable(fev_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$hybrid_data_table <- DT::renderDataTable({
    DT::datatable(hybrid_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
