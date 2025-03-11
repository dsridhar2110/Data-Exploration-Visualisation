if (!require(shiny)) install.packages("shiny")
if (!require(highcharter)) install.packages("highcharter")
if (!require(dplyr)) install.packages("dplyr")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(sf)) install.packages("sf")
if (!require(viridis)) install.packages("viridis")
if (!require(readxl)) install.packages("readxl")
if (!require(plotly)) install.packages("plotly")
if (!require(tsibble)) install.packages("tsibble")
if (!require(hrbrthemes)) install.packages("hrbrthemes")
if (!require(feasts)) install.packages("feasts")
if (!require(htmlwidgets)) install.packages("htmlwidgets")
if (!require(lubridate)) install.packages("lubridate")
if (!require(fable)) install.packages("fable")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(scales)) install.packages("scales")
if (!require(plotly)) install.packages("feasts")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(highcharter)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(viridis)
library(lubridate)
library(htmlwidgets)
library(tsibble)
library(fable)
library(feasts)
library(shinyWidgets)
library(readxl)
library(sf)


# Data
df_year <- data.frame(
  Year = rep(2017:2024, times = 4),
  Calls = c(21127462, 19981065, 18031134, 18731091, 17690733, 13501237, 12406581, 10400678,
            143164596, 143847477, 145102640, 150674443, 138127253, 141774261, 141633177, 142492233,
            5086983, 8369223, 9052320, 9235755, 8572384, 9344567, 10772124, 12420634,
            98646, 188646, 581151, 492328, 507969, 529874, 838796, 1327822),
  CallType = rep(c("Wireline", "Wireless", "VoIP", "Texts"), each = 8)
)

# Load spatial data
hex_grid_path <- "us_states_hexgrid.geojson"
my_sf <- read_sf(hex_grid_path)

# Clean up the state names
my_sf <- my_sf %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name, perl = TRUE))

# Read PSAP dataset
psap_data <- read_excel("state_responses_columns.xlsx")

# Handle missing values
psap_data <- psap_data %>%
  mutate(Primary = as.numeric(ifelse(Primary == "x", NA, Primary)),
         Secondary = as.numeric(ifelse(Secondary == "x", NA, Secondary)))

# Join the hex grid with PSAP data
my_sf_psap <- my_sf %>%
  left_join(psap_data, by = c("google_name" = "State"))

# Bin the Primary and Secondary PSAP data into custom ranges
my_sf_psap <- my_sf_psap %>%
  mutate(Primary_bin = cut(Primary, 
                           breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, Inf),
                           labels = c("0-50", "50-100", "100-150", "150-200", "200-250", "250-300", "300-350", "350-400", "400-450", "450-500", "Information not provided"),
                           include.lowest = TRUE),
         Secondary_bin = cut(Secondary, 
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
                             labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "Information not provided"),
                             include.lowest = TRUE))

# Define color palettes
primary_palette <- rev(magma(11))[c(-1, -11)]  # Adjusted palette to handle bins properly
secondary_palette <- rev(magma(9))[c(-1, -9)] 

us_states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Load the call types data from CSV
call_data <- read.csv("call_types_state_wise.csv")

# Load dataset from CSV file
crime_data <- read.csv("combined.csv")

# Add a tooltip text column for interactivity
crime_data <- crime_data %>%
  mutate(text = paste0("Day: ", Day, "\nCrime: ", Crime, "\nCount: ", Count))

data <- read.csv("Daily_Averages_combined.csv")

# Step 1: Convert DATE column to Date type
data$DATE <- as.Date(data$DATE, format = "%d/%m/%Y")

# Step 2: Remove duplicates if necessary (keeping only one record per day)
data <- data %>%
  distinct(DATE, .keep_all = TRUE)

# Step 3: Apply log transformation to AVG_RESP to stabilize variance and create a tsibble object
data$AVG_RESP <- log(data$AVG_RESP + 1)
clean_data <- data %>%
  as_tsibble(index = DATE) %>%
  fill_gaps()

# Step 4: Decompose the time series into trend, seasonality, and remainder
decomposition <- clean_data %>%
  model(STL(AVG_RESP ~ season(window = "periodic")))

# Step 5: View the decomposition components
decomp_model <- components(decomposition)

# Step 6: Fit SARIMA model to the seasonally adjusted component of the time series
decomp_model_adjusted <- decomp_model %>%
  select(-.model)  # Remove the STL model info

sarima_model <- decomp_model_adjusted %>%
  model(sarima = ARIMA(season_adjust ~ pdq(1,1,1) + PDQ(1,1,0)))

# Step 7: Forecast for the next 60 days with SARIMA model
sarima_forecast_result <- sarima_model %>%
  forecast(h = 60)

# Convert the forecast to a dataframe for plotting
forecast_data <- sarima_forecast_result %>%
  as_tibble() %>%
  rename(AVG_RESP = .mean) %>%
  mutate(Type = "Forecast")

# Combine actual and forecast data for plotting
combined_data <- bind_rows(
  data %>% mutate(Type = "Actual"),
  forecast_data %>% mutate(DATE = as.Date(DATE)) # Ensure DATE is Date type
)

# Filter the data to only show from January onwards
combined_data <- combined_data %>%
  filter(DATE >= as.Date("2024-01-01"))

# Extract unique months available in the dataset for the slider
unique_months <- unique(format(combined_data$DATE, "%Y-%m"))

# Load your second dataset (assuming you have 'hour' and 'response_time' columns)
df <- read.csv('Hourly_Averages_combined.csv')

# One-hot encode the 'hour' column using model.matrix()
df_onehot <- as.data.frame(model.matrix(~ as.factor(df$hour) - 1))

# Combine the one-hot encoded columns with the response time
df_combined <- cbind(df_onehot, response_time = df$response_time)

# Scale the response_time using Z-score (standardization)
df_combined$response_time_scaled <- scale(df_combined$response_time)

# Training Set: Where response_time is not NA
train_data <- df_combined[!is.na(df_combined$response_time), ]

# Test Set: Where response_time is NA
test_data <- df_combined[is.na(df_combined$response_time), ]

# Select the one-hot encoded columns for X (features) and the scaled response time for y (target)
X_train <- train_data %>% select(matches("^as\\.factor\\(df\\$hour\\)"))
y_train <- train_data$response_time_scaled

X_test <- test_data %>% select(matches("^as\\.factor\\(df\\$hour\\)"))  # Only features for prediction

# Train the linear regression model
model <- lm(response_time_scaled ~ ., data = cbind(X_train, response_time_scaled = y_train))

# Make predictions on the test set (rows where response_time was NA)
y_pred_scaled <- predict(model, newdata = X_test)

# Inverse the scaling to get the predictions back to the original scale
mean_response <- mean(df$response_time, na.rm = TRUE)
sd_response <- sd(df$response_time, na.rm = TRUE)
y_pred_original_scale <- y_pred_scaled * sd_response + mean_response

# Extract the correct hours where response_time was NA
df_test_with_pred <- test_data %>%
  mutate(predicted_response = y_pred_original_scale) %>%
  mutate(hour = df$hour[is.na(df$response_time)]) %>%
  select(hour, predicted_response)

# Group by hour and compute the mean predicted response
predicted_means <- df_test_with_pred %>%
  group_by(hour) %>%
  summarise(mean_predicted = mean(predicted_response))

# Calculate the mode of the actual response time for each hour
mode_function <- function(x) {
  uniq_x <- unique(na.omit(x))
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

actual_modes <- df %>%
  mutate(response_time_rounded = round(response_time, 0)) %>%  # Round to nearest integer
  group_by(hour) %>%
  summarise(mode_actual = mode_function(response_time_rounded))  # Mode of actual values for each hour

# Combine the actual modes and predicted averages into one dataframe
df_anim <- data.frame(
  Hour = 0:23,  # Hours 0 to 23
  Actual_Mode_Response_Time = actual_modes$mode_actual,  # Mode of actual response times
  Predicted_Mean_Response_Time = predicted_means$mean_predicted  # Mean predicted response times
)

# Adjusted palette for Secondary bins

# UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      .btn-custom {
        background-color: #f0f0f0;  /* Default button color */
        color: #000;
        border: 1px solid #ccc;
      }
      .btn-custom.active {
        background-color: #007bff;  /* Active button color */
        color: #fff;
        border-color: #007bff;
      }
      .sidebar {
        height: 550px;  /* Match the height of psap_map */
        overflow-y: auto;  /* Enable scrolling if content overflows */
      }
      .distribution-sidebar {
        height: 650px;  /* Match the height of psap_map */
        overflow-y: auto; 
      }
      .distribution-sidebar2 {
        height: 580px;  /* Match the height of psap_map */
        overflow-y: auto; 
      }
       /* Styling and animations for summary statistic boxes */
      .info-box {
        background-color: #f9f9f9;
        border: 2px solid #ddd;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 15px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        text-align: center;
        transition: transform 0.2s, box-shadow 0.2s;
        animation: fade-in 1.5s ease;
      }
      .info-box h4 {
        color: #333;
        font-size: 16px;
        margin-bottom: 5px;
      }
      .info-box-value {
        font-size: 24px;
        font-weight: bold;
        color: #007bff;
        animation: fade-in-up 1s ease forwards;
        opacity: 0;
      }
      /* Hover effect */
      .info-box:hover {
        transform: scale(1.05);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
      }
      /* Keyframes for fade-in animation */
      @keyframes fade-in {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      @keyframes fade-in-up {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      /* Styling for sidebar text boxes */
      .sidebar-text-box {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 10px;
        margin-top: 10px;
        color: #333;
        font-size: 14px;
        line-height: 1.5;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
    "))
  ), # Enable shinyjs
  titlePanel("Comprehensive Analysis of PSAP Distribution, Distress Call Trends, and Future Forecasting"),
  tabsetPanel(
  # Section for Distribution of PSAPs
    tabPanel("Distress Call Trends & Service Distribution Overview",
    h4("Distribution of PSAP's across States"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("psap_type", "Select PSAP Type:", choices = c("Primary", "Secondary"), selected = "Primary"),
        div(htmlOutput("psapInfoText"), class = "sidebar-text-box"),
        class="sidebar"# Add a text box for additional information
      ),
      mainPanel(
        fluidRow(
          column(4, div(class = "info-box", h4("Total Calls this Year"), div(textOutput("totalCalls"), class = "info-box-value"))),
          column(4, div(class = "info-box", h4("Average Answer Time (sec)"), div(textOutput("avgCalls"), class = "info-box-value"))),
          column(4, div(class = "info-box", h4("Calls Dispatched within 60 secs"), div(textOutput("maxCalls"), class = "info-box-value")))
        ),
        plotlyOutput("psap_map", width = "100%", height = "460px")
      )
    ),
    
    # Section for Distribution of Calls
    h3("Trends in Call Volumes Across Different Mediums (2017-2024)"),
    sidebarLayout(
      sidebarPanel(
        div(class = "checkbox-inline",
            checkboxGroupInput("callTypeSelect", "Select Call Types:",
                               choices = unique(df_year[['CallType']]),
                               selected = "Wireless")),
        actionButton("playButton", "Start Animation", class = "btn-primary"),
        sliderInput("yearSlider", "Year", min = 2017, max = 2024, value = c(2017, 2024), step = 1, animate = TRUE),
        div(htmlOutput("distributionInfoText"), class = "sidebar-text-box"),
        class="distribution-sidebar"
      ),
      mainPanel(
        fluidRow(
          column(12, highchartOutput("hchart", height = "620px"))
        )
      )
    ),
    h3("Choropleth Map of Call Distribution by Type Across U.S. States"),
    sidebarLayout(
      sidebarPanel(
        selectInput("call_type", "Select Call Type:", choices = c("Wireline", "Wireless", "VoIP", "Texts"), 
                    selected = "Wireless"),
        div(htmlOutput("hexbinInfoText"), class = "sidebar-text-box"),
        class="distribution-sidebar2"
      ),
      mainPanel(
        fluidRow(
          column(12, plotlyOutput("call_map", height = "500px"))
        ),
      )
    )
    ),
    
    # Section for Analysis of Distress Calls
    tabPanel("Distress Call Analysis: Frequency & Type",
             sidebarLayout(
               
               sidebarPanel(
                 
                 width = 3,
                 
                 radioButtons("typeInput", "Select Action Type:", choices = c("Incident Overview", "Resolution Analysis"), selected = "Incident Overview"),
                 selectizeInput("dayInput", "Select Day(s) of the Week:", 
                                choices = unique(crime_data$Day), 
                                selected = unique(crime_data$Day),
                                multiple = TRUE,
                                options = list(plugins = list('remove_button'), maxItems = NULL)),
                 selectizeInput("crimeInput", "Select Distress Type(s):", 
                                choices = unique(crime_data$Crime), 
                                selected = unique(crime_data$Crime),
                                multiple = TRUE,
                                options = list(plugins = list('remove_button'), maxItems = NULL)),
                 dateRangeInput("dateInput", "Select Date Range:",
                                start = as.Date("2024-01-01"), end = as.Date("2024-06-30"),
                                min = as.Date("2024-01-01"), max = as.Date("2024-06-30")),
                 sliderInput("timeInput", "Select Time Range (Hours):", min = 0, max = 23, value = c(0, 23)),
                 actionButton("submit", "Submit")
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(5, plotlyOutput("barPlot", height = "600px")),  # Placeholder for the bar chart
                   column(7, plotlyOutput("heatmapPlot", height = "600px"))
                 )
               )
             ),
             
             sidebarLayout(
               
               sidebarPanel(
                 id = "specificSidebar",
                 width = 3,
                 div(
                   style = "background-color: #f0f0f0; border-radius: 5px;",
                   h4("Information:"),
                   p(HTML("
    <ul>
      <li>This line chart illustrates the hourly trend in Distress incidents, highlighting their activity variations throughout the day</li>
    </ul>
    <br>
    Each distress type has a unique color and line style to make comparisons easier and more visually effective.
  "))
                 )
                 ,
                 
                 div(
                   style = "background-color: #f0f0f0; margin-top: 20px; border-radius: 5px;",
                   h4("Instructions:"),
                   p(HTML("
    <ul>
      <li>Filter incidents by Type of Distress or Date Range or hour of the day (0 to 23 hours)</li>
      <li>Switch to 'Resolution Analysis' to analyze how resolution times vary across the day</li>
    </ul>
    <br>
    By examining the peaks and troughs, you can gain insights into when incidents are most frequent or when resolutions are most delayed.
  "))
                 )
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   highchartOutput("lineChart", height = "700px")  # New line chart for hourly crime rates
                 )
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 id = "specificSidebar",
                 width = 3,
                 div(
                   style = "background-color: #f0f0f0; border-radius: 5px;",
                   h4("Information:"),
                   p(HTML("
    <ul>
      <li>This scatter bubble chart represents 8 different distress types across the months.</li>
       <li>The size of each bubble is restricted to a specific range for each distress type to help visualize the change in occurrence across months.</li>
    </ul>
    <br>
    This helps viewers effectively understand both the frequency of incidents and the resolution performance for each distress type.
  "))
                 )
                 ,
                 
                 div(
                   style = "background-color: #f0f0f0; margin-top: 20px; border-radius: 5px;",
                   h4("Instructions:"),
                   p(HTML("
    <ul>
      <li>You can filter incidents by Month, Type of Crime, or Time of the day (0 to 23 hours)</li>
      <li>Switch to 'Resolution Analysis' to view variations across months</li>
      <li>Click Autoscale to view the graph in place</li>
    </ul>
    <br>
    By examining the peaks and troughs, you can gain insights into when incidents are most frequent or when resolutions are most delayed.
  "))
                 )
                 
                 
                 
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   plotlyOutput("scatterPlot", height = "700px")
                 )
               )
             )
    ),
    # Section for Forecasting
    tabPanel("Forecasting for the Next Quarter",
      titlePanel("Predictions in Response Time: July-Sep"),
      sidebarLayout(
      sidebarPanel(
        actionButton("play_button", "Play Animation", style = "color: #530c0c;"),
        actionButton("pause_button", "Pause", style = "color: #530c0c;"),
        sliderInput("month_range", "Select Month Range:",
                    min = 1, max = length(unique_months),
                    value = c(1, length(unique_months)),
                    step = 1,
                    ticks = TRUE,
                    animate = TRUE),
        div(style = "background-color: #f0f0f0; padding: 15px; margin-top: 20px; border-radius: 5px;",
            h4("Information:"),
            p("This chart presents the SARIMA model forecast for average response times over time. ",
              "The green line represents the actual average response times, while the blue line represents ",
              "the forecasted response values. Use the slider above to filter data by month, and click 'Play Animation' ",
              "to visualize how the data evolves over time.")
        )
      ),
      mainPanel(
        plotlyOutput("time_series_plot"),
        br(),
        br(),
        div(style = "margin-top: 40px;")  # Spacing for the next chart
      )
    ),
    titlePanel("Actual vs Predicted Response Time (Hourly Estimates)"),
    sidebarLayout(
      sidebarPanel(
        actionButton("play", "Play Animation", style = "color: #530c0c;"),
        actionButton("stop", "Pause",  style = "color: #530c0c;"),
        sliderInput("hour_range", "Select Hour Range:",
                    min = 0, max = 23, value = c(0, 23), step = 1,
                    ticks = TRUE, animate = animationOptions(interval = 500, loop = TRUE)),
        div(style = "background-color: #f0f0f0; padding: 15px; margin-top: 20px; border-radius: 5px;",
            h4("Information:"),
            p("A combination of ARIMA and Linear Model presents the actual vs. predicted response times for each hour of the day. ",
              "The green line represents the actual response times, while the blue line shows the predicted ",
              "values. Use the slider above to select the hour range, and click 'Play Animation' to animate the visualization.")
        )
      ),
      mainPanel(
        plotlyOutput("responsePlot", height = "600px")
      )
    )
    )
    )
)
# Server
server <- function(input, output, session) {
  
  
  output$psapInfoText <- renderUI({
    HTML("
    <div style='background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 15px;'>
      <h4>Information:</h4>
      <p>
        <ul>
          <li>This hexbin map illustrates the density and distribution of Public Safety Answering Points (PSAPs) across states</li>
          <li>Based on the criticality of the distress, either Primary or secondary Team is contacted</li>
          </ul>
      </p>
      <h4>Instructions:</h4>
      <p>
        <ul>
          <li>Switch between Primary and Secondary PSAP views</li>
          <li>Use the interactive zoom and pan functionality to closely explore specific areas of the map by dragging or zooming in/out</li>
          <li>You can further interact with the graph by clicking on the legend on the right to filter states by the selected PSAP count ranges</li>
          <li>Deselecting a PSAP count type will remove the states in that bracket, giving a more catered view of the data</li>
        </ul>
      </p>
    </div>
  ")
  })
  
  # Adding Information and Instructions Content
  output$distributionInfoText <- renderUI({
    HTML("
    <div style='background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 15px;'>
      <h4>Information:</h4>
      <ul>
        <li>This line chart illustrates the yearly trend in different types of calls made across the years.</li>
        <li>Users can analyze the distribution of calls by multi-selecting different call types and adjusting the year range.</li>
      </ul>
      <h4>Instructions:</h4>
      <ul>
        <li>Adjust the year slider and the chart will reflect data for those years.</li>
        <li>Click on the 'Play Animation' button to animate the change in call distribution from 2017 to 2024, year by year.</li>
      </ul>
    </div>
  ")
  })
  output$hexbinInfoText <- renderUI({
    div(
      style = "background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
      h4("Information:"),
      p(HTML("
      <ul>
        <li>This choropleth map illustrates the distribution of calls across different states based on the selected call type</li>
        <li>The color gradient on the map represents the number of calls, with lighter colors indicating lower counts and darker colors indicating higher counts.</li>
        <li>Hovering over a state will display a tooltip containing the state name and the number of calls for the selected call type.</li>
      </ul>
    ")),
      h4("Instructions:"),
      p(HTML("
      <ul>
        <li>Use the dropdown menu to select the type of call (Wireless, Wireline, VoIP, Text) to view its distribution across the U.S. states.</li>
        <li>The map provides an interactive tooltip that shows the state name and corresponding number of calls when you hover over a state.</li>
    
      </ul>
    "))
    )
  })
  
  # Calculate total calls
  output$totalCalls <- renderText({
    sum(df_year[['Calls']])
  })
  
  # Calculate average calls per year
  output$avgCalls <- renderText({
    round(mean(crime_data[['Resp_TS']]), 2)
  })
  
  # Calculate maximum calls in a year
  output$maxCalls <- renderText({'61.9%'})
  
  # Dummy content for statistics
  output$textBox1 <- renderText("This is some descriptive information about the statistics or visualizations.")
  output$textBox2 <- renderText("This is additional context or insights related to the visualizations.")
  output$tab3Content <- renderText("Forecasting content will be added here.")
  
  
  # Interactive Line Chart
  currentYear <- reactiveVal(2024)
  selectedYears <- reactive({ input$yearSlider })
  timer <- reactiveTimer(1500)  # Create a timer with a 1.5 second interval
  isAnimating <- reactiveVal(FALSE)  # Track if the animation is running
  
  output$hchart <- renderHighchart({
    req(input$callTypeSelect)  # Ensure a call type is selected
    df_filtered <- df_year %>% filter(Year >= selectedYears()[1], Year <= currentYear(), CallType %in% input$callTypeSelect)
    highchart() %>%
      hc_chart(backgroundColor = 'transparent') %>%  # This line removes the background
      hc_add_series(df_filtered, "line", hcaes(x = Year, y = Calls, group = CallType)) %>%
      hc_xAxis(categories = as.character(selectedYears()[1]:selectedYears()[2]), title = list(text = "Year"),
               gridLineWidth = 0) %>%  # Remove grid lines on the x-axis
      hc_yAxis(title = list(text = "Number of Calls"),
               gridLineWidth = 0) %>%  # Remove grid lines on the y-axis
      hc_add_theme(hc_theme_flat()) %>%
      hc_title(text = NULL)
  })
  
  # Start animation when play button is clicked
  observeEvent(input$playButton, {
    currentYear(selectedYears()[1])  # Reset to start year when play is clicked
    isAnimating(TRUE)  # Set animation flag to TRUE
  })
  
  # Animation loop
  observe({
    if (isAnimating()) {
      timer()  # Wait for the timer to invalidate
      if (currentYear() < selectedYears()[2]) {
        currentYear(currentYear() + 1)  # Increment the year
      } else {
        currentYear(selectedYears()[2])  # Stop at the end year
      }
    }
  })
  
  # Allow slider to manually adjust the year
  observeEvent(input$yearSlider, {
    currentYear(input$yearSlider[2])
    isAnimating(FALSE)  # Stop animation when manually adjusting the year
  })
  
  observe({
    if (input$psap_type == "Primary") {
      output$psap_map <- renderPlotly({
        # Render Primary PSAP map
        my_sf_psap_filtered <- my_sf_psap
        
        # Calculate highest, lowest, and count of NA
        highest_value <- max(my_sf_psap_filtered$Primary, na.rm = TRUE)
        lowest_value <- min(my_sf_psap_filtered$Primary, na.rm = TRUE)
        na_count <- sum(is.na(my_sf_psap_filtered$Primary))
        
        # Find the state with highest and lowest values
        highest_state <- my_sf_psap_filtered %>% filter(Primary == highest_value) %>% pull(google_name)
        lowest_states <- my_sf_psap_filtered %>% filter(Primary == lowest_value) %>% pull(google_name)
        na_states <- my_sf_psap_filtered %>% filter(is.na(Primary)) %>% pull(google_name)
        
        # Create the base plot
        plot_map <- ggplot(my_sf_psap_filtered) +
          geom_sf(aes(
            fill = Primary_bin,
            text = paste("<b>State:</b>", google_name, "<br><b>Primary PSAPs:</b>", Primary)
          ), linewidth = 0.5, alpha = 0.8) +
          geom_sf_text(aes(label = iso3166_2), color = "white", size = 3, alpha = 0.8) +
          theme_minimal() +
          theme(
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            legend.position = "bottom"
          ) +
          scale_fill_manual(
            values = primary_palette,
            name = "Primary PSAP"
          )
        
        # Convert to interactive plotly plot and add annotations
        annotations <- list(
          # Highest count annotation
          list(
            x = my_sf_psap_filtered %>% filter(google_name == highest_state) %>% st_coordinates() %>% .[1,1],
            y = my_sf_psap_filtered %>% filter(google_name == highest_state) %>% st_coordinates() %>% .[1,2],
            text = paste("Highest:", highest_state, "Count:", highest_value),
            showarrow = TRUE,
            arrowhead = 3,
            ax = 20,
            ay = -40,
            font = list(size = 12, color = "white", fontWeight = "bold"),
            bgcolor = "rgba(199, 21, 133, 0.9)",  # Dark pink background
            bordercolor = "darkred",
            borderRadius = 5
          ),
          # NA count annotation
          list(
            x = 0.5,
            y = 1.1,
            text = paste("Number of States with NA:", na_count),
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            align = "center",
            font = list(size = 12, color = "white", fontWeight = "bold"),
            bgcolor = "rgba(169, 169, 169, 0.9)",  # Grey background
            bordercolor = "darkgrey",
            borderRadius = 5
          )
        )
        
        # Add annotations for each of the states with the lowest count
        for (state in lowest_states) {
          annotations <- append(annotations, list(
            list(
              x = my_sf_psap_filtered %>% filter(google_name == state) %>% st_coordinates() %>% .[1, 1],
              y = my_sf_psap_filtered %>% filter(google_name == state) %>% st_coordinates() %>% .[1, 2],
              text = paste("Lowest:", state, "Count:", lowest_value),
              showarrow = TRUE,
              arrowhead = 3,
              ax = 20,
              ay = 40,
              font = list(size = 12, color = "black", fontWeight = "bold"),
              bgcolor = "rgba(255, 182, 193, 0.8)",  # Light pink background
              bordercolor = "black",
              borderRadius = 5
            )
          ))
        }
        
        plot <- ggplotly(plot_map, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(title = ""),  # Remove x-axis label
            yaxis = list(title = ""),  # Remove y-axis label
            legend = list(
              x = 1.05, y = 0.5,
              bgcolor = "#f8f9fa",
              title = list(text = "<b>PSAP Primary Counts</b>"),
              font = list(size = 10),
              borderwidth = 0
            ),
            margin = list(l = 0, r = 0, t = 0, b = 0),
            annotations = annotations
          ) %>%
          style(hoverinfo = "text",
                hoverlabel = list(
                  bgcolor = "rgba(255, 255, 255, 0.85)",
                  bordercolor = "rgba(0, 0, 0, 0.2)",
                  font = list(size = 14, color = "black")
                )) %>%
          config(displayModeBar = FALSE)
        
        return(plot)
      })
    }
    
    else if (input$psap_type == "Secondary") {
      output$psap_map <- renderPlotly({
        # Render Secondary PSAP map
        my_sf_psap_filtered <- my_sf_psap
        
        # Calculate highest, lowest, and count of NA
        highest_value <- max(my_sf_psap_filtered$Secondary, na.rm = TRUE)
        lowest_value <- min(my_sf_psap_filtered$Secondary, na.rm = TRUE)
        na_count <- sum(is.na(my_sf_psap_filtered$Secondary))
        
        # Find the states with the highest and lowest values
        highest_state <- my_sf_psap_filtered %>% filter(Secondary == highest_value) %>% pull(google_name)
        lowest_states <- my_sf_psap_filtered %>% filter(Secondary == lowest_value) %>% pull(google_name)
        
        # Create the base plot
        plot_map <- ggplot(my_sf_psap_filtered) +
          geom_sf(aes(
            fill = Secondary_bin, 
            text = paste("<b>State:</b>", google_name, "<br><b>Secondary PSAPs:</b>", Secondary)
          ), linewidth = 0.5, alpha = 0.8) +
          geom_sf_text(aes(label = iso3166_2), color = "white", size = 3, alpha = 0.8) +
          theme_minimal() +
          theme(
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            legend.position = "bottom"
          ) +
          scale_fill_manual(
            values = secondary_palette,
            name = "Secondary PSAP"
          )
        
        # Convert to interactive plotly plot and add annotations
        annotations <- list(
          # Highest count annotation
          list(
            x = my_sf_psap_filtered %>% filter(google_name == highest_state) %>% st_coordinates() %>% .[1,1],
            y = my_sf_psap_filtered %>% filter(google_name == highest_state) %>% st_coordinates() %>% .[1,2],
            text = paste("Highest:", highest_state, "Count:", highest_value),
            showarrow = TRUE,
            arrowhead = 3,
            ax = 20,
            ay = -40,
            font = list(size = 12, color = "white", fontWeight = "bold"),
            bgcolor = "rgba(199, 21, 133, 0.9)",  # Dark pink background
            bordercolor = "darkred",
            borderRadius = 5
          ),
          # NA count annotation
          list(
            x = 0.5,
            y = 1.1,
            text = paste("Number of States with NA:", na_count),
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            align = "center",
            font = list(size = 12, color = "white", fontWeight = "bold"),
            bgcolor = "rgba(169, 169, 169, 0.9)",  # Grey background
            bordercolor = "darkgrey",
            borderRadius = 5
          )
        )
        
        # Add annotations for each of the states with the lowest count
        for (state in lowest_states) {
          annotations <- append(annotations, list(
            list(
              x = my_sf_psap_filtered %>% filter(google_name == state) %>% st_coordinates() %>% .[1, 1],
              y = my_sf_psap_filtered %>% filter(google_name == state) %>% st_coordinates() %>% .[1, 2],
              text = paste("Lowest:", state, "Count:", lowest_value),
              showarrow = TRUE,
              arrowhead = 3,
              ax = 20,
              ay = 40,
              font = list(size = 12, color = "black", fontWeight = "bold"),
              bgcolor = "rgba(255, 182, 193, 0.8)",  # Light pink background
              bordercolor = "black",
              borderRadius = 5
            )
          ))
        }
        
        plot <- ggplotly(plot_map, tooltip = "text") %>%
          layout(
            xaxis = list(title = ""),  # Remove x-axis label
            yaxis = list(title = ""),  # Remove y-axis label
            hovermode = "closest",
            legend = list(
              x = 1.05, y = 0.5, 
              bgcolor = "#f8f9fa",
              title = list(text = "<b>PSAP Secondary Counts</b>"),
              font = list(size = 10),
              borderwidth = 0
            ),
            margin = list(l = 0, r = 0, t = 0, b = 0),
            annotations = annotations
          ) %>%
          style(hoverinfo = "text", 
                hoverlabel = list(
                  bgcolor = "rgba(255, 255, 255, 0.85)",
                  bordercolor = "rgba(0, 0, 0, 0.2)",
                  font = list(size = 14, color = "black")
                )) %>%
          config(displayModeBar = FALSE)
        
        return(plot)
      })
    }
    
  })
  
  output$call_map <- renderPlotly({
    # Merge the call data with the spatial data
    us_states_call <- left_join(us_states, call_data, by = c("ID" = "ID"))
    
    # Select the appropriate column based on user input
    selected_column <- switch(input$call_type,
                              "Total" = ifelse(us_states_call$Total == 0, NA, us_states_call$Total),
                              "Wireline" = us_states_call$Wireline,
                              "Wireless" = us_states_call$Wireless,
                              "VoIP" = us_states_call$VoIP,
                              "Texts" = us_states_call$Texts)
    
    # Round the selected column to 2 decimal places for annotations
    selected_column_rounded <- round(selected_column, 2)
    
    # Calculate the highest and lowest counts (ignore 0 for lowest)
    highest_value <- round(max(selected_column_rounded, na.rm = TRUE), 2)
    lowest_value <- round(min(selected_column_rounded[selected_column_rounded >= 1], na.rm = TRUE), 2)
    
    # Find states with highest and lowest counts
    highest_states <- us_states_call %>% filter(selected_column_rounded == highest_value) %>% pull(ID)
    lowest_states <- us_states_call %>% filter(selected_column_rounded == lowest_value) %>% pull(ID)
    
    # Create the map using ggplot
    plot_map <- ggplot(us_states_call) +
      geom_sf(aes(fill = selected_column_rounded, text = paste("<b>State:</b>", ID, "<br><b>Calls:</b>", selected_column_rounded)),
              color = "black", size = 0.05) +
      scale_fill_gradientn(colors = primary_palette, 
                           name = "Number of Calls (%)", 
                           na.value = "grey", 
                           breaks = round(seq(0, max(selected_column_rounded, na.rm = TRUE), length.out = 10), 2), 
                           labels = round(seq(0, max(selected_column_rounded, na.rm = TRUE), length.out = 10), 2)) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(size = 20, hjust = 0.5),
        panel.background = element_blank(),  # Remove panel background
        plot.background = element_blank()  # Set background to #f8f9fa
      )
    
    # Convert the ggplot to an interactive plotly object and add annotations
    plot <- ggplotly(plot_map, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        margin = list(l = 0, r = 0, t = 50, b = 0),
        annotations = list(
          # Annotation for highest count
          lapply(highest_states, function(state) {
            state_coords <- us_states_call %>% filter(ID == state) %>% st_coordinates()
            list(
              x = state_coords[1, 1],
              y = state_coords[1, 2],
              text = paste("Highest:", state, "Count:", highest_value),
              showarrow = TRUE,
              arrowhead = 3,
              ax = 20,
              ay = -40,
              font = list(size = 12, color = "white", fontWeight = "bold"),
              bgcolor = "rgba(199, 21, 133, 0.9)",  # Dark pink background
              bordercolor = "darkred",
              borderRadius = 5,
              xref = "x",
              yref = "y"
            )
          }),
          # Annotation for lowest count
          lapply(lowest_states, function(state) {
            state_coords <- us_states_call %>% filter(ID == state) %>% st_coordinates()
            list(
              x = state_coords[1, 1],
              y = state_coords[1, 2],
              text = paste("Lowest:", state, "Count:", lowest_value),
              showarrow = TRUE,
              arrowhead = 3,
              ax = 20,
              ay = 40,
              font = list(size = 12, color = "black", fontWeight = "bold"),
              bgcolor = "rgba(255, 182, 193, 0.8)",  # Light pink background
              bordercolor = "black",
              borderRadius = 5,
              xref = "x",
              yref = "y"
            )
          })
        ) %>% unlist(recursive = FALSE)
      ) %>%
      config(displayModeBar = FALSE)
    
    return(plot)
  })
  
  
  
  
  
  updateActionButton(session, "submit", label = "Submit")
  
  # Reactive expression to filter data based on inputs
  filtered_data <- reactive({
    
    if (input$typeInput == "Incident Overview") {
      
      filtered <- crime_data %>%
        filter(
          !is.na(Hour),
          as.Date(Date, format = "%d/%m/%Y") >= input$dateInput[1] &
            as.Date(Date, format = "%d/%m/%Y") <= input$dateInput[2],
          Hour >= input$timeInput[1] & Hour <= input$timeInput[2],
          Day %in% input$dayInput,
          Crime %in% input$crimeInput
        ) %>%
        group_by(Crime, Day) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
        ungroup()
    }
    
    else {
      filtered <- crime_data %>%
        filter(
          !is.na(Hour),
          as.Date(Date, format = "%d/%m/%Y") >= input$dateInput[1] &
            as.Date(Date, format = "%d/%m/%Y") <= input$dateInput[2],
          Hour >= input$timeInput[1] & Hour <= input$timeInput[2],
          Day %in% input$dayInput,
          Crime %in% input$crimeInput
        ) %>%
        group_by(Crime, Day) %>%
        summarise(Count = round(mean(Resp_TS, na.rm = TRUE), 2), .groups = 'drop') %>%
        ungroup()
      
    }
    
    # Ensure that the filtered data is not empty and set a default max value for Count
    if (nrow(filtered) == 0) {
      filtered <- filtered %>%
        add_row(Hour = NA, Crime = NA, Day = NA, Count = 0)
    }
    
    filtered
  })
  
  
  # Reactive expression to filter and preprocess the data
  processed_data <- reactive({
    input$submit  # Depend on the submit button to reprocess data
    
    filtered <- crime_data %>%
      filter(
        as.Date(Date, format = "%d/%m/%Y") >= input$dateInput[1] &
          as.Date(Date, format = "%d/%m/%Y") <= input$dateInput[2],
        Day %in% input$dayInput,
        Hour >= input$timeInput[1] & Hour <= input$timeInput[2],
        Crime %in% input$crimeInput
      )
    
    # Remove any leading/trailing spaces in Date column
    filtered$Date <- trimws(filtered$Date)
    
    # Convert Date column to Date type using lubridate
    filtered$Date <- dmy(filtered$Date)
    
    # Extract month name from Date column and store it in a new column 'Month_Name'
    filtered$Month_Name <- month(filtered$Date, label = TRUE, abbr = TRUE)
    
    # Extract month and year for aggregation purposes
    filtered$Month <- floor_date(filtered$Date, unit = "month")
    
    if (input$typeInput == "Incident Overview") {
      # Aggregate data to get the sum of counts per Crime type per Month
      crime_summary <- filtered %>%
        group_by(Crime, Month, Month_Name) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
        ungroup()
    } else {
      # Aggregate data to get the mean of response time per Crime type per Month
      crime_summary <- filtered %>%
        group_by(Crime, Month, Month_Name) %>%
        summarise(Count = round(mean(Resp_TS, na.rm = TRUE), 2), .groups = 'drop') %>%
        ungroup()
    }
    
    # Assign unique dates within each month for each crime type (4-day intervals)
    crime_summary <- crime_summary %>%
      group_by(Month) %>%
      mutate(
        Days_Offset = (row_number() - 1) * 4,
        Adjusted_Date = Month + days(Days_Offset)
      ) %>%
      ungroup()
    
    # Apply Min-Max Scaling to Count within each Crime Type to enhance visualization
    crime_summary <- crime_summary %>%
      group_by(Crime) %>%
      mutate(Scaled_Size = rescale(Count, to = c(30, 70))) %>%
      ungroup()
    
    # Prepare text for tooltip
    crime_summary <- crime_summary %>%
      mutate(text = paste("Distress Type: ", Crime, "\nCount: ", Count, "\nDate: ", Month_Name, sep=""))
    
    crime_summary
  })
  
  # Render the scatter plot as a bubble plot using ggplot
  output$scatterPlot <- renderPlotly({
    crime_summary <- processed_data()
    
    # Get unique Crime Types
    unique_crime_types <- unique(crime_summary$Crime)
    
    # Define Colors and Shapes
    colors <- RColorBrewer::brewer.pal(n = max(3, length(unique_crime_types)), name = "Set1")
    shapes <- c(19, 17, 18, 15, 16, 3, 4, 8) # Specify shapes (customizable)
    
    # Create the bubble chart using ggplot
    bubble_plot <- ggplot(crime_summary, aes(x = Adjusted_Date, y = Scaled_Size, size = Scaled_Size, color = Crime, shape = Crime, text = text)) +
      geom_point(alpha = 0.7) + # Set transparency for better overlapping visualization
      scale_size_continuous(name = "Crime Count", range = c(6, 20)) + # Set the size range for bubbles
      scale_color_manual(values = colors, name = "Crime Type") + # Use Set1 colors
      scale_shape_manual(values = shapes, name = "Crime Type") + # Assign shapes for each crime type
      theme_ipsum() + # Apply hrbrthemes for better styling
      theme(
        legend.position = "right",
        axis.title.y = element_blank(), # Remove Y-axis title
        axis.text.y = element_blank(), # Remove Y-axis text labels
        axis.ticks.y = element_blank() # Remove Y-axis ticks
        
      ) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") + # Clear month labels on the x-axis
      labs(title = "Monthly Trends of Distress Types and Resolution Analysis: An Interactive Bubble Chart",
           x = "Month")
    
    # Convert ggplot to an interactive plotly plot
    ggplotly(bubble_plot, tooltip = "text") %>%
      layout(legend = list(title = list(text = "Crime Type")))
  })
  
  # Render the heatmap
  output$heatmapPlot <- renderPlotly({
    input$submit  # Depend on the submit button
    
    title <- if (input$typeInput == "Incident Overview") "Incident Frequency by Weekday" else "Resolution Efficiency by Weekday"
    
    
    isolate({
      
      #if (input$typeInput == "Incident Overview") {
      filtered <- filtered_data()
      
      # Check if filtered data is empty and set proper sequence for breaks
      max_count <- max(filtered$Count, na.rm = TRUE)
      break_step <- ifelse(max_count > 0, max(1, max_count / 5), 1)
      
      p <- ggplot(filtered, aes(x = Day, y = Crime, fill = Count, text = paste0("Day: ", Day, "\nCrime: ", Crime, "\nCount: ", Count))) + 
        geom_tile() +
        scale_fill_gradient(
          low = "yellow", high = "purple",
          breaks = if (max_count > 0) seq(0, max_count, by = break_step) else NULL,
          guide = guide_colorbar(barheight = unit(25, "cm"), barwidth = unit(2, "cm"))
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0, size = 14) # Make title bold and left-aligned
        ) +
        labs(
          title = title,
          x = "Day of the Week",
          y = "Distress Type",
          fill = "Count"
        )
      
      ggplotly(p, tooltip = "text")
      # }
    })
    
    
  })
  
  # Render the bar chart
  output$barPlot <- renderPlotly({
    filtered <- crime_data %>%
      filter(
        !is.na(Hour),
        as.Date(Date, format = "%d/%m/%Y") >= input$dateInput[1] &
          as.Date(Date, format = "%d/%m/%Y") <= input$dateInput[2],
        Hour >= input$timeInput[1] & Hour <= input$timeInput[2],
        Day %in% input$dayInput,
        Crime %in% input$crimeInput
      )
    
    if (input$typeInput == "Incident Overview") {
      filtered <- filtered %>%
        group_by(Crime_Main) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop')
    } else {
      filtered <- filtered %>%
        group_by(Crime_Main) %>%
        summarise(Count = round(mean(Resp_TS, na.rm = TRUE), 2), .groups = 'drop')
    }
    
    p <- ggplot(filtered, aes(x = Crime_Main, y = Count, fill = Crime_Main, text = paste0("Crime Main: ", Crime_Main, "\nCount: ", Count))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0, size = 14) # Make title bold and left-aligned
      ) +
      labs(
        title = if (input$typeInput == "Incident Overview") "Incident Overview by \nDistress Type" else "Resolution Analysis by \nCrime Type",
        x = "Crime Main Type",
        y = if (input$typeInput == "Incident Overview") "Total Incidents" else "Average Response Time"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  
  filtered_hourly_data <- reactive({
    if (input$typeInput == "Incident Overview") {
      filtered <- crime_data %>%
        filter(
          !is.na(Hour),
          as.Date(Date, format = "%d/%m/%Y") >= input$dateInput[1] &
            as.Date(Date, format = "%d/%m/%Y") <= input$dateInput[2],
          Crime %in% input$crimeInput,
          Hour >= input$timeInput[1] & Hour <= input$timeInput[2],
          Day %in% input$dayInput,
        ) %>%
        group_by(Hour, Crime) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
        ungroup()
    } else {
      filtered <- crime_data %>%
        filter(
          !is.na(Hour),
          as.Date(Date, format = "%d/%m/%Y") >= input$dateInput[1] &
            as.Date(Date, format = "%d/%m/%Y") <= input$dateInput[2],
          Crime %in% input$crimeInput,
          Hour >= input$timeInput[1] & Hour <= input$timeInput[2]
        ) %>%
        group_by(Hour, Crime) %>%
        summarise(Count = round(mean(Resp_TS, na.rm = TRUE), 2), .groups = 'drop') %>%
        ungroup()
    }
    
    # Ensure that the filtered data is not empty and set a default max value for Count
    if (nrow(filtered) == 0) {
      filtered <- filtered %>%
        add_row(Date = NA, Hour = NA, Crime = NA, Count = 0)
    }
    
    filtered
  })
  
  # Render the line chart for hourly data
  output$lineChart <- renderHighchart({
    
    text <- if (input$typeInput == "Incident Overview") 'Hourly Trends of Incidents: An Overview of Distress Type Dynamics by Time of Day' else 'Hourly Trends in Resolution Analysis: Visualizing Resolution Patterns'
    
    # Get unique Crime Types
    unique_crime_types <- unique(filtered_hourly_data()$Crime)
    
    # Define Colors
    colors <- RColorBrewer::brewer.pal(n = max(3, length(unique_crime_types)), name = "Set1")
    
    # Create Highchart Line Chart
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(
        text = text,
        align = "left", # Align title to the left
        style = list(fontWeight = "bold") # Make title bold
      ) %>%
      hc_xAxis(title = list(text = "Hour of Day"),
               categories = as.character(unique(filtered_hourly_data()$Hour))) %>%
      hc_yAxis(title = list(text = if (input$typeInput == "Incident Overview") "Distress Count" else "Average Response Time"))
    
    # Adding Series Dynamically and Calculating Highest and Lowest Points
    highest_value <- -Inf
    lowest_value <- Inf
    highest_point <- list()
    lowest_point <- list()
    
    for (i in seq_along(unique_crime_types)) {
      crime_type <- unique_crime_types[i]
      color <- colors[i]
      crime_data_filtered <- filtered_hourly_data() %>% filter(Crime == crime_type)
      crime_counts <- crime_data_filtered %>% pull(Count)
      
      # Update the highest and lowest values for annotations
      max_value <- max(crime_counts, na.rm = TRUE)
      min_value <- min(crime_counts, na.rm = TRUE)
      max_index <- which.max(crime_counts)
      min_index <- which.min(crime_counts)
      
      if (max_value > highest_value) {
        highest_value <- max_value
        highest_point <- list(x = crime_data_filtered$Hour[max_index], y = max_value, crime_type = crime_type)
      }
      if (min_value < lowest_value) {
        lowest_value <- min_value
        lowest_point <- list(x = crime_data_filtered$Hour[min_index], y = min_value, crime_type = crime_type)
      }
      
      hc <- hc %>% hc_add_series(
        name = crime_type,
        data = crime_counts,
        color = color
      )
    }
    
    # Adding Annotations for the Highest and Lowest Points with Specific Colors
    annotations <- list(
      labels = list(
        list(
          point = list(
            xAxis = 0,
            yAxis = 0,
            x = highest_point$x,
            y = highest_point$y
          ),
          text = paste("Highest:", highest_point$crime_type, "Count:", highest_value),
          style = list(fontSize = "12px", color = "white", fontWeight = "bold"),
          backgroundColor = "rgba(199, 21, 133, 0.9)",  # Dark pink background
          borderColor = "darkred",
          borderRadius = 5
        ),
        list(
          point = list(
            xAxis = 0,
            yAxis = 0,
            x = lowest_point$x,
            y = lowest_point$y
          ),
          text = paste("Lowest:", lowest_point$crime_type, "Count:", lowest_value),
          style = list(fontSize = "12px", color = "black", fontWeight = "bold"),
          backgroundColor = "rgba(255, 182, 193, 0.8)",  # Light pink background
          borderColor = "black",
          borderRadius = 5
        )
      )
    )
    
    # Adding Tooltip, Interaction (Opacity Change on Hover), and Annotations
    hc %>%
      hc_tooltip(shared = TRUE, 
                 pointFormat = "<b>{series.name}: {point.y}</b><br>") %>%
      hc_plotOptions(
        series = list(
          states = list(
            hover = list(
              enabled = TRUE,
              halo = list(size = 0)
            ),
            inactive = list(
              opacity = 0.2  # Set opacity of other series when hovering
            )
          )
        )
      ) %>%
      hc_annotations(annotations)
  })
  
  
  current_index <- reactiveVal(1)
  playing <- reactiveVal(FALSE)
  autoInvalidate <- reactiveTimer(100)  # Timer for animation
  observeEvent(input$play_button, {
    playing(TRUE)
    current_index(1)
  })
  observeEvent(input$pause_button, {
    playing(FALSE)
  })
  observeEvent(input$month_range, {
    playing(TRUE)
    current_index(1)
  })
  observe({
    if (playing()) {
      autoInvalidate()
      isolate({
        filtered_length <- nrow(filtered_data_tab3())
        new_index <- current_index() + 1
        if (new_index > filtered_length) {
          playing(FALSE)
        } else {
          current_index(new_index)
        }
      })
    }
  })
  filtered_data_tab3 <- reactive({
    selected_months <- unique_months[input$month_range[1]:input$month_range[2]]
    combined_data %>%
      filter(format(DATE, "%Y-%m") %in% selected_months)
  })
  output$time_series_plot <- renderPlotly({
    data_to_plot <- filtered_data_tab3()
    max_index <- min(current_index(), nrow(data_to_plot))
    plot_ly(data = data_to_plot[1:max_index, ], x = ~DATE, y = ~AVG_RESP, type = 'scatter', mode = 'lines+markers',
            color = ~Type, colors = c("Actual" = "#00FF00", "Forecast" = "blue")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Average Response Time")
      )
  })
  
  current_hour <- reactiveVal(23)
  playing_hour <- reactiveVal(FALSE)
  autoInvalidateHour <- reactiveTimer(500)
  observeEvent(input$play, {
    playing_hour(TRUE)
    current_hour(0)
  })
  observeEvent(input$stop, {
    playing_hour(FALSE)
  })
  observe({
    if (playing_hour()) {
      autoInvalidateHour()
      isolate({
        new_hour <- current_hour() + 1
        if (new_hour > 23) {
          playing_hour(FALSE)
        } else {
          current_hour(new_hour)
        }
      })
    }
  })
  output$responsePlot <- renderPlotly({
    plot_ly(df_anim[df_anim$Hour >= input$hour_range[1] & df_anim$Hour <= current_hour(), ], 
            x = ~Hour, y = ~Actual_Mode_Response_Time, 
            name = "Actual", type = 'scatter', mode = 'lines+markers', 
            line = list(color = "#00FF00", width = 3)) %>%
      add_trace(y = ~Predicted_Mean_Response_Time, name = "Predicted", 
                type = 'scatter', mode = 'lines+markers', 
                line = list(color = "blue", width = 3)) %>%
      layout(
        xaxis = list(title = "Hour", titlefont = list(size = 16)),
        yaxis = list(title = "Response Time", titlefont = list(size = 16))
      )
  })
  
}

# Run App
shinyApp(ui, server)


