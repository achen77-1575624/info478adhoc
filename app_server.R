# load your libraries
# read in the data set 
library(shiny)

# Clean your data and explore variables 

source("prep_data.R")



# Define the server
server <- function(input, output) {
  
  # Page 1

  output$plot1 <- renderPlot({
    if(input$page1 == "Total COVID Deaths") {
      plot1 <- deaths_income_aqi %>%
        filter(!is.na(covid_death_percentage)) %>%
        ggplot(aes(x = urbanicity, y = deaths_covid)) +
        geom_bar(stat="identity", fill = "#A897F9") +
        scale_y_continuous(name="Total Deaths Involving COVID 19", labels = comma) +
        labs(title = "Total Deaths Involving COVID in Different Urbanicity Levels", x = "Levels of Urbanicity")
    } else if (input$page1 == "% of Deaths Involving COVID") {
      plot1 <- deaths_income_aqi %>%
        filter(!is.na(covid_death_percentage)) %>%
        group_by(urbanicity) %>%
        summarise(average_percent_deaths = mean(covid_death_percentage, na.rm = TRUE)) %>%
        ggplot(aes(x = urbanicity, y = average_percent_deaths)) +
        geom_bar(stat="identity", fill = "#A897F9") +
        scale_y_continuous(name="Percentage of Deaths Involving COVID 19", labels = comma) +
        labs(title = "Percentage of Deaths Involving COVID in Different Urbanicity Levels", x = "Levels of Urbanicity")
    }
    print(plot1)
  })
  
  # Page 2

  output$plot2 <- renderPlot({
    if(input$page2 == "AQI") {
      plot2 <- county_table %>%
        ggplot(aes(x = long, y = lat, group = group, fill = median_AQI)) +
        geom_polygon(color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
        scale_fill_continuous(type = "viridis") +
        labs(title = "Median AQI Across Counties in 2021", fill = "Median") +
        theme(legend.position="right",
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank())
    } else if (input$page2 == "Ozone Caution") {
      plot2 <- county_table %>%
        ggplot(aes(x = long, y = lat, group = group, fill = Days.Ozone)) +
        geom_polygon(color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
        scale_fill_continuous(type = "viridis") +                                                                        
        labs(title = "Days with Ozone Caution Across Counties in 2021", fill = "Days with Ozone Caution") +
        theme(legend.position="right",
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank())
    } else if (input$page2 == "Unhealthy for Sensitive Groups Days") {
      plot2 <- county_table %>%
        ggplot(aes(x = long, y = lat, group = group, fill = unhealthy_sensitive_days)) +
        geom_polygon(color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
        scale_fill_continuous(type = "viridis") +
        labs(title = "Days Considered 'Unhealthy for Sensitive Groups' Across Counties in 2021", fill = "Days Considered Unhealthy for Sensitive Groups") +
        theme(legend.position="right",
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank())
    }
    print(plot2)
  })
  
  output$plot3 <- renderPlot({
    if(input$page3 == "Total COVID Deaths") {
      plot3 <- county_table %>%
        ggplot(aes(x = long, y = lat, group = group, fill = deaths_covid)) +
        geom_polygon(color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
        scale_fill_continuous(type = "viridis") +
        labs(title = "Number of Deaths Involving COVID Across Counties in 2021", fill = "Deaths Involving COVID") +
        theme(legend.position="right",
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank())
    } else if (input$page3 == "% of Deaths Involving COVID") {
      plot3 <- county_table %>%
        ggplot(aes(x = long, y = lat, group = group, fill = covid_death_percentage)) +
        geom_polygon(color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
        scale_fill_continuous(type = "viridis") +
        labs(title = "Percentage of All Deaths Involving COVID Across Counties in 2021", fill = "% of Total Deaths Involving COVID") +
        theme(legend.position="right",
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank())
    }
    print(plot3)
  })
  
  output$plot4 <- renderPlot({
    plot4 <- deaths_income_aqi %>%
      filter(!is.na(covid_death_percentage)) %>%
      filter(!is.na(median_AQI)) %>%
      ggplot(mapping = aes(x = median_AQI, y = covid_death_percentage, color = urbanicity)) +
      geom_point(stat = "identity") +
      geom_smooth(method = lm, se = FALSE) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      labs(title = "Median AQI vs Percentage of Total Deaths Involving COVID Across Counties in 2021", x = "Median AQI", y = "Percentage of Total Deaths Involving COVID", color = "Urbanicity")
    print(plot4)
  })
  
  # Page 3
  
  output$plot5 <- renderPlot({
    as
  })
  
  # further clean and/or manipulate the data based on the input from the widgets
  # any code that has input$ or output$ (ex. Your chart or a dataframe that will updated based on user input 
  # insert code for chart here        
  
}

