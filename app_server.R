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
    if(input$page2.2 == "Total COVID Deaths") {
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
    } else if (input$page2.2 == "% of Deaths Involving COVID") {
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
  
  output$plot5 <- renderTable({
    if (input$page3 == "By Total Count of COVID Deaths") {
      plot5 <- county_table %>%
        select(region, subregion, urbanicity, deaths_covid) %>%
        distinct(.keep_all = TRUE) %>%
        arrange(-deaths_covid) %>%
        top_n(20)
    } else if (input$page3 == "By Percentage of Deaths") {
      plot5 <- county_table %>%
        select(region, subregion, urbanicity, covid_death_percentage) %>%
        distinct(.keep_all = TRUE) %>%
        arrange(-covid_death_percentage) %>%
        top_n(20)
    }
  })
  
  output$plot6 <- renderTable({
    if (input$page3 == "By Total Count of COVID Deaths") {
      plot5 <- county_table %>%
        select(region, subregion, urbanicity, deaths_covid) %>%
        distinct(.keep_all = TRUE) %>%
        arrange(-deaths_covid) %>%
        top_n(20)
      plot6 <- plot5 %>%
        group_by(urbanicity) %>%
        summarize(count = n())
    } else if (input$page3 == "By Percentage of Deaths") {
      plot5 <- county_table %>%
        select(region, subregion, urbanicity, covid_death_percentage) %>%
        distinct(.keep_all = TRUE) %>%
        arrange(-covid_death_percentage) %>%
        top_n(20)
      plot6 <- covid_deaths_percentage_top_table %>%
        group_by(urbanicity) %>%
        summarise(count = n()) %>%
        arrange(-count)
    }
  })
  
  output$plot7 <- renderPlot({
    if (input$page4 == "By Unemployment Rate") {
      plot7 <- deaths_income_aqi %>%
        filter(!is.na(Unemployment_rate_2020)) %>%
        filter(!is.na(urbanicity)) %>%
        group_by(urbanicity) %>%
        summarise(mean_unemployment_rate_2020 = (mean(Unemployment_rate_2020) * 100)) %>%
        ggplot(mapping = aes(x = urbanicity, y = mean_unemployment_rate_2020)) +
        geom_bar(stat="identity", fill = "#A897F9") +
        scale_x_discrete(guide = guide_axis(n.dodge=3)) +
        labs(title = "Average Unemployment Rate in 2020 in Different Urbanicity Levels", x = "Levels of Urbanicity", y = "Mean Unemployment Rate in 2020")
    } else if (input$page4 == "By Household Income") {
      plot7 <- deaths_income_aqi %>%
        filter(!is.na(Median_Household_Income_2019)) %>%
        filter(!is.na(urbanicity)) %>%
        group_by(urbanicity) %>%
        summarise(mean_household_income_2019 = mean(Median_Household_Income_2019)) %>%
        ggplot(mapping = aes(x = urbanicity, y = mean_household_income_2019)) +
        geom_bar(stat="identity", fill = "#A897F9") +
        scale_x_discrete(guide = guide_axis(n.dodge=3)) +
        labs(title = "Average Household Income in 2019 in Different Urbanicity Levels", x = "Levels of Urbanicity", y = "Mean Household Income in 2019")
    } else if (input$page4 == "Unemployment Rate within Urbanicities") {
      plot7 <- county_table %>%
        filter(!is.na(covid_death_percentage)) %>%
        ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = urbanicity)) +
        geom_histogram(stat="identity") +
        labs(title = "Unemployment Rate in 2020 vs the Percentage of All Deaths Involving COVID Based on Urbanicity", x = "Unemployment Rate in 2020", y = "Percentage of All Deaths Involving COVID", color = "Urbanicity Category of each County")
    }
    print(plot7)
  })
  
  output$plot8 <- renderPlot({
    if (input$page4 == "By Unemployment Rate") {
      plot8 <- deaths_income_aqi %>%
        filter(!is.na(covid_death_percentage)) %>%
        ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = urbanicity)) +
        geom_point() +
        geom_smooth(method = lm) +
        stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE) +
        labs(title = "Unemployment Rate vs Percentage of Deaths Involving COVID by Urbanicity in 2020", x = "Unemployment Rate", y = "Percentage of Deaths Involving COVID", color = "Urbanicity")
    } else if (input$page4 == "By Household Income") {
      plot8 <- deaths_income_aqi %>%
        filter(!is.na(covid_death_percentage)) %>%
        filter(!is.na(Median_Household_Income_2019)) %>%
        ggplot(mapping = aes(x = as.numeric(Median_Household_Income_2019), y = covid_death_percentage, color = urbanicity)) +
        geom_point(stat = "identity") +
        geom_smooth(method = lm) +
        stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE) +
        labs(title = "Median Household Income in 2019 vs Percentage of Deaths Involving COVID by Urbanicity in 2020", x = "Median Household Income", y = "Percentage of Deaths Involving COVID", color = "Urbanicity")
    } else if (input$page4 == "Unemployment Rate within Urbanicities") {
      plot8 <- "..."
    }
    print(plot8)
  })
  
  output$plot9 <- renderPlot({
    if (input$page5 == "By Unemployment Rate and Total COVID Deaths") {
      plot9 <- covid_race_combine %>%
        filter(!is.na(cases)) %>%
        filter(!is.na(largestRace1)) %>%
        ggplot(mapping = aes(x = Unemployment_rate_2020, y = log(cases), color = largestRace1)) +
        geom_point(stat="identity") +
        geom_smooth(method = lm, se = FALSE) +
        stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE) +
        labs(title = "Unemployment Rate in 2020 vs Total COVID Cases per County Based on Race", x = "Unemployment Rate in 2020", y = "Total COVID Cases (Logrithmic)", color = "Most Prominent Race per County")
    } else if (input$page5 == "By Unemployment Rate and Percentage of Deaths due to COVID") {
      plot9 <- covid_race_combine %>%
        filter(!is.na(covid_death_percentage)) %>%
        filter(!is.na(largestRace1)) %>%
        ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = largestRace1)) +
        geom_point(stat="identity") +
        geom_smooth(method = lm) +
        stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE) +
        labs(title = "Unemployment Rate in 2020 vs the Percentage of All Deaths Involving COVID Based on Race", x = "Unemployment Rate in 2020", y = "Percentage of All Deaths Involving COVID", color = "Most Prominent Race per County")
    }
    print(plot9)
  })
  
  output$plot10 <- renderPlot({
    plot10 <- healthcare_combined %>%
      filter(!county_code == 51059) %>%
      filter(!county_code == 51760) %>%
      filter(!is.na(urbanicity)) %>%
      ggplot(mapping = aes(x = log(healthcare_fac_per_5000), y = covid_death_percentage, color = urbanicity)) +
      geom_point(stat="identity") +
      geom_smooth(method = lm) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +         
      labs(title = "Number of Healthcare Facilities per 5000 People in each County vs Percentage of Deaths Involving COVID in 2020", x = "# of Healthcare Facilities per 5000 People (Logrithmic)", y = "Percentage of All Deaths Involving COVID", color = "Urbanicity Category of each County")
    print(plot10)
  })

  output$urban_img <- renderUI({
    tags$img(src = "https://th.bing.com/th/id/R.9e1e6e5db937d0c77a2a35605eb4fb3d?rik=wVVBL9ii48BnOw&riu=http%3a%2f%2fcdn7.dissolve.com%2fp%2fD948_57_006%2fD948_57_006_0004_600.jpg&ehk=B8nvPAb%2by7rhx4RO%2fXMv1qWo%2f%2bcgKvqSJ4Bb2xqkdSA%3d&risl=&pid=ImgRaw&r=0")
  })
  
  output$rural_img <- renderUI({
    tags$img(src = "https://i.postimg.cc/tCKLxDnT/rural.jpg")
  })
  
  output$con_img <- renderUI({
    tags$img(src = "https://i.postimg.cc/t4ZMj9QY/conclusion.jpg")
  })
  
  # further clean and/or manipulate the data based on the input from the widgets
  # any code that has input$ or output$ (ex. Your chart or a dataframe that will updated based on user input 
  # insert code for chart here        
  
}

