# load your libraries
# read in the data set 
source("app_server.R")

# Define Widgets (shiny widget library here)

# Define structure of tabs (aka pages) -- must make 2 tabs
intro <- tabPanel(
  "Introduction",
    mainPanel(
      h1("The Effects of Urbanicity and SES Factors on COVID Prevalence and Deaths"),
      h2("Introduction"),
      p("Even into 2022, the COVID-19 pandemic continues to rage through the world, affecting businesses,
        policy, travel, and population health. COVID-19 is the illness caused by the SARS-CoV-2 coronavirus.
        Its effects can range from mild or asymptomatic to debilitating symptoms such as difficulty breathing
        or even death (Coronavirus (who.int)). On a global level, different countries have enacted recommended
        policies at different rates to attempt to control the spread of the virus, with China and South Africa's
        national governments enacting strict and compulsory containment measures, while the U.S. and Japan's
        national governments had less power in such measures in the beginning of the pandemic
        (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8178944/). However, even within a country, differences in
        geological location can affect the risk of COVID-19 exposure and death. Urban and rural environments
        inherently expose people to different living conditions that could affect their air quality, proximity to
        other people, and availability of health resources like emergency care. According to the CDC, 2020 showed
        a pattern of urban locations being hit the hardest by COVID-19, which rippled into rural communities later
        (https://www.cdc.gov/mmwr/volumes/69/wr/mm6946a6.htm). It could be said that urban areas would
        consistently have more grave COVID-19 outcomes, although urban areas also have higher access to
        healthcare. Comparing 2015-2017 and 2005-2007, the ratio between years of life lost before age 75 in
        urban and rural communities increased, further separating the disparity of healthcare access. Rural
        areas also spend less on healthcare infrustructures relative to the amount of healthcare workers relative
        to urban areas (The State of Rural Public Health: Enduring Needs in a New Decade (nih.gov)). These
        factors complicate the notion that every factor that comes with urbanicity will increase COVID-19 rates
        and deaths."),
      h2("Hypothesis"),
      p("I hypothesize that with increased urbanicity and factors that come with it (air pollution, population density),
        the rates of COVID-19 infections and deaths will increase. This might be moderated by certain socioeconomic
        factors such as income and ethnicity, as income may be a deciding factor in whether or not someone would be
        able to afford healthcare, and ethnicity for its possible effects in population congregations. However, these
        results may be negatively mediated by the lack of availability of health resources in rural communities."),
      h2("Methods"),
      p("Data manipulation and analysis will be computed in the R programming language. Datasets in .csv format
        regarding the multiple urbanicity, socioeconomic, and COVID factors will be obtained from multiple government
        database collections. In particular, the Center for Disease Control's website contains a publicly accessible
        set of datasets for a variety of population health parameters."),
      p("Ideally, data collected will be on the county level, which is the most convenient way to separate the
        geography of a state while maintaining the general population proportions from urban/rural settings. Datasets
        collected should be relevant for 2021."),
      p("Average annual air quality data will be obtained at the county level from the United States Environmental
        Protection Agency (EPA). COVID deaths will also be obtained at the county level, but from the CDC's website.
        The COVID deaths data also contains its own classifications of each county in the United States based on its
        urbanicity. These classifications will also be aggregated into the rest of the county-level data.
        Availability of health resources will be measured as the amount of healthcare facilities available in a
        given county. For datasets that don't include county distinctions on a country-wide level, such as the
        availability of health resources, they will have to be aggregated and combined together with R before analysis."),
      p("Multivariate analysis will be done in R, and map plots and scatter plots will be made that include regression
        analysis across multiple factors compared to each other. In particular, air pollution data and population
        density data will be compared with COVID cases, and income, ethnicity, and availability of health resources
        will be compared with COVID deaths.")
    )
)

page_1 <- tabPanel(
  "Deaths by Urbanicity",
  sidebarLayout(
    sidebarPanel(
      radioButtons("page1", label = h3("Select box"),
                  choices = c("Total COVID Deaths", "% of Deaths Involving COVID"), 
                  selected = "Total COVID Deaths")
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)


page_2 <- tabPanel(
  "Factors Across Counties",
  sidebarLayout(
    sidebarPanel(
      radioButtons("page2", label = h3("Select box1"),
                   choices = c("AQI", "Ozone Caution", "Unhealthy for Sensitive Groups Days"),
                   selected = "AQI"),
      radioButtons("page3", label = h3("Select box2"),
                   choices = c("Total COVID Deaths", "% of Deaths Involving COVID"),
                   selected = "Total COVID Deaths")
    ),
    mainPanel(
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4")
    )
  )
)

page_3 <- tabPanel(
  "Top Counties",             #title of the page, what will appear as the tab name
  sidebarLayout(             
    sidebarPanel( 
      # left side of the page 
      # insert widgets or text here -- their variable name(s), NOT the raw code
    ),           
    mainPanel(                # typically where you place your plots + texts
      # insert chart and/or text here -- the variable name NOT the code
    )))

page_4 <- tabPanel(
  "Socioeconomic Factors",             #title of the page, what will appear as the tab name
  sidebarLayout(             
    sidebarPanel( 
      # left side of the page 
      # insert widgets or text here -- their variable name(s), NOT the raw code
    ),           
    mainPanel(                # typically where you place your plots + texts
      # insert chart and/or text here -- the variable name NOT the code
    )))

page_5 <- tabPanel(
  "Race and Ethnicity",             #title of the page, what will appear as the tab name
  sidebarLayout(             
    sidebarPanel( 
      # left side of the page 
      # insert widgets or text here -- their variable name(s), NOT the raw code
    ),           
    mainPanel(                # typically where you place your plots + texts
      # insert chart and/or text here -- the variable name NOT the code
    )))

page_6 <- tabPanel(
  "Access to Facilities",             #title of the page, what will appear as the tab name
  sidebarLayout(             
    sidebarPanel( 
      # left side of the page 
      # insert widgets or text here -- their variable name(s), NOT the raw code
    ),           
    mainPanel(                # typically where you place your plots + texts
      # insert chart and/or text here -- the variable name NOT the code
    )))

conclusion <- tabPanel(
  "Conclusion",             #title of the page, what will appear as the tab name
  sidebarLayout(             
    sidebarPanel( 
      # left side of the page 
      # insert widgets or text here -- their variable name(s), NOT the raw code
    ),           
    mainPanel(                # typically where you place your plots + texts
      # insert chart and/or text here -- the variable name NOT the code
    )))

# Define the UI and what pages/tabs go into it
ui <- tags$div(class="container",
      fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      setBackgroundColor(
        color = c("#F7FBFF", "#f7e7b4"),
        gradient = "linear",
        direction = "bottom"
      ),
        navbarPage(
        "Urbanicity and COVID",
        intro,
        page_1,
        page_2,
        page_3,
        page_4,
        page_5,
        page_6,
        conclusion
        )
  #insert other pages here
      )
)
