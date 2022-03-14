library("tidyverse")
library("shiny")

# load your libraries
# read in the data set 
source("app_server.R")

# Define Widgets (shiny widget library here)

# Define structure of tabs (aka pages) -- must make 2 tabs
intro <- tabPanel(
  "Introduction",
    mainPanel(width = 12,
      h1("The Effects of Urbanicity and SES Factors on COVID Deaths"),
      h2("Introduction"),
      p("Even into 2022, the COVID-19 pandemic continues to rage through the world, affecting businesses,
        policy, travel, and population health. COVID-19 is the illness caused by the SARS-CoV-2 coronavirus.
        Its effects can range from mild or asymptomatic to debilitating symptoms such as difficulty breathing
        or even death (World Health Organization, n.d.). On a global level, different countries have enacted
        policies at different rates to attempt to control the spread of the virus, with China and South Africa's
        national governments enacting strict and compulsory containment measures, while the U.S. and Japan's
        national governments had less power in such measures in the beginning of the pandemic
        (Wang & Mao, 2021). However, even within a country, differences in
        geological location can affect the risk of death from COVID-19. Urban and rural environments
        inherently expose people to different living conditions that could affect their air quality, proximity to
        other people, and availability of health resources like emergency care. According to the CDC, 2020 showed
        a pattern of urban locations being hit the hardest by COVID-19, which rippled into rural communities later
        (COVID-19 stats, 2020). It could be said that urban areas would
        consistently have more grave COVID-19 outcomes, although urban areas also have higher access to
        healthcare. Comparing 2015-2017 and 2005-2007, the ratio between years of life lost before age 75 in
        urban and rural communities increased, further separating the disparity of healthcare access. Rural
        areas also spend less on healthcare infrustructures relative to the amount of healthcare workers relative
        to urban areas (Leider et al., 2020). These factors complicate the notion that every aspect that comes 
        with increased urbanicity will also increase COVID-19 rates and deaths."),
      h2("Hypothesis"),
      p("I hypothesize that with increased urbanicity and factors that come with it like air pollution,
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
        collected should be relevant for the duration of the COVID pandemic so far (2019-2022)."),
      p("Average annual air quality data will be obtained at the county level from the United States Environmental
        Protection Agency (EPA). COVID deaths will also be obtained at the county level, but from the CDC's website.
        The COVID deaths data also contains its own classifications of each county in the United States based on its
        urbanicity. These classifications will also be aggregated into the rest of the county-level data.
        Availability of health resources will be measured as the amount of healthcare facilities available in a
        given county. For datasets that don't include county distinctions on a country-wide level, such as the
        availability of health resources, they will have to be aggregated and combined together with R before analysis."),
      p("Multivariate analysis will be done in R, and map plots and scatter plots will be made that include regression
        analyses across multiple factors compared to each other. In particular, air pollution data and urbanicity
        categorization data will be compared with COVID cases, and income, ethnicity, and availability of 
        health resources will be compared with COVID deaths."),
      column(6, uiOutput("urban_img")) %>%
              tagAppendAttributes(class = 'intro_img'),
      column(6, uiOutput("rural_img")) %>%
              tagAppendAttributes(class = 'intro_img')
    )
)

page_1 <- tabPanel(
  "Deaths by Urbanicity",
  sidebarLayout(
    sidebarPanel(
      radioButtons("page1", label = h3("Sort by:"),
                  choices = c("Total COVID Deaths", "% of Deaths Involving COVID"), 
                  selected = "Total COVID Deaths")
    ),
    mainPanel(
      plotOutput("plot1"),
      p("Looking at the number of deaths involving COVID throughout the COVID pandemic in 2020-2022,
        we can see that there were significantly more deaths happening in more urban areas. The bar
        chart shows the different county categories sorted from largest to smallest. Large fringe
        metro counties had almost 300,000 deaths while noncore (most rural) counties had less than 50,000.
        However, this could mostly be due to the fact that there are normally more deaths in urban areas
        anyway."),
      p("In order to normalize the deaths by population, we can calculate the percentage of all deaths
        that were due to COVID (%CD) to see what communities are most impacted by COVID. Looking at the second
        bar graph, we can see that the gap between the urban and rural counties is not so large after controlling
        for population size. While large fringe metro counties still have a higher %CD at around
        16%, small metro counties actually come in second place with about 13%. Noncore counties are not so far
        behind with about 9.5%")
    )
  )
)


page_2 <- tabPanel(
  "Air Quality Across Counties",
  sidebarLayout(
    sidebarPanel(
      radioButtons("page2", label = h3("Sort air quality by:"),
                   choices = c("AQI", "Ozone Caution", "Unhealthy for Sensitive Groups Days"),
                   selected = "AQI"),
      radioButtons("page2.2", label = h3("Sort deaths by:"),
                   choices = c("Total COVID Deaths", "% of Deaths Involving COVID"),
                   selected = "Total COVID Deaths"),
      p(""),
      p("More urban areas are usually at higher risk of poorer air quality due to gas emissions and
        less greenery. On the top map, we can see the distribution of different air quality indicators across
        different U.S. counties in 2021. No matter if it's by median AQI, days with ozone caution,
        or days considered 'Unhealthy for Sensitive Groups,' the southwest is consistently brighter
        on the heatmap. This means that the counties on the southwest, including those in California and New
        Mexico, had more days in 2021 with poor air quality. However, this may have been due to the many
        wildfires that were consuming the southwest at the time."),
      p("The bottom map shows the distribution of total COVID deaths or %CD. Again, the southwest is
        consistently brighter than the rest of the United States on total deaths. The results are much more spread out
        when sorting by %CD, and since COVID still makes up a small (yet impactful) portion of deaths, 
        the actual percentage differences between the southwest and the rest of the United States aren't 
        considerably large. But combined with the rest of the maps, it's clear that the southwest has been most impacted.")
    ),
    mainPanel(
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4"),
      p("This scatterplot shows each county plotted by its median AQI in 2021 and percentage of all deaths
        attributable to COVID. Each county is color-coded by its urbanicity. While there aren't any obvious
        trends, there are some weak correlations. Medium metros have a very slight positive correlation
        between median AQI and %CD with an R^2 value of 0.07.")
    )
  )
)

page_3 <- tabPanel(
  "Top Counties",
  sidebarLayout(
    sidebarPanel(
      radioButtons("page3", label = h3("Sort by:"),
                   choices = c("By Total Count of COVID Deaths", "By Percentage of Deaths"),
                   selected = "By Total Count of COVID Deaths"),
    ),           
    mainPanel(width = 8,
      tableOutput("plot5"),
      tableOutput("plot6"),
      p("As seen in the maps, most of the counties most affected by COVID were in the southwest.
        Looking at the top 20 counties affected by COVID, sorting by total COVID deaths supports
        this and further shows how most of these counties are large metros. 17 of the top counties are considered
        large central metros, and 3 of the top counties are considered large fringe metros."),
      p("Sorting by %CD tells a completely different story, however. Here, noncore counties take up
        8 of the 20 top counties, while small and medium metros make up the next 7. Again, this
        validates the notion that while there are cumulatively more deaths in more urban areas,
        rural areas are just about, if not more impacted by COVID deaths.")
    )
  )
)

page_4 <- tabPanel(
  "Economic Factors",
  sidebarLayout(             
    sidebarPanel( 
      radioButtons("page4", label = h3("Sort by:"),
                   choices = c("By Unemployment Rate", "By Household Income", "Unemployment Rate within Urbanicities"),
                   selected = "By Unemployment Rate"),
    ),           
    mainPanel(
      plotOutput("plot7"),
      plotOutput("plot8"),
      p("Sorting by unemployment rate shows us how it slightly increases as you go from noncore counties
        to large fringe metros, and then it dramatically jumps up in large central metros. Looking at the
        scatterplot underneath, large central metros also have the strongest correlation between unemployment
        rate and %CD with an R^2 value of 0.23. While this still isn't considered a strong correlation, it
        is still the most meaningful one. Interestingly enough, noncore counties had a smaller %CD the more
        unemployment there was, but only has an R^2 value of -0.07."),
      p("Sorting by average household income in 2019 shows less conclusive results. The average household
        income of large fringe metro counties rose higher than that of large central metros, and the scatterplot
        underneath does not have any meaningful correlations at all. The strongest correlation between
        the median household income of counties in 2019 with %CD throughout the pandemic was a negative one
        in micropolitan areas with an R^2 value of -0.04. This goes hand-in-hand with how noncore counties
        had a very slight negative correlation between unemployment rates and %CD too"),
      p("The histogram in 'Unemployment Rate within Urbanicities' support how unemployment rate is a much
        more deciding factor in how COVID can impact a community. Communities across different most urbanicities
        have similar unemployment rates, but unemployment affected large central metro counties much more
        drastically. %CD was dramatically higher in large central metros than other urbanicities of similar
        unemployment rates."),
      p("The reasoning for large central metro counties being affected more drastically could be the different
        work opporutunities available in different urbanicities. In noncore and smaller counties, many people
        are employed in places where they have to interact with others. The higher the unemployment rate,
        the less that people would be in contact with others, and the lower the unemployment rate, the higher
        the risk of contracting and dying from COVID. Meanwhile, larger cities have jobs that have
        work-from-home opportunities. By not going in person, higher employment doesn't necessarily equate
        to higher COVID impact (Holgersen et al., 2021).")
    )
  )
)

page_5 <- tabPanel(
  "Race and Ethnicity",
  sidebarLayout(             
    sidebarPanel( 
      radioButtons("page5", label = h3("Sort by:"),
                   choices = c("By Unemployment Rate and Total COVID Deaths", "By Unemployment Rate and Percentage of Deaths due to COVID"),
                   selected = "By Unemployment Rate and Total COVID Deaths"),
    ),           
    mainPanel(
      plotOutput("plot9"),
      p("Knowing that unemployment rates have a higher moderating effect on COVID, we can factor this into
        race and ethnicity as well. In the scatterplots, each point represents a county, and they are color
        coded by their most prominent ethnicity group. Sorting by total COVID deaths, the scatterplot shows
        that unemployment affects counties where the prominent ethnicity groups are Hispanic or Latino populations
        or white populations the most. However, considering how most counties in the U.S. are predominantly white
        anyway, these results could be skewed."),
      p("Like with the other visualizations, sorting by %CD controls for population size of ethnicity groups.
        Here, unemployment affects counties where the predominant ethnicity groups are Hispanic or Latino, Native
        populations, and black or African American the most."),
      p("A very confusing part of these visualizations is how the calculated R^2 values don't seem to match
        with the lines of best fit. In the scatterplot with total deaths, counties with a predominantly Asian
        population have an R^2 value of 0.18, but the graph shows a downward trend. Sorting by %CD, these same
        counties with a prominent Asian population have an R^2 value of 1.00, but have the most dramatic
        'negative correlation' line of best fit. I assume that the values only talk about the strength
        of the correlation, not the direction. The counties with Asian dominant populations could have
        a perfect 1.00 negative correlation."),
      p("Consideirng how many counties in the United States have white dominant populations, there may not have
        been enough counties with other dominant ethnicities to where there wasn't enough data to support
        any significant strength of correlation at all.")
    )
  )
)

page_6 <- tabPanel(
  "Access to Facilities",
    mainPanel(width = 12,
      plotOutput("plot10"),
      p("Having access to healthcare could be a big reason why some communities have been more impacted
        than others. More healthcare facilities means more hospital beds that are available for the
        population. It's not fair to assume that urban areas have better ratios of healthcare facilities
        to the population though. In fact, healthcare utilization is not significantly different
        across urbanicities if socioeconomic factors are controlled for (Loftus et al., 2017).
        For this dataset, we will look at how many healthcare facilities
        are available per 5000 people to control for population size."),
      p("In this scatterplot, most urbanicities seem to have negative correlations with the number
        of healthcare facilities and the %CD. The more healthcare facilities there are, the less
        that COVID deaths impact a community. The strongest correlation here is with large central metro
        counties, with an R^2 value of -0.17. Another observation we can make is that noncore counties
        don't necessarily have less healthcare facilities for the population than larger central metros do.")
    )
)

conclusion <- tabPanel(
  "Conclusion",
    mainPanel(width = 12,
      h2("Conclusion"),
      p("Just like many other factors of daily living, there are disparities throughout different
        populations in the United States. For COVID impact, death is arguably the most deciding factor.
        From different categories of urbanicities, we can see that socioeconomic data and healthcare data
        also differ based on location. At first glance, large central metro counties seem to have been consistently
        impacted the most by COVID deaths. However, controlling for population size, we can see that noncore
        and other smaller counties are almost just as impacted by COVID deaths. My hypothesis is
        therefore only particially correct. Larger central metros are impacted more by COVID deaths, but
        this difference is not large at all between the urbanicities. However, factors like unemployment rate, air
        quality, and healthcare facilities seem to have much more of a moderating effect on COVID death outcome in
        large central metros, not noncore counties."),
      p("Judging by how noncore counties have just as many healthcare facilities as large central metro counties,
        it's also inconclusive to say that this is a decisive factor for the rates of death from COVID in noncore
        counties. Even so, having more healthcare facilities is slightly correlated with lower amounts of death
        being attributed to COVID across the board."),
      h2("Limitations"),
      p("One of the limitations of this project include the timeframe of the datasets gathered. The dataset
        for COVID deaths has data from 2020-2022 (Centers for Disease Control and Prevention, 2020), the dataset
        for air quality data was from 2021 (Environmental Protection Agency, 2021), the income data was taken from
        2019 (U.S. Department of Agriculture, 2019), the race and ethnicity data went as far as 2021 
        (The Atlantic, 2021), and the healthcare and census data were both from 2020 (U.S. Bureau of Labor 
        Statistics, 2020 ; United States Census Bureau, 2021). Because the timeframe isn't standardized, conclusions
        can only be made about the whole of the COVID pandemic instead of during a specific year."),
      p("The location data used to graph the maps were also not consistent. Some datasets had records of more counties
        than others. The COVID deaths dataset had data on 3,065 counties, while the air quality dataset only
        had data for 986 counties! Perhaps this difference was due to how many monitoring stations for air
        quality there are in the U.S. and how they are distributed."),
      p("Finally, there were many, many coviarate factors that were not looked at in this project. Political
        affiliations could impact COVID attitudes and exposure, leading to different rates of death.
        These political affiliations could also account for vaccine hesitancy, which would also affect
        deaths. A whole separate project could be conducted on political effects on COVID impact. Speaking of
        vaccine hesitancy, data on vaccine distribution and how much of the population in different urbanicities
        is vaccinated could affect impact as well, and could even tie into which ethnicity groups had easier
        access to vaccines. Maybe having more healthcare facilities also equates to higher vaccine rates.
        Without taking these factors into consideration, there is always the possibility that the results found in
        this study have hidden, covariate factors influencing them."),
      h2("Closing Thoughts"),
      p("As the pandemic continues on, data visualizations like these offer insights on specific
        populations that are at risk for negative outcomes. In this case, those who are unemployed and
        have less healthcare access are at higher risk of death from COVID especially if they live in
        larger central metros with poor air quality."),
      column(12, uiOutput("con_img")) %>%
        tagAppendAttributes(class = 'intro_img'),
    )
)

references <- tabPanel(
  "References",
  mainPanel(width = 12,
            h1("References") %>%
              tagAppendAttributes(class = 'references'),
            p("2020 Census. United States Census Bureau. (2021, May 4). Retrieved March 13, 2022, from 
              https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/"),
            p("The Atlantic. (2021). The Covid racial data tracker. The COVID Tracking Project. 
              Retrieved March 13, 2022, from https://covidtracking.com/race"),
            p("Centers for Disease Control and Prevention. (2020, April 21). 
              Provisional COVID-19 death counts in the United States by county. 
              Centers for Disease Control and Prevention. Retrieved March 13, 2022, 
              from https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy"),
            p("COVID-19 stats: Covid-19 incidence,* by urban-rural classification - United States, 
              January 22-October 31, 2020. (2020). MMWR. Morbidity and Mortality Weekly Report, 69(46), 1753. 
              https://doi.org/10.15585/mmwr.mm6946a6"),
            p("Environmental Protection Agency. (2021, November 24). Pre-generated data files. 
              EPA. Retrieved from https://aqs.epa.gov/aqsweb/airdata/download_files.html"),
            p("Holgersen, H., Jia, Z., & Svenkerud, S. (2021). Who and how many can work from home? 
              Evidence from task descriptions. Journal for Labour Market Research, 55(1). 
              https://doi.org/10.1186/s12651-021-00287-z"),
            p("Leider, J. P., Meit, M., McCullough, J. M., Resnick, B., Dekker, D., Alfonso, Y. N., & 
              Bishai, D. (2020). The state of rural public health: Enduring needs in a new decade. American 
              Journal of Public Health, 110(9), 1283-1290. https://doi.org/10.2105/ajph.2020.305728"),
            p("Loftus, J., Allen, E. M., Call, K. T., & Everson-Rose, S. A. (2017). Rural-urban differences 
              in access to preventive health care among publicly insured Minnesotans. The Journal of Rural Health, 
              34, 48-55. https://doi.org/10.1111/jrh.12235"),
            p("U.S. Bureau of Labor Statistics. (2020). Quarterly census of employment and wages. 
              U.S. Bureau of Labor Statistics. Retrieved March 13, 2022, from 
              https://data.bls.gov/cew/apps/table_maker/v4/table_maker.htm#type=1&amp;year=2020&amp;qtr=A&amp;own=2&amp;ind=62&amp;supp=0"),
            p("U.S. Department of Agriculture. (2019). County-level data sets. USDA Economic 
              Research Service. Retrieved March 13, 2022, from 
              https://www.ers.usda.gov/data-products/county-level-data-sets"),
            p("Wang, D., & Mao, Z. (2021). A comparative study of public health and social 
              measures of COVID-19 advocated in different countries. Health Policy, 125(8), 957-971. 
              https://doi.org/10.1016/j.healthpol.2021.05.016"),
            p("World Health Organization. (n.d.). Coronavirus. World Health Organization. 
              Retrieved from https://www.who.int/health-topics/coronavirus#tab=tab_1")
  )
)


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
        conclusion,
        references
        )
      )
)

