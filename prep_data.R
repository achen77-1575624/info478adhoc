library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)
require(data.table)
library(scales)
library(maps)
library(mapproj)
library(patchwork)

# https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy
covid_deaths <- read.csv("./data/covid_death_county.csv",
                         stringsAsFactors = FALSE) %>%
  rename(state = State, county = County.name, county_code = FIPS.County.Code, urbanicity = Urban.Rural.Code, deaths_covid = Deaths.involving.COVID.19, deaths_all = Deaths.from.All.Causes) %>%
  mutate(across(county, tolower)) %>%
  mutate(county = trimws(county)) %>%
  select(!c("Date.as.of", "Start.Date", "End.Date", "Footnote"))

aqi_county <- read.csv("./data/annual_aqi_by_county_2021.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(across(County, tolower)) %>%
  mutate(State = state.abb[match(State,state.name)]) %>%
  filter(!is.na(State)) %>%
  rename(state = State, county = County, year = Year, max_AQI = Max.AQI, median_AQI = Median.AQI) %>%
  select(state, county, year, max_AQI, median_AQI)

# https://www.ers.usda.gov/data-products/county-level-data-sets
income_2019 <- read.csv("./data/unemployment_2019.csv",
                        stringsAsFactors = FALSE) %>%
  select(c("FIPS_Code", "State", "Area_name", "Employed_2019", "Unemployed_2019", "Unemployment_rate_2019", "Employed_2020", "Unemployed_2020", "Unemployment_rate_2020", "Median_Household_Income_2019")) %>%
  rename(county_code = FIPS_Code, state = State, county = Area_name) %>%
  filter(county_code != 0) %>%
  filter(county_code %% 1000 != 0) %>%
  mutate(across(county, tolower))
income_2019$county <- gsub("(.*),.*", "\\1", income_2019$county)

deaths_income <- as.data.table(merge(covid_deaths, income_2019,  by = c("county_code", "state", "county"), all.x = TRUE,all.y=TRUE))
deaths_income <- transform(deaths_income, county = sub(' city and borough', '', county))
deaths_income <- transform(deaths_income, county = sub(' city', '', county))
deaths_income <- transform(deaths_income, county = sub(' county', '', county))
deaths_income <- transform(deaths_income, county = sub(' borough', '', county))
deaths_income <- transform(deaths_income, county = sub(' census area', '', county))
deaths_income <- transform(deaths_income, county = sub(' municipality', '', county))

deaths_income_aqi <- as.data.table(merge(deaths_income, aqi_county,  by = c("state","county"), all.x = TRUE, all.y=TRUE)) %>%
  filter(!is.na(county_code)) %>%
  rename("region" = "state", "subregion" = "county") %>%
  mutate(covid_death_percentage = round((deaths_covid / deaths_all) * 100, 2))
deaths_income_aqi$region <- state.name[match(deaths_income_aqi$region,state.abb)]
deaths_income_aqi$region <- tolower(deaths_income_aqi$region)

# aqi_county <- transform(aqi_county, County = sub('city and borough', '', County))
# aqi_county <- transform(aqi_county, County = sub('city', '', County))
# aqi_county <- transform(aqi_county, County = sub('county', '', County))
# aqi_county <- transform(aqi_county, County = sub('borough', '', County))
# aqi_county <- transform(aqi_county, County = sub('census area', '', County))
# aqi_county <- transform(aqi_county, County = sub('municipality', '', County))


# require(data.table)
# covid_deaths<-as.data.table(covid_deaths)
# aqi_county<-as.data.table(aqi_county)
# covid_deaths[,County:=trimws(County)]

## Page 2. There are more deaths in large central metro, but that could just be because of population size. Let's look at it by
## percentage. We can see that large central metro still has a large percentage of its deaths in 2021 be from COVID, but so
## are a large percentage of deaths in the small metro and micropolitan urbanicities.
urbanicity_deaths_plot <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(aes(x = urbanicity, y = deaths_covid)) +
  geom_bar(stat="identity", fill = "#A897F9") +
  scale_y_continuous(name="Total Deaths Involving COVID 19", labels = comma) +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  labs(title = "Total Deaths Involving COVID in Different Urbanicity Levels", x = "Levels of Urbanicity")

urbanicity_deaths_percentage_plot <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  group_by(urbanicity) %>%
  summarize(average_percent_deaths = mean(covid_death_percentage, na.rm = TRUE)) %>%
  ggplot(aes(x = urbanicity, y = average_percent_deaths)) +
  geom_bar(stat="identity", fill = "#A897F9") +
  scale_y_continuous(name="Percentage of Deaths Involving COVID 19", labels = comma) +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  labs(title = "Percentage of Deaths Involving COVID in Different Urbanicity Levels", x = "Levels of Urbanicity")


## Map Section

## Initialize Map
us_counties <- map_data("county")

county_table <- as.data.table(merge(deaths_income_aqi, us_counties,  by = c("region","subregion"), all.x = TRUE, all.y=TRUE))


## Page 3. AQI, Deaths, and Deaths Percentage vs County Maps
aqi_county_map <- county_table %>%
  ggplot(aes(x = long, y = lat, group = group, fill = median_AQI)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

aqi_county_map <- county_table %>%
  ggplot(aes(x = long, y = lat, group = group, fill = median_AQI)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

covid_deaths_county_map <- county_table %>%
  ggplot(aes(x = long, y = lat, group = group, fill = deaths_covid)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

covid_deaths_percentage_county_map <- county_table %>%
  ggplot(aes(x = long, y = lat, group = group, fill = covid_death_percentage)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())


## Page 4. Tables that show top counties by death and by death percentage and their urbanicities.

covid_deaths_top_table <- county_table %>%
  select(region, subregion, urbanicity, deaths_covid) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(-deaths_covid) %>%
  top_n(20)
covid_deaths_top_urbanicity <- covid_deaths_top_table %>%
  group_by(urbanicity) %>%
  summarize(count = n())

covid_deaths_percentage_top_table <- county_table %>%
  select(region, subregion, urbanicity, covid_death_percentage) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(-covid_death_percentage) %>%
  top_n(20)
covid_percentage_top_urbanicity <- covid_deaths_percentage_top_table %>%
  group_by(urbanicity) %>%
  summarize(count = n()) %>%
  arrange(-count)

## page 5, covid deaths vs unemployment rates and household income

#plug in urbanicity_deaths_percentage_plot here from above to compare with this one
urbanicity_unemployment_2020_plot <- deaths_income_aqi %>%
  filter(!is.na(Unemployment_rate_2020)) %>%
  group_by(urbanicity) %>%
  summarize(mean_unemployment_rate_2020 = (mean(Unemployment_rate_2020) * 100)) %>%
              ggplot(mapping = aes(x = urbanicity, y = mean_unemployment_rate_2020)) +
              geom_bar(stat="identity", fill = "#A897F9") +
              scale_x_discrete(guide = guide_axis(n.dodge=3)) +
              labs(title = "Unemployment Rate in 2020 in Different Urbanicity Levels", x = "Levels of Urbanicity")
## there isn't much of a correlation here, but larger cities tend to have more unemployment rates
            
unemployment_deaths_percentage_scatter <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = urbanicity)) +
  geom_point() +
  labs(title = "Unemployment Rate vs Percentage of Deaths Involving COVID by Urbanicity in 2020", x = "Unemployment Rate", y = "Percentage of Deaths Involving COVID", color = "Urbanicity")

income_deaths_percentage_scatter <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  filter(!is.na(Median_Household_Income_2019)) %>%
  ggplot(mapping = aes(x = Median_Household_Income_2019, y = covid_death_percentage, color = urbanicity)) +
  geom_point(stat = "identity") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Median Household Income in 2019 vs Percentage of Deaths Involving COVID by Urbanicity in 2020", x = "Median Household Income", y = "Percentage of Deaths Involving COVID", color = "Urbanicity")
## there doesn't seem to be a correlation here with income and percentage deaths, but it's very slight with unemployment.
## you can see that most people in large fringe metro are either unemployed or make a lot of money, almost 100,000

# https://covidtracking.com/race
covid_race_combine <- read.csv("./data/covid-county-by-race.csv",
                                 stringsAsFactors = FALSE) %>%
  select(-c(deaths, deathsPer100k)) %>%
  rename("region" = "state", "subregion" = "countyName") %>%
  mutate(across(region, tolower)) %>%
  mutate(across(subregion, tolower)) %>%
  rename("total pop" = total)
covid_race_combine <- transform(covid_race_combine, subregion = sub(' city and borough', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' city', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' county', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' borough', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' census area', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' municipality', '', subregion))
county_table <- as.data.table(merge(county_table, covid_race_combine,  by = c("region","subregion"), all.x = TRUE, all.y = FALSE))


## page 6. unemployment rate vs percentage covid deaths in context of urbanicity and in race
covid_race_scatter <- county_table %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = largestRace1)) +
  geom_histogram(stat="identity") +
  labs(title = "Unemployment Rate in 2020 vs the Percentage of All Deaths Involving COVID Based on Race", x = "Unemployment Rate in 2020", y = "Percentage of All Deaths Involving COVID", color = "Most Prominent Race per County")

urbanicity_deaths_percentage_scatter <- county_table %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = urbanicity)) +
  geom_histogram(stat="identity") +
  labs(title = "Unemployment Rate in 2020 vs the Percentage of All Deaths Involving COVID Based on Urbanicity", x = "Unemployment Rate in 2020", y = "Percentage of All Deaths Involving COVID", color = "Urbanicity Category of each County")
