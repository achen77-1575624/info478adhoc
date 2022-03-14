library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)
require(data.table)
library(scales)
library(maps)
library(mapproj)
library(ggpmisc)

# https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy
#2020
covid_deaths <- read.csv("./data/covid_death_county.csv",
                         stringsAsFactors = FALSE) %>%
  rename(state = State, county = County.name,
         county_code = FIPS.County.Code,
         urbanicity = Urban.Rural.Code,
         deaths_covid = Deaths.involving.COVID.19,
         deaths_all = Deaths.from.All.Causes) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = trimws(county)) %>%
  select(!c("Date.as.of", "Start.Date", "End.Date", "Footnote"))

# https://aqs.epa.gov/aqsweb/airdata/download_files.html#Meta
aqi_county <- read.csv("./data/annual_aqi_by_county_2021.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(County = tolower(County)) %>%
  mutate(State = state.abb[match(State,state.name)]) %>%
  filter(!is.na(State)) %>%
  rename(state = State, county = County, year = Year, max_AQI = Max.AQI, median_AQI = Median.AQI, unhealthy_sensitive_days = Unhealthy.for.Sensitive.Groups.Days) %>%
  select(state, county, year, max_AQI, median_AQI, Days.Ozone, unhealthy_sensitive_days)

# https://www.ers.usda.gov/data-products/county-level-data-sets
income_2019 <- read.csv("./data/unemployment_2019.csv",
                        stringsAsFactors = FALSE) %>%
  select(c("FIPS_Code", "State", "Area_name", "Employed_2019", "Unemployed_2019", "Unemployment_rate_2019", "Employed_2020", "Unemployed_2020", "Unemployment_rate_2020", "Median_Household_Income_2019")) %>%
  rename(county_code = FIPS_Code, state = State, county = Area_name) %>%
  filter(county_code != 0) %>%
  filter(county_code %% 1000 != 0) %>%
  mutate(county = tolower(county))
income_2019$county <- gsub("(.*),.*", "\\1", income_2019$county)
income_2019 <- income_2019 %>%
  mutate(Employed_2019 = as.numeric(gsub(",","", Employed_2019)),
         Unemployed_2019 = as.numeric(gsub(",","", Unemployed_2019)),
         Employed_2020 = as.numeric(gsub(",","", Employed_2020)),
         Unemployed_2020 = as.numeric(gsub(",","", Unemployed_2020)),
         Median_Household_Income_2019 = as.numeric(gsub(",","", Median_Household_Income_2019)))

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



## Page 2. There are more deaths in large central metro, but that could just be because of population size. Let's look at it by
## percentage. We can see that large central metro still has a large percentage of its deaths in 2021 be from COVID, but so
## are a large percentage of deaths in the small metro and micropolitan urbanicities.
deaths_income_aqi <- deaths_income_aqi %>%
  mutate(urbanicity = fct_relevel(urbanicity, 
                                  "Large central metro", "Large fringe metro", "Medium metro", 
                                  "Small metro", "Micropolitan", "Noncore"))
urbanicity_deaths_plot <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(aes(x = urbanicity, y = deaths_covid)) +
  geom_bar(stat="identity", fill = "#A897F9") +
  scale_y_continuous(name="Total Deaths Involving COVID 19", labels = comma) +
  labs(title = "Total Deaths Involving COVID in Different Urbanicity Levels", x = "Levels of Urbanicity")

urbanicity_deaths_percentage_plot <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  group_by(urbanicity) %>%
  summarise(average_percent_deaths = mean(covid_death_percentage, na.rm = TRUE)) %>%
  ggplot(aes(x = urbanicity, y = average_percent_deaths)) +
  geom_bar(stat="identity", fill = "#A897F9") +
  scale_y_continuous(name="Percentage of Deaths Involving COVID 19", labels = comma) +
  labs(title = "Percentage of Deaths Involving COVID in Different Urbanicity Levels", x = "Levels of Urbanicity")


## Page 3
## Map Section

## Initialize Map
us_counties <- map_data("county")

county_table <- as.data.table(merge(deaths_income_aqi, us_counties,  by = c("region","subregion"), all.x = TRUE, all.y=TRUE))


## Page 3. AQI, Deaths, and Deaths Percentage vs County Maps
aqi_county_map <- county_table %>%
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

ozone_county_map <- county_table %>%
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

unhealthy_county_map <- county_table %>%
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

covid_deaths_county_map <- county_table %>%
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

covid_deaths_percentage_county_map <- county_table %>%
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

aqi_deaths_percentage_scatter <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  filter(!is.na(median_AQI)) %>%
  ggplot(mapping = aes(x = median_AQI, y = covid_death_percentage, color = urbanicity)) +
  geom_point(stat = "identity") +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title = "Median AQI vs Percentage of Total Deaths Involving COVID Across Counties in 2021", x = "Median AQI", y = "Percentage of Total Deaths Involving COVID", color = "Urbanicity")

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
covid_deaths_percentage_top_urbanicity <- covid_deaths_percentage_top_table %>%
  group_by(urbanicity) %>%
  summarise(count = n()) %>%
  arrange(-count)

## page 5, covid deaths vs unemployment rates and household income

#plug in urbanicity_deaths_percentage_plot here from above to compare with this one
urbanicity_unemployment_2020_plot <- deaths_income_aqi %>%
  filter(!is.na(Unemployment_rate_2020)) %>%
  filter(!is.na(urbanicity)) %>%
  group_by(urbanicity) %>%
  summarise(mean_unemployment_rate_2020 = (mean(Unemployment_rate_2020) * 100)) %>%
              ggplot(mapping = aes(x = urbanicity, y = mean_unemployment_rate_2020)) +
              geom_bar(stat="identity", fill = "#A897F9") +
              scale_x_discrete(guide = guide_axis(n.dodge=3)) +
              labs(title = "Average Unemployment Rate in 2020 in Different Urbanicity Levels", x = "Levels of Urbanicity", y = "Mean Unemployment Rate in 2020")

urbanicity_income_2019_plot <- deaths_income_aqi %>%
  filter(!is.na(Median_Household_Income_2019)) %>%
  filter(!is.na(urbanicity)) %>%
  group_by(urbanicity) %>%
  summarise(mean_household_income_2019 = mean(Median_Household_Income_2019)) %>%
  ggplot(mapping = aes(x = urbanicity, y = mean_household_income_2019)) +
  geom_bar(stat="identity", fill = "#A897F9") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  labs(title = "Average Household Income in 2019 in Different Urbanicity Levels", x = "Levels of Urbanicity", y = "Mean Household Income in 2019")
## there isn't much of a correlation here, but larger cities tend to have more unemployment rates
            
unemployment_deaths_percentage_scatter <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = urbanicity)) +
  geom_point() +
  geom_smooth(method = lm) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title = "Unemployment Rate vs Percentage of Deaths Involving COVID by Urbanicity in 2020", x = "Unemployment Rate", y = "Percentage of Deaths Involving COVID", color = "Urbanicity")

#hypothesis: in noncore, lot of people are employed in places where they have to interact with others
# the less unemployment there is, the more risk of contracting and dying from covid
#larger cities have jobs where you can do online, so it might lead to less

income_deaths_percentage_scatter <- deaths_income_aqi %>%
  filter(!is.na(covid_death_percentage)) %>%
  filter(!is.na(Median_Household_Income_2019)) %>%
  ggplot(mapping = aes(x = as.numeric(Median_Household_Income_2019), y = covid_death_percentage, color = urbanicity)) +
  geom_point(stat = "identity") +
  geom_smooth(method = lm) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title = "Median Household Income in 2019 vs Percentage of Deaths Involving COVID by Urbanicity in 2020", x = "Median Household Income", y = "Percentage of Deaths Involving COVID", color = "Urbanicity")
## there doesn't seem to be a correlation here with income and percentage deaths, but it's very slight with unemployment.
## you can see that most people in large fringe metro are either unemployed or make a lot of money, almost 100,000

# https://covidtracking.com/race
covid_race_combine <- read.csv("./data/covid-county-by-race.csv",
                                 stringsAsFactors = FALSE) %>%
  select(-c(deaths, deathsPer100k)) %>%
  rename("region" = "state", "subregion" = "countyName") %>%
  mutate(region = tolower(region)) %>%
  mutate(subregion = tolower(subregion)) %>%
  rename("total pop" = total)
covid_race_combine <- transform(covid_race_combine, subregion = sub(' city and borough', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' city', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' county', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' borough', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' census area', '', subregion))
covid_race_combine <- transform(covid_race_combine, subregion = sub(' municipality', '', subregion))
covid_race_combine <- as.data.table(merge(deaths_income_aqi, covid_race_combine,  by = c("region","subregion"), all.x = TRUE, all.y = FALSE))


## page 6. unemployment rate vs percentage covid cases and deaths in context of urbanicity and in race. you can see how
## hispanic people are hit the hardest with higher unemployment
cases_unemployment_race_scatter <- covid_race_combine %>%
  filter(!is.na(cases)) %>%
  filter(!is.na(largestRace1)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = log(cases), color = largestRace1)) +
  geom_point(stat="identity") +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title = "Unemployment Rate in 2020 vs Total COVID Cases per County Based on Race", x = "Unemployment Rate in 2020", y = "Total COVID Cases (Logrithmic)", color = "Most Prominent Race per County")

deaths_percentage_unemployment_race_scatter <- covid_race_combine %>%
  filter(!is.na(covid_death_percentage)) %>%
  filter(!is.na(largestRace1)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = largestRace1)) +
  geom_point(stat="identity") +
  geom_smooth(method = lm) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title = "Unemployment Rate in 2020 vs the Percentage of All Deaths Involving COVID Based on Race", x = "Unemployment Rate in 2020", y = "Percentage of All Deaths Involving COVID", color = "Most Prominent Race per County")
## for asian alone, it's a perfect negative correlation. looking closely, there are so few asian prominent counties that it's probably not right

# when interpreting, think about how unemployment rate could be looking at not jutst that race/ethnicity; when you
# are looking for a job, you're willing to take anything

## here, unemployment seems to hit larger areas that are more urban than smaller ones
urbanicity_unemployment_deaths_percentage_histogram <- county_table %>%
  filter(!is.na(covid_death_percentage)) %>%
  ggplot(mapping = aes(x = Unemployment_rate_2020, y = covid_death_percentage, color = urbanicity)) +
  geom_histogram(stat="identity") +
  labs(title = "Unemployment Rate in 2020 vs the Percentage of All Deaths Involving COVID Based on Urbanicity", x = "Unemployment Rate in 2020", y = "Percentage of All Deaths Involving COVID", color = "Urbanicity Category of each County")

## page 7. hospitals per 100,000 in each county and rates of covid death

#https://data.bls.gov/cew/apps/table_maker/v4/table_maker.htm#type=1&year=2020&qtr=A&own=2&ind=62&supp=0
healthcare_2020 <- read.csv("./data/healthcare_2020.csv",
                            stringsAsFactors = FALSE) %>%
  mutate(area_fips = as.numeric(area_fips)) %>%
  filter(area_fips %% 1000 != 0) %>%
  group_by(area_fips) %>%
  summarise(average_establishments = round(mean(annual_avg_estabs), 2), average_employment = round(mean(annual_avg_emplvl), 2))

#https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/
census_2020 <- read.csv("./data/census_2020.csv",
                            stringsAsFactors = FALSE) %>%
  rename(region = STNAME, subregion = CTYNAME, population_2020 = POPESTIMATE2020) %>%
  filter(!SUMLEV == 40) %>%
  mutate(region = tolower(region)) %>%
  mutate(subregion = tolower(subregion)) %>%
  select(region, subregion, population_2020)
census_2020 <- transform(census_2020, subregion = sub(' city and borough', '', subregion))
census_2020 <- transform(census_2020, subregion = sub(' city', '', subregion))
census_2020 <- transform(census_2020, subregion = sub(' county', '', subregion))
census_2020 <- transform(census_2020, subregion = sub(' borough', '', subregion))
census_2020 <- transform(census_2020, subregion = sub(' census area', '', subregion))
census_2020 <- transform(census_2020, subregion = sub(' municipality', '', subregion))

census_healthcare_2020 <- as.data.table(merge(census_2020, deaths_income_aqi,  by = c("region", "subregion"), all.x = TRUE,all.y=TRUE)) %>%
  select(region, subregion, county_code, population_2020)
healthcare_2020 = healthcare_2020 %>%
  rename(county_code = area_fips)
census_healthcare_2020 <- as.data.table(merge(healthcare_2020, census_healthcare_2020, by = "county_code"), all.x = TRUE, all.y = FALSE) %>%
  mutate(healthcare_fac_per_5000 = round(average_establishments /  (population_2020 / 5000), 2))

healthcare_combined <- as.data.table(merge(census_healthcare_2020, deaths_income_aqi,  by = c("county_code", "region", "subregion"), all.x = TRUE,all.y=FALSE))
  
facilities_deaths_scatter <- healthcare_combined %>%
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
