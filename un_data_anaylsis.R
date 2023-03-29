library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")


# summerise() - summarise data frame
summarise(gapminder_data, averagelifeExp=mean(lifeExp))

gapminder_data_summarized <-  gapminder_data %>% summarise(averagelifeExp=mean(lifeExp))
gapminder_data_summarized

#picking out variables from data
gapminder_data %>% summarise(recent_year = max(year))
rlang::last_trace()

# filter() -subsets the rows in a dataframe

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarise(averagelifeExp=mean(lifeExp))

#Find average GDP per capita for the first year in the data set
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarise(averageGDP=mean(gdpPercap))

gapminder_data %>% summarise(first_year=min(year))

gapminder_data %>% 
  filter(year==1952) %>% 
  summarise(average_gdp=mean(gdpPercap))

# group_by() - group values from a column
gapminder_data %>% 
  group_by(year) %>% 
  summarise(average_lifeExp=mean(lifeExp))

# Calc the average life expectancy by continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average_lifeExp=mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average_lifeExp=mean(lifeExp), min_lifeExp=min(lifeExp))

# mutate() - add or change a variable/column in a data frame
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

# making a new column called popInMillions that is the population in millions
gapminder_data_with_mutation <- gapminder_data %>% 
  mutate(popInMillions = pop / 1000000)

# select() - subsets columns in a dataframe
gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(-continent)

# create a data frame with only country, continent, year, and lifeExp
gapminder_data %>% 
  select(-pop, -gdpPercap)
gapminder_data %>% 
  select(country, continent, year, lifeExp)

# pivot_wider() & pivot_longer - changes shape of data frame
gapminder_data %>% 
  select(-pop, -gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#subsetting the gapminder_data to the year 2007 and drop the year and continent columns
gapminder_data %>% 
  filter(year == 2007) %>% 
  select(-year, -continent)

# data that is only from 2007 and only from the americas continent
gapminder_data %>% 
  filter(year == 2007) %>% 
  filter(continent == "Americas") %>% 
  select(-year, -continent)

gapminder_data_2007_Americas <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)
  
# is CO2 emissions related to GDP
co2_emssions_dirty <- read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emssions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)
# now each year and country pair have one row, total_emissions and per_capita_emissions have columns

co2_emssions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)

co2_emissions <- co2_emssions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

View(co2_emissions)

# inner_join()
inner_join(gapminder_data_2007_Americas, co2_emissions)
anti_join(gapminder_data_2007_Americas, co2_emissions)

#make names common between data sets
co2_emissions <- co2_emssions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) %>% 
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia", 
                          "United States of America"= "United States", 
                          "Venezuela (Boliv. Rep. of)"= "Venezuela"))
View(co2_emissions)

anti_join(gapminder_data_2007_Americas, co2_emissions)

gapminder_data_2007_Americas <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States"))
View(gapminder_data_2007_Americas)

gapminder_data_2007_Americas <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarise(lifeExp = sum(lifeExp * pop) / sum(pop), 
            gdpPercap = sum(gdpPercap * pop) / sum(pop), 
            pop = sum(pop))
View(gapminder_data_2007_Americas)  

anti_join(gapminder_data_2007_Americas, co2_emissions)
gapminder_co2 <- inner_join(gapminder_data_2007_Americas, co2_emissions)

View(gapminder_co2)

# is CO2 emissions related to GDP

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "GDP per capita)", y = "CO2 emitted (per capita)")

write_csv(gapminder_co2, "data/gapminder_co2.csv")


