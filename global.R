library(shiny)
library(shinydashboard)
library(tidyverse)
library(googleVis)
library(maps)
library(countrycode)
library(plotly)
library(ggthemes)
library(gganimate)
library(ggalt) # dumbbell plots
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots
library(broom)

data <- read_csv("E:/TAI_LIEU_NAM_TU/Chuyende5/Cuoi ki/master.csv")

# glimpse(data) # will tidy up these variable names

# sum(is.na(data$`HDI for year`)) # remove, > 2/3 missing, not useable

# table(data$age, data$generation) # don't like this variable

data <- data %>%
  select(-c(`HDI for year`, `suicides/100k pop`)) %>%
  rename(gdp_for_year = `gdp_for_year ($)`,
         gdp_per_capita = `gdp_per_capita ($)`,
         country_year = `country-year`) %>%
  as.data.frame()


# 2) OTHER ISSUES

# a) this SHOULD give 12 rows for every county-year combination (6 age bands * 2 genders):

# test <- data %>%
#   group_by(country_year) %>%
#   count() %>%
#   filter(n != 12) # note: there appears to be an issue with 2016 data

# View(test)
# not only are there few countries with data, but those that do have data are incomplete

data <- data %>%
  filter(year != 2016) %>% # I therefore exclude 2016 data
  select(-country_year)


# b) excluding countries with <= 3 years of data:

minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(),
            years = rows / 12) %>%
  arrange(years)

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))


# no other major data issues found yet



# 3) TIDYING DATAFRAME
data$age <- gsub(" years", "", data$age)
data$sex <- ifelse(data$sex == "male", "Male", "Female")


# getting continent data:
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# Nominal factors
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})


# Making age ordinal
data$age <- factor(data$age,
                   ordered = T,
                   levels = c("5-14",
                              "15-24",
                              "25-34",
                              "35-54",
                              "55-74",
                              "75+"))

# Making generation ordinal
data$generation <- factor(data$generation,
                          ordered = T,
                          levels = c("G.I. Generation",
                                     "Silent",
                                     "Boomers",
                                     "Generation X",
                                     "Millenials",
                                     "Generation Z"))

data <- as_tibble(data)


# the global rate over the time period will be useful:

global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000

# view the finalized data
glimpse(data)

suicide_rates <- read_csv(file = "./suicide_rates.csv")
df  =
  suicide_rates %>%
  select(-`country-year`) %>% 
  rename(suicides.per.100k = `suicides/100k pop`,
         hdi = `HDI for year`,
         gdp.capita = `gdp_per_capita ($)`,
         gdp.year = `gdp_for_year ($)`,
         suicides = suicides_no)
  
df = df %>% 
  mutate(continent = countrycode(sourcevar = df$country,
                            origin = "country.name",
                            destination = "continent"),
         country = ifelse(country == "Republic of Korea", "South Korea", country))

df3 = df %>%
  filter(country %in% c("United States", "Canada", "Australia", "Mexico", "South Korea")) %>% 
  select(-hdi, -age, -continent, -gdp.year, -sex) %>% 
  select(-generation, -year)

## explore the years (wars, etc.)

