# load neccesary packages
library(tidyverse) #used for elementary data wrangling and visualization
library(countrycode) #used to assign countries their respective regions and iso3 codes
library(lubridate) #used to deal with dates and times
library(wbstats) #used to add total population and gross domestic product to countries
library(plotly) #used for dynamic visualizations
library(naniar) #used for missing values visualization
library(corrplot) #used for correlation matrix

# deactive scientific notation and round to two decimals
options(scipen = 100, digits=2)

# setting for table view
knitr::opts_chunk$set(echo = TRUE)

# read in raw datasets
social_media <- read_csv("GWI_socialmedia_2019.csv")
mental_health <- read_csv("IMHE_mentaldisorders_2019.csv")

# read and wrangle mental health data
mental_health <- mental_health %>%
  select(location_name, year, val) %>%
  rename(country_name = location_name, population_with_mental_disorders = val) %>%
  mutate(country_code = countrycode(sourcevar = country_name, origin = "country.name", destination = "iso3c"))

# read and wrangle social media data
social_media <- social_media %>%
  mutate(country_code = countrycode(sourcevar = country_name, origin = "country.name", destination = "iso3c")) %>%
  mutate(social_media_use = hms(social_media_use)) %>% # split hours, minutes and seconds
  mutate(social_media_use = hour(social_media_use)*60 + minute(social_media_use)) %>% #to get total minutes spent on social media
  rename(minutes_on_social_media = social_media_use) %>%
  select(country_code, year, minutes_on_social_media) # dropping country_name

# merge social media and mental health dataframes
mental_health_social_media <- merge(mental_health, social_media, 
                                    by=c("country_code", "year"))

# add country region
mental_health_social_media <- mental_health_social_media %>%
  mutate(country_region = countrycode(sourcevar = country_name, origin = "country.name", destination = "region"))

# read in socioeconomic data
socioeconomic_indicators <- wb_data(
  indicator = c("SP.POP.TOTL","NY.GDP.MKTP.CD") #World Bank codes for GDP and total population
  , country = c("countries_only") , start_date = 2012
  , end_date = 2019
)

# wrangle socioeconomic data
socioeconomic_indicators <- socioeconomic_indicators %>%
  rename(country_code = iso3c, year = date, total_population = SP.POP.TOTL, gdp = NY.GDP.MKTP.CD) %>%
  select(country_code, year, total_population, gdp) #dropping country name and two-digit code

# merge socioeconomic data with social media and mental health dataframe
mental_health_social_media <- merge(
  mental_health_social_media
  , socioeconomic_indicators
  , by=c("country_code", "year")
) %>%
  relocate(
    country_name
    , country_code
    , country_region
    , year
    , population_with_mental_disorders
    , minutes_on_social_media
    , gdp
    , total_population
  )

# code to generate snippet of resulting dataframe
rmarkdown::paged_table(mental_health_social_media[sample(nrow(mental_health_social_media), 10), ])

# export resulting dataframe to csv
write.csv(mental_health_social_media, "socialmedia_mentalhealth_bycountry.csv")

# plotting missing values 
plot_missing_values <- gg_miss_fct(x = mental_health_social_media, fct = year)

plot_missing_values

# plotting global averages over the years
lineplot_mental_health_social_media <- mental_health_social_media %>%
  select(year, population_with_mental_disorders, minutes_on_social_media) %>% # selecting variables of interest
  mutate(population_with_mental_disorders = population_with_mental_disorders*100) %>%
  group_by(year) %>% # step needed to calculate global means
  summarise_at(vars(population_with_mental_disorders:minutes_on_social_media), mean, na.rm = TRUE) %>%
  plot_ly(width = 500) %>%
  add_trace(
    x = ~year
    , y = ~population_with_mental_disorders
    , name = "population_with_mental_disorders"
    , type = "scatter"
    , mode = "lines+markers"
  ) %>%
  add_trace(
    x = ~year
    , y = ~minutes_on_social_media
    , name = "minutes_on_social_media"
    , type = "scatter"
    , mode = "lines+markers"
    , yaxis = "y2"
  ) %>%
  layout(
    title = list(text = "<b>Global Averages Over The Years</b>", x = 0)
    , xaxis = list(title = "Year")
    , yaxis = list(
      tickfont = list(color = "blue")
      , title = "Population with Mental Disorders (Yearly %)"
    )
    , yaxis2 = list(
      tickfont = list(color = "orange")
      , title = "Minutes on Social Media (Daily)"
      , overlaying = "y"
      , side = "right"
      , automargin = TRUE
    )
    , legend = list(
      orientation = "h"
      #, xanchor = "center"
      #, yanchor = "top"
      , x = 0
      , y = 1
    )
  )

lineplot_mental_health_social_media

# plotting global averages over the years
choroplethmap_mental_health_social_media <- mental_health_social_media %>%
  plot_ly(
    type = 'choropleth',
    locations = ~country_code,
    frame = ~year,
    width = 750
  ) %>%
  add_trace(
    z = ~population_with_mental_disorders,
    colorscale = "YlOrRd",
    showscale = FALSE,
    reversescale = TRUE,
    text = "Mental Disorder Prevalence",
    visible = FALSE  # Set the initial visibility of the trace to FALSE
  ) %>%
  add_trace(
    z = ~minutes_on_social_media,
    colorscale = "Blues",
    showscale = FALSE,
    reversescale = TRUE,
    text = "Minutes on Social Media",
    visible = TRUE  # Set the initial visibility of the trace to TRUE
  ) %>%
  layout(
    title = list(text = "<b>Country Averages Over The Years</b>", x = 0),
    autosize = TRUE,
    updatemenus = list(
      list(
        active = 0,
        type = "buttons",
        direction = "right",
        xanchor = "center",
        yanchor = "bottom",
        x = .5,
        y = -.1,
        buttons = list(
          list(
            label = "Click to view minutes on social media (blue)",
            method = "update",
            args = list(list(visible = list(TRUE, FALSE)))
          ),
          list(
            label = "Click to view mental disorder prevalence (red)",
            method = "update",
            args = list(list(visible = list(FALSE, TRUE)))
          )
        )
      )
    )
  )

choroplethmap_mental_health_social_media

# plotting social media against mental health
scatterplot_mental_health_social_media <- mental_health_social_media %>%
  mutate(population_with_mental_disorders = population_with_mental_disorders*100) %>%
  plot_ly(
    x = ~minutes_on_social_media
    , y = ~population_with_mental_disorders 
    , size = ~gdp
    , color = ~country_region
    , frame = ~year
    , text = ~country_name
    , hoverinfo = "text"
    , type = 'scatter'
    , mode = 'markers'
    , width = 750
  ) %>%
  layout(
    title = list(text = "<b>Mental Disorder Prevalence x Social Media Use</b>", x = 0)
    , xaxis = list(title = "Minutes Spent on Social Media (Daily)")
    , yaxis = list(title = "Population with Mental Disorders (Yearly %)")
  )

scatterplot_mental_health_social_media

# plotting correlation matrix
cor(
  select(mental_health_social_media, -starts_with(c("country", "year")))
  , use = "na.or.complete"
) %>%
  corrplot(
    type = "lower" #makes correlations appear at the bottom
    , method = "square" #alternative: change to square or number
    , order = "FPC" #shows most relevant components first
    , tl.col = "black" #legend text color
    , tl.pos = "l" #includes only labels on the left
    , addCoef.col = 'grey50' #includes grey correlation coefficients
    , cl.pos = "n" #removes color legend/gradient
  )
