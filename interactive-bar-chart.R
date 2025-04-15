library(WDI)
library(tidyverse)
library(dplyr)
library(countrycode)
library(shiny)
library(plotly)
library(shinythemes)



population_data <- WDI(indicator = "SP.POP.TOTL", start = 2021, end = 2021)



valid_countries <- population_data %>%
  filter(!country %in% c(
    "Africa Eastern and Southern", "Africa Western and Central", "Arab World", 
    "Caribbean small states", "Central Europe and the Baltics", "Channel Islands", 
    "Early-demographic dividend", "East Asia & Pacific", "East Asia & Pacific (excluding high income)", 
    "East Asia & Pacific (IDA & IBRD countries)", "Euro area", "Europe & Central Asia", "Europe & Central Asia (IDA & IBRD countries)", 
    "Europe & Central Asia (excluding high income)", "European Union", "Fragile and conflict affected situations", 
    "Heavily indebted poor countries (HIPC)", "High income", "IBRD only", "IDA & IBRD total", 
    "IDA blend", "IDA only", "IDA total", "Kosovo", "Late-demographic dividend", 
    "Latin America & Caribbean", "Latin America & Caribbean (excluding high income)", 
    "Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification", 
    "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa", 
    "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)", 
    "Middle income", "North America", "Not classified", "OECD members", "Other small states", 
    "Pacific island small states", "Post-demographic dividend", "Pre-demographic dividend", 
    "Small states", "South Asia", "South Asia (IDA & IBRD)", "Sub-Saharan Africa", 
    "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)", 
    "Upper middle income", "World"
  ))

valid_countries <- valid_countries %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  rename(Country = country, Population = SP.POP.TOTL) %>%
  filter(!is.na(Population))

valid_countries <- valid_countries %>%
  mutate(continent = countrycode(Country, "country.name", "continent"))

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  
  titlePanel("Population of Countries by Continent"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select continent or 'World':", 
                  choices = c("World", unique(valid_countries$continent)), 
                  selected = "World"),
      radioButtons("display_option", "Choose countries to display:", 
                   choices = c("Top" = "top", "Lowest" = "lowest"), 
                   selected = "top"),
      numericInput("top_n", "Select number of countries to display:", min = 1, max = nrow(valid_countries), value = 5)
    ),
    mainPanel(
      plotlyOutput("bar_chart")
    )
  )
)


# Server
ui <- fluidPage(
  titlePanel("Population of Countries by Continent"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select continent or 'World':", 
                  choices = c("World", unique(valid_countries$continent)), 
                  selected = "World"),  
      radioButtons("display_option", "Choose countries to display:", 
                   choices = c("Top" = "top", "Lowest" = "lowest"), 
                   selected = "top"),
      numericInput("top_n", "Select number of countries to display:", min = 1, max = nrow(valid_countries), value = 5),
      textInput("country_search", "Search for a country:", "")
    ),
    mainPanel(
      plotlyOutput("bar_chart")
    )
  )
)

# Server
server <- function(input, output) {
  output$bar_chart <- renderPlotly({
    continent_input <- input$continent
    top_n <- input$top_n
    display_option <- input$display_option
    country_search <- input$country_search
    
    # If the user selects "World", don't filter by continent
    if (continent_input == "World") {
      continent_data <- valid_countries  # Use data for all countries
    } else {
      # Otherwise, filter by the selected continent
      continent_data <- valid_countries %>%
        filter(continent == continent_input)
    }
    
    # Filter by country search if provided
    if (country_search != "") {
      continent_data <- continent_data %>%
        filter(str_detect(Country, regex(country_search, ignore_case = TRUE)))
    }
    
    if (display_option == "top") {
      selected_countries <- continent_data %>%
        arrange(desc(Population)) %>%
        slice_head(n = top_n)  # only the top N rows
    } else {
      selected_countries <- continent_data %>%
        arrange(Population) %>%
        slice_head(n = top_n)  # only the bottom N rows
    }
    
    # Dynamic title
    title_text <- ifelse(country_search != "", 
                         paste("Population of", country_search), 
                         paste("Population of", top_n, ifelse(display_option == "top", "Most", "Least"), "Populated Countries in", continent_input))
    
    plot_ly(selected_countries, 
            x = ~Population, 
            y = ~fct_reorder(Country, Population), 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = "#d0849d"),
            text = ~paste("Population: ", format(Population, big.mark = ",")), 
            hoverinfo = 'text') %>%
      layout(title = title_text,
             xaxis = list(title = "Population"),
             yaxis = list(title = "", tickvals = selected_countries$Country)) %>%
      config(displayModeBar = FALSE)
  })
}




# Run the app
shinyApp(ui, server)
