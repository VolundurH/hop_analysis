library(shiny)
library(tidyverse)
library(countrycode)
library(gt)

hop_brew_values <- read_tsv("../hop_brew_values.txt")
hop_aromas <- read_tsv("../hop_aromas.txt") %>%
  mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c'))

function(input, output, session) {

  hop_aromas_filtered <- reactive({
    data <- hop_aromas
    data <- data %>%
      filter(country %in% input$inputCountry)
    if (input$inputPurpose != 'Any') {
      data <- data %>% 
        filter(hop_purpose %in% input$inputPurpose)
    }
    data <- data %>%
      filter(between(Citrus, input$sliderCitrus[1], input$sliderCitrus[2]),
             between(TropicalFruit, input$sliderTropicalfruit[1], input$sliderTropicalfruit[2]),
             between(StoneFruit, input$sliderStonefruit[1], input$sliderStonefruit[2]),
             between(Berry, input$sliderBerry[1], input$sliderBerry[2]),
             between(Floral, input$sliderFloral[1], input$sliderFloral[2]),
             between(Grassy, input$sliderGrassy[1], input$sliderGrassy[2]),
             between(Herbal, input$sliderHerbal[1], input$sliderHerbal[2]),
             between(Spice, input$sliderSpice[1], input$sliderSpice[2]),
             between(Pine, input$sliderPine[1], input$sliderPine[2]))
    data
    })
  
  output$hop_table <- render_gt({
    hop_aromas_filtered() %>% 
      select(hop_name, hop_purpose, country_code, country, link) %>% 
      mutate(link = map(link, ~ htmltools::a(href = .x, link)),
             link = map(link, ~ gt::html(as.character(.x)))) %>% 
      gt() %>% 
      fmt_flag(columns = country_code) %>%
      cols_label(hop_name = md('**Hop name**'),
                 hop_purpose = md('**Purpose**'),
                 country_code = '',
                 country = md('**Country**'),
                 link = md('**Original link**'))
  })
  
  output$profile_table <- render_gt({
    hop_aromas_filtered() %>% 
      select(hop_name, Citrus, TropicalFruit, StoneFruit, Berry, 
             Floral, Grassy, Herbal, Spice, Pine) %>% 
      gt() %>%
      tab_spanner(label = md('**Profiles**'),
                  columns = -c(hop_name)) %>% 
      cols_label(hop_name = md('**Hop name**'))
  })
  
  
  # output$distPlot <- renderPlot({
  #       # generate bins based on input$bins from ui.R
  #       x    <- faithful[, 2]
  #       bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #       # draw the histogram with the specified number of bins
  #       hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #            xlab = 'Waiting time to next eruption (in mins)',
  #            main = 'Histogram of waiting times')
  # 
  #   })

}
