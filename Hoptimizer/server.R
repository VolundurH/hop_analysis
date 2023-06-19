library(shiny)
library(tidyverse)
library(countrycode)
library(gt)


function(input, output, session) {
  # input data
  hop_brew_values <- read_tsv("../hop_brew_values.txt")
  hop_aromas <- read_tsv("../hop_aromas.txt") %>%
    mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c'))
  
  # get summary table of all hops
  output$all_hops <- render_gt({
    hop_aromas %>% 
      count(country) %>%
      arrange(desc(n)) %>%
      mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c')) %>%
      gt() %>%
      cols_move_to_start(country_code) %>% 
      fmt_flag(columns = country_code) %>% 
      cols_label(country_code = '',
                 country = md('**Country**'),
                 n = md('**Hop strains**'))
  })
  
  # filter data down to the countries you want
  hop_aromas_country <- reactive({
    data <- hop_aromas %>%
          filter(country %in% input$inputCountry)
    return(data)
    })
  
  # create the table
  output$hop_table_country <- render_gt({
    req(input$inputCountry)
    hop_aromas_country() %>% 
      mutate(hop_name = paste0("<a href = ", link, ">", hop_name, "</a>"),
             hop_name = map(hop_name, gt::html)) %>%
      select(hop_name, hop_purpose, country_code, country) %>% 
      gt() %>% 
      fmt_flag(columns = country_code) %>%
      cols_label(hop_name = md('**Hop name**'),
                 hop_purpose = md('**Purpose**'),
                 country_code = '',
                 country = md('**Country**'))
  })
  
  # filter data down to the options you want
  hop_aromas_profiles <- reactive({
    data <- hop_aromas
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
    # alternatively can use this: 
    # data %>% 
    #  filter(if_all(
    #   .cols = c("Citrus", "Berry"),
    #   ~ between(
    #     ., 
    #     paste0("input[[\"slider", ., "\"]][1]") %>%
    #       parse(text = .) %>%
    #       eval(), 
    #     paste0("input[[\"slider", ., "\"]][2]") %>%
    #       parse(text = .) %>%
    #       eval())
    # ))
    return(data)
  })
  
  output$profile_table <- render_gt({
    hop_aromas_profiles() %>%
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
