library(shiny)
library(tidyverse)
library(countrycode)
library(gt)

function(input, output, session) {
  # input data
  hop_aromas <- read_tsv("../hop_aromas.txt") %>%
    mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c'))
  hop_brew_values <- read_tsv("../hop_brew_values.txt")
  hop_oil_overview  <- hop_brew_values |> 
    filter(brew_value %in% c("All Others", "Myrcene", "Humulene", "Farnesene", "Caryophyllene")) |> 
    left_join(
      hop_brew_values |> 
        filter(brew_value == "Total Oils (mL/100g)") |> 
        select(hop_name, brew_value, range_mean) |> 
        pivot_wider(names_from = brew_value, values_from = range_mean )
    ) |> 
    rename(total_oil = `Total Oils (mL/100g)`) |> 
    mutate(brew_value = fct_relevel(brew_value, "Farnesene", "Caryophyllene","Myrcene", "Humulene", "All Others"))
  
  #### First tab code
  
  # summary table
  output$summaryCountry <- render_gt({
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
  
  # filter based on country
  hop_aromas_country <- reactive({
    data <- hop_aromas
    data <- data %>%
      filter(country %in% input$inputCountry)
    return(data)
  })
  
  # plot table
  output$hop_table_countries <- render_gt({
    req(input$inputCountry)
    hop_aromas_country() %>% 
      mutate(hop_name = paste0("<a href=", link, ">", hop_name,"</a>")) %>% 
      select(hop_name, hop_purpose, country_code, country) %>% 
      gt() %>% 
      cols_move_to_end(columns = "hop_purpose") %>% 
      fmt_flag(columns = country_code) %>%
      fmt_url(columns = "hop_name") %>% 
      cols_label(hop_name = md('**Hop name**'),
                 country_code = '',
                 country = md('**Country**'),
                 hop_purpose = md('**Purpose**'))
  })
  
  #### Second tab code
  
  
  #### Third tab code
  
  # Reactive for hop oils 
  hop_brew_values_overview_plot <- reactive({
    data <- hop_oil_overview |> 
      mutate(range_mean = range_mean/100) |> 
      ggplot(aes(x = fct_inorder(brew_value), y = range_mean, group = hop_name)) +
      geom_point() +
      geom_path(alpha = 0.5, aes(col = total_oil)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Total oil breakdown", y = NULL, col = "Total Oils\n(mL/100g)")  + 
      theme_classic() 
    
    return(data)
  })
  
  # Plot hop oil
  
  output$total_oil_overview <- renderPlot({
    hop_brew_values_overview_plot()
  })
  
  # output$txtout <- renderText({
  #   paste(input$txt, input$slider, format(input$date), sep = ", ")
  # })
  # output$table <- renderTable({
  #   head(cars, 4)
  # })
}








# function(input, output, session) {
# 
#   # input data
#   hop_brew_values <- read_tsv("../hop_brew_values.txt")
#   hop_aromas <- read_tsv("../hop_aromas.txt") %>%
#     mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c'))
#   
#   print(head(hop_aromas))
#   
#   # summary table
#   output$summaryCountry <- renderTable({
#     hop_aromas %>% 
#       count(country) %>% 
#       arrange(desc(n)) %>%
#       mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c')) %>% 
#       gt() %>%
#       cols_move_to_start(country_code) %>% 
#       fmt_flag(columns = country_code) %>% 
#       cols_label(country_code = '',
#                  country = md('**Country**'),
#                  n = md('**Hop strains**'))
#       })
#   
#   print(head(output$summaryCountry))
#   
#   # filter based on country
#   hop_aromas_country <- reactive({
#     data <- hop_aromas
#     data <- data %>%
#       filter(country %in% input$inputCountry)
#     return(data)
#     })
#   
#   # plot table
#   t1 <- hop_aromas_country() %>% 
#     select(hop_name, hop_purpose, country_code, country, link) %>% 
#     mutate(hop_name = paste0("<a href=", link, "></a>")) %>% 
#     gt() %>% 
#     fmt_flag(columns = country_code) %>%
#     cols_label(hop_name = md('**Hop name**'),
#                country_code = '',
#                country = md('**Country**'))
#   
#   output$hop_table_profiles <- render_gt({
#     req(input$inputCountry)
#     t1
#     })
#   
#   
#   hop_aromas_profiles <- reactive({
#     data <- hop_aromas
#     if (input$inputPurpose != 'Any') {
#       data <- data %>% 
#         filter(hop_purpose %in% input$inputPurpose)
#     }
#     data <- data %>%
#       filter(between(Citrus, input$sliderCitrus[1], input$sliderCitrus[2]),
#              between(TropicalFruit, input$sliderTropicalfruit[1], input$sliderTropicalfruit[2]),
#              between(StoneFruit, input$sliderStonefruit[1], input$sliderStonefruit[2]),
#              between(Berry, input$sliderBerry[1], input$sliderBerry[2]),
#              between(Floral, input$sliderFloral[1], input$sliderFloral[2]),
#              between(Grassy, input$sliderGrassy[1], input$sliderGrassy[2]),
#              between(Herbal, input$sliderHerbal[1], input$sliderHerbal[2]),
#              between(Spice, input$sliderSpice[1], input$sliderSpice[2]),
#              between(Pine, input$sliderPine[1], input$sliderPine[2]))
#     return(data) 
#     })
#   
#   t2 <- hop_aromas_profiles() %>% 
#     select(country_code, hop_name, hop_purpose, 
#            Citrus, TropicalFruit, StoneFruit, Berry, Floral,
#            Grassy, Herbal, Spice, Pine) %>% 
#     gt() %>% 
#     fmt_flag(columns = country_code) %>%
#     cols_label(hop_name = md('**Hop name**'),
#                hop_purpose = md('**Purpose**'),
#                country_code = '') %>% 
#     tab_spanner(label = md("**Profile**"),
#                 columns = c(Citrus, TropicalFruit, StoneFruit, Berry, Floral,
#                             Grassy, Herbal, Spice, Pine))
#   
#   output$hop_table_profiles <- render_gt({t2})
#   
#   # output$distPlot <- renderPlot({
#   #       # generate bins based on input$bins from ui.R
#   #       x    <- faithful[, 2]
#   #       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#   # 
#   #       # draw the histogram with the specified number of bins
#   #       hist(x, breaks = bins, col = 'darkgray', border = 'white',
#   #            xlab = 'Waiting time to next eruption (in mins)',
#   #            main = 'Histogram of waiting times')
#   # 
#   #   })
# 
# }
