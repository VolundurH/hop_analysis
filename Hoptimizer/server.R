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
  
  #### First tab code # Countries
  
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
  

  # plot
  output$hop_plot_countries <- renderPlot({
    validate(need(input$inputCountry, 'Choose countries on the left to start.'))
    req(input$inputCountry)
    hop_aromas_country() %>% 
      filter(country %in% input$inputCountry) %>% 
      ggplot(aes(y = country %>% fct_rev(), fill = hop_purpose)) +
      geom_bar(position = 'fill') +
      labs(x = "Purpose proportion", y = NULL) +
      theme_minimal() +
      theme(axis.text.y = element_text(face = 'bold'),
            legend.position = 'bottom') +
      scale_fill_manual(name = NULL,
                        values = c('Aroma' = 'yellow',
                                   'Bittering' = 'brown',
                                   'Dual' = 'orange')) +
      scale_x_continuous(labels = scales::percent)
  })
  
  #### Second tab code # Profiles

  # filter based on profiles
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
    return(data)
  })

  table_check_min <- reactive({
    if (nrow(hop_aromas_profiles()) == 0) {
      as_tibble(paste(emo::ji('prohibited'), 'No hops found, relax filters.', emo::ji('prohibited'))) %>% 
        gt() %>% 
        cols_label(value = '') }
  })  
  
  output$hop_table_check <- render_gt({
    table_check_min()
    })
    
  output$hop_table_check2 <- render_gt({
    table_check_min()
    })
  
  output$hop_table_check3 <- render_gt({
    table_check_min()
  })
  
  output$hop_table_profiles <- render_gt({
    if (nrow(hop_aromas_profiles()) > 0) {
      hop_aromas_profiles() %>%
        select(country_code, hop_name, hop_purpose,
               Citrus, TropicalFruit, StoneFruit, Berry, Floral,
               Grassy, Herbal, Spice, Pine) %>%
        gt() %>%
        fmt_flag(columns = country_code) %>%
        cols_label(hop_name = md('**Hop name**'),
                   hop_purpose = md('**Purpose**'),
                   country_code = '') %>%
        tab_spanner(label = md("**Profile**"),
                    columns = c(Citrus, TropicalFruit, StoneFruit, Berry, Floral,
                                Grassy, Herbal, Spice, Pine))
    }
  })
  
  output$profile_overview <- renderPlot({
    hop_aromas_profiles() %>%
      select(hop_name, Citrus, TropicalFruit, StoneFruit, Berry, Floral,
             Grassy, Herbal, Spice, Pine) %>% 
      pivot_longer(cols = -hop_name, names_to = 'Aroma', values_to = 'Score') %>% 
      mutate(Score = as.factor(Score)) %>% 
      ggplot(aes(y = Aroma, fill=Score)) +
      geom_bar() +
      theme_minimal() +
      theme(axis.text.y = element_text(face = 'bold')) +
      scale_fill_viridis_d(direction = -1) +
      labs(x = 'Number of hops')
  })
  
  table_check_max <- reactive({
    if (nrow(hop_aromas_profiles()) > 20) {
      tibble(a = emo::ji('exploding_head'), 
             b = 'Too many hops selected!', 
             c = emo::ji('exploding_head')) %>% 
        gt() %>% 
        tab_options(column_labels.hidden = TRUE)
    }
  }) 
  
  output$hop_table_check_max <- render_gt({
    table_check_max()
  })
  
  # Render hop aroma radial plot
  
  output$radial_plots <- renderPlot({
    if (between(nrow(hop_aromas_profiles()), 1, 20)) {
      hop_aromas_profiles() |> 
        pivot_longer(
          cols = -c(hop_name, hop_purpose, country, link, aroma_tags, country_code),
          names_to = "aroma", 
          values_to = "aroma_value"
        ) |> 
        mutate(aroma = str_replace(aroma, "(?<=[:lower:])(?=[:upper:])", "\n"))|>
        ggplot(aes(x = fct_inorder(aroma), y = aroma_value, group = hop_name)) +
        geom_col(aes(fill = aroma), show.legend = F) +
        geom_segment(aes(xend = fct_inorder(aroma), y = 5, yend  = 0), linetype = 2, alpha = 0.5) +
        coord_polar() +
        theme_minimal() + 
        theme(legend.position = "none",
              axis.text.y = element_blank(),
              # panel.border = element_rect(fill = NA),
              plot.margin = margin(0,0,0,0,"mm"),
              plot.background = element_rect(fill = NA, colour = NA), 
              panel.background =  element_rect(fill = NA, colour = NA), 
              panel.border =  element_rect(fill = NA, colour = NA),
              axis.line.x = element_blank(),
              strip.text = element_text(face = 'bold')) +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(limits = c(0,5)) +
        scale_fill_viridis_d() +
        facet_wrap(~hop_name, ncol = 4)
    }
  }, # Need a good height/width ratio for the radial plots. Try with each row = 200px to start, and width always 700px. 
    width = 700,
    height = reactive({(as.integer(nrow(hop_aromas_profiles()) / 4.01) + 1 ) * 200})
    )
  
  
  #### Third tab code

 
  
  # Reactive for hop oils 
  hop_brew_values_overview_plot <- reactive({
    data <- hop_oil_overview |> 
      mutate(range_mean = range_mean/100) |> 
      ggplot(aes(x = fct_inorder(brew_value), y = range_mean, group = hop_name)) +
      geom_path(alpha = 0.5, aes(col = total_oil)) +
      geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Total oil breakdown", y = NULL, col = "Total Oils\n(mL/100g)")  + 
      theme_classic()  +
      scale_color_gradient(low = "white", high = "forestgreen", )
    
    return(data)
  })
  
  # Render hop oil overview
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
