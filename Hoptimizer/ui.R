library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hoptimizer - Aromas"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose a country -----
      selectInput("inputCountry",
                  label="Select a country:",
                  choices = c(unique(hop_aromas$country)),
                  multiple = TRUE),
      
      # Input: Choose purpose ----
      selectInput("inputPurpose",
                  label="Select a purpose:",
                  choices = c("Any", unique(hop_aromas$hop_purpose)),
                  selected = 'Any'),
    
      # Input: Aroma profile sliders -----
      sliderInput("sliderCitrus",
                  label="Select a range for the Citrus profile:",
                  min=0, max=5, value=c(0,5), step = 1),
      sliderInput("sliderTropicalfruit",
                  label="Select a range for the Tropical Fruit profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderStonefruit",
                  label="Select a range for the Stone Fruit profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderBerry",
                  label="Select a range for the Berry profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderFloral",
                  label="Select a range for the Floral profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderGrassy",
                  label="Select a range for the Grassy profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderHerbal",
                  label="Select a range for the Herbal profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderSpice",
                  label="Select a range for the Spice profile:",
                  value=c(0,5), min=0, max=5, step = 1),
      sliderInput("sliderPine",
                  label="Select a range for the Pine profile:",
                  value=c(0,5), min=0, max=5, step = 1),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      gt_output('hop_table'),
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)