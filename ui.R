library("shiny")
library("shinydashboard")
library("ggplot2")
library("cowplot")

shinyUI(
  
  # Dashboard Build
  
  dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Visualization of the Shoe Industry",
    titleWidth = 1985
  ),
  
  dashboardSidebar(
    sidebarUserPanel("KEY FACTS",
                     image = 'http://www.justwomenfashion.com/wp-content/uploads/2016/03/pumps-womens-shoes-silver.jpg'), 
    
    sidebarMenu(
          menuItem("Overall Insights", tabName = "overall", icon = icon("camera"),
            menuSubItem("Overall Shoe Distribution", tabName = "overall_shoe"),
            menuSubItem("Women Tier Breakdown", tabName = "women_tier"),
            menuSubItem("Men Tier Breakdown", tabName = "men_tier")),
          menuItem("A Look at Pricing Strategies", tabName = "pricing", icon = icon("money"),
            menuSubItem("Women's Pricing Analysis", tabName = "women_pricing"),
            menuSubItem("Men's Pricing Analysis", tabName = "men_pricing")),
          menuItem("Drivers of Price", tabName = "drivers", icon = icon("arrow-up"),
            menuSubItem("Pattern", tabName = "pattern"), 
            menuSubItem("Material", tabName = "material"), 
            menuSubItem("Color", tabName = "color"), 
            menuSubItem("Boot Height", tabName = "boot_type")))
  ),
  
  dashboardBody(
  
    tabItems(
      tabItem(tabName = "overall_shoe", 
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("Overall Insights by Market Type"))),
              fluidRow(plotOutput("overall_plot", width = 1500, height = 750), align = "center", width = "1000" , height = "50")),
      
      tabItem(tabName = "women_tier", 
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("Women's Shoes: A Look at Tiers"))),
              fluidRow(plotOutput("women_tier_graph", width = 1500, height = 600), align = "center", width = "1000" , height = "50")),
        
      tabItem(tabName = "men_tier", 
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("Men's Shoes: A Look at Tiers"))),
              fluidRow(plotOutput("men_tier_graph", width = 1500, height = 600), align = "center", width = "1000" , height = "50")),
      
      tabItem(tabName = "women_pricing",
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("The Women's Market: A Look at Pricing Strategies"))),
              fluidRow(box(selectInput("market_type", h4("Select Market"), choices = c("Non-Luxury Shoes", "Everyday Luxury Shoes", "Luxury Shoes")))),
              fluidRow(plotOutput("women_pricing_plot", width = 1200, height = 600), align = "center", width = 1000 , height = 50)),
      
      tabItem(tabName = "men_pricing",
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("The Men's Market: A Look at Pricing Strategies"))),
              fluidRow(box(selectInput("market_type_men", h4("Select Market"), choices = c("Everyday Luxury Shoes", "Luxury Shoes")))),
              fluidRow(plotOutput("men_pricing_plot", width = 1200, height = 600), align = "center", width = 1000 , height = 50)),
      
              
      tabItem(tabName = "pattern",
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("A Combined Look: How Pattern Drives Price"))),
              fluidRow(box(selectInput("pattern_toggle", h4("Pattern Dimensions"), choices = c("Pattern", "Pattern & Color")))),
              fluidRow(plotOutput("pattern_plot", width = 1050, height = 600), align = "center", width = 1000 , height = 50)),
      
      tabItem(tabName = "material",
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("How Material Drives Price"))),
              fluidRow(box(selectInput("market_type_material", h4("Select Market"), choices = c("Women", "Men")))),
              fluidRow(plotOutput("material_plot", width = 1450, height = 600), align = "center", width = 1000 , height = 50)),
              
      tabItem(tabName = "color",
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("How Color Drives Price"))),
              fluidRow(box(selectInput("color_type_material", h4("Select Market"), choices = c("Women", "Men")))),
              fluidRow(plotOutput("color_plot", width = 1450, height = 600), align = "center", width = 1000 , height = 50)),
      
      tabItem(tabName = "boot_type",
              fluidRow(box(width = 50 , height = 50, align = "center", solidHeader = TRUE, background = "light-blue", 
                           h4("How Boot Height Drives Price"))),
              fluidRow(plotOutput("boot_plot", width = 1450, height = 600), align = "center", width = 1000 , height = 50))
      
  )
  
))
  
)