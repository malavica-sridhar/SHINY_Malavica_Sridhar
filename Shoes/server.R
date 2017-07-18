library("shinydashboard")
library("ggplot2")
library("gridExtra")
library("cowplot")

shinyServer(function(input, output){
  
# Overall Distribution Plots
  
  output$overall_plot <- renderPlot({
    
    grid.arrange(NL, EL, L, ncol = 3)
    
  })
  
# Sex Tier Breakdown Plots
  
  output$women_tier_graph <- renderPlot({
    
    Non_Luxury_Shoes <- ggplot(subset(summary_prices, Tier == "Non-Luxury"), aes(x = Average_by_Brand)) + 
      geom_density(aes(fill = Tier), binwidth = 10, alpha = 0.4) + xlab("Average Price (in Dollars)") + ylab("Count") + 
      theme(legend.position="none") + ggtitle("Non-Luxury Shoes") + theme(text = element_text(size=20)) + scale_fill_manual(values = c("lightblue"))
    
    Everyday_Shoes <- ggplot(subset(summary_prices, Tier == "Everyday Luxury"), aes(x = Average_by_Brand)) + 
      geom_density(aes(fill = Tier), binwidth = 5, alpha = 0.4) + xlab("Average Price (in Dollars)") + ylab("Count") + 
      theme(legend.position="none") + ggtitle("Everyday Luxury Shoes") + theme(text = element_text(size=20)) + scale_fill_manual(values = c("lightblue"))
    
    Luxury_Shoes <- ggplot(subset(summary_prices, Tier == "Luxury"), aes(x = Average_by_Brand)) + 
      geom_density(aes(fill = Tier), binwidth = 100, alpha = 0.4) + xlab("Average Price (in Dollars)") + ylab("Count") + 
      theme(legend.position="none") + ggtitle("Luxury Shoes") + xlim(c(100,4500)) + theme(text = element_text(size=20)) + scale_fill_manual(values = c("lightblue"))
    
    grid.arrange(Non_Luxury_Shoes, Everyday_Shoes, Luxury_Shoes, ncol = 3)
    
  })
  
  output$men_tier_graph <- renderPlot({
    
    Non_Luxury_Shoes_Men <- ggplot(subset(summary_prices_men, Tier == "Non-Luxury"), aes(x = Average_by_Brand)) + 
      geom_density(aes(fill = Tier), binwidth = 10, alpha = 0.4) + xlab("Average Price (in Dollars)") + ylab("Count") + 
      theme(legend.position="none") + ggtitle("Non-Luxury Shoes") + theme(text = element_text(size=20)) + scale_fill_manual(values = c("lightblue"))
    
    Everyday_Shoes_Men <- ggplot(subset(summary_prices_men, Tier == "Everyday Luxury"), aes(x = Average_by_Brand)) + 
      geom_density(aes(fill = Tier), binwidth = 5, alpha = 0.4) + xlab("Average Price (in Dollars)") + ylab("Count") + 
      theme(legend.position="none") + ggtitle("Everyday Luxury Shoes") + theme(text = element_text(size=20)) + scale_fill_manual(values = c("lightblue"))
    
    Luxury_Shoes_Men <- ggplot(subset(summary_prices_men, Tier == "Luxury"), aes(x = Average_by_Brand)) + 
      geom_density(aes(fill = Tier), binwidth = 100, alpha = 0.4) + xlab("Average Price (in Dollars)") + ylab("Count") + 
      theme(legend.position="none") + ggtitle("Luxury Shoes") + xlim(c(100,1500)) + theme(text = element_text(size=20)) + scale_fill_manual(values = c("lightblue"))
    
    grid.arrange(Non_Luxury_Shoes_Men, Everyday_Shoes_Men, Luxury_Shoes_Men, ncol = 3)
    
  })
  
  output$women_pricing_plot <- renderPlot({
    
    if(input$market_type == "Non-Luxury Shoes") {
      
      grid.arrange(nl1_graph, nl2_graph, nl3_graph, 
                   nl4_graph, nl5_graph, nl6_graph, 
                   nl7_graph, nl8_graph, nl9_graph)
      
    } else if(input$market_type == "Everyday Luxury Shoes") {
      
      grid.arrange(el1_graph, el2_graph, el3_graph, 
                   el4_graph, el5_graph, el6_graph, 
                   el7_graph, el8_graph, el9_graph)
      
    } else if(input$market_type == "Luxury Shoes") {
      
      grid.arrange(l1_graph, l2_graph, l3_graph, 
                   l4_graph, l5_graph, l6_graph, 
                   l7_graph, l8_graph, l9_graph)
    }
    
  })
  
  output$men_pricing_plot <- renderPlot({
    
    if(input$market_type_men == "Everyday Luxury Shoes") {
      
      grid.arrange(mel1_graph, mel2_graph, mel3_graph, 
                   mel4_graph, mel5_graph, mel6_graph,
                   mel7_graph, mel8_graph, mel9_graph)
      
    } else if(input$market_type_men == "Luxury Shoes") {
      
      grid.arrange(ml1_graph, ml2_graph, ml3_graph, 
                   ml4_graph, ml5_graph, ml6_graph,
                   ml7_graph, ml8_graph, ml9_graph)
      
    } 
    
  })
  
  output$pattern_plot <- renderPlot({
    
    if(input$pattern_toggle == "Pattern") {
      
      ggplot(data = top_patterns, aes(x = reorder(Pattern, prices.amountMax), y = prices.amountMax)) + 
        geom_boxplot() + ylim(c(0,200)) + xlab("Pattern") + ylab("Price") + theme(text = element_text(size=20))
      
    } else if(input$pattern_toggle == "Pattern & Color") {
      
      ggplot(data = combined_color_material, aes(colors, Material)) + geom_raster(aes(fill = prices.amountMax)) + 
        scale_fill_gradient(low = "lightblue", high = "purple") + theme(text = element_text(size=20)) + labs(fill = "Average Price") + 
        ylab("Material") + xlab("Color")
      
    } 
      
  })
  
  output$material_plot <- renderPlot({
    
    if(input$market_type_material == "Women") {
      
      ggplot(data = women_top_material, aes(x = reorder(Material, prices.amountMax), y = prices.amountMax)) +
        geom_boxplot() + xlab("Material") + ylab("Price") + ylim(c(0,500)) + theme(text = element_text(size=20))
      
    } else if(input$market_type_material == "Men") {
      
      ggplot(data = men_top_material, aes(x = reorder(Material, prices.amountMax), y = prices.amountMax)) + 
        geom_boxplot() + xlab("Material") + ylab("Price") + ylim(c(0,500)) + theme(text = element_text(size=20))
      
    } 
    
  })
  
  output$color_plot <- renderPlot({
    
    if(input$color_type_material == "Women") {
      
      ggplot(data = women_top_color, aes(x = reorder(colors, prices.amountMax), y = prices.amountMax)) + 
        geom_boxplot() + xlab("Colors") + ylab("Price") + ylim(c(0,400)) + theme(text = element_text(size=20))
      
    } else if(input$color_type_material == "Men") {
      
      ggplot(data = men_top_color, aes(x = reorder(colors, prices.amountMax), y = prices.amountMax)) + 
        geom_boxplot() + xlab("Colors") + ylab("Price") + ylim(c(0,400)) + theme(text = element_text(size=20))
      
    } 
    
  })
  
  output$boot_plot <- renderPlot({
    
    ggplot(data = women_boot, aes(x = reorder(Boot, prices.amountMax), y = prices.amountMax)) + 
      geom_boxplot() + xlab("Boot Type") + ylab("Price") + ylim(c(0,250)) + theme(text = element_text(size=20))
    
  })

  
})





