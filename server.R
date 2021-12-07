shinyServer(function(input, output){
  #### Map Tab #########################################################
  ## Filter
  df1 <- reactive({
    if(is.null(input$checkGroup)){
      df1 = df %>% 
        filter(between(year, input$slider[1], input$slider[2])) %>%
        group_by(country, age) %>%
        summarise(suicides100 = sum(suicides.per.100k),
                  suicides = sum(suicides))
    }
    else{
      df1 = df %>%
        filter(age %in% input$checkGroup) %>%
        filter(between(year, input$slider[1], input$slider[2])) %>% 
        group_by(country, age) %>%
        summarise(suicides100 = sum(suicides.per.100k),
                  suicides = sum(suicides))
    }
  })
  
  
  output$globaltrend  = renderPlot({
    data %>%
      group_by(year) %>%
      summarize(population = sum(population),
                suicides = sum(suicides_no),
                suicides_per_100k = (suicides / population) * 100000) %>%
      ggplot(aes(x = year, y = suicides_per_100k)) +
      geom_line(col = "deepskyblue3", size = 1) +
      geom_point(col = "deepskyblue3", size = 2) +
      geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
      labs(title = "Global Suicides (per 100k)",
           subtitle = "Trend over time, 1985 - 2015.",
           x = "Year",
           y = "Suicides per 100k") +
      scale_x_continuous(breaks = seq(1985, 2015, 2)) +
      scale_y_continuous(breaks = seq(10, 20))
  })
  
  output$bycontinent = renderPlot({
    continent <- data %>%
      group_by(continent) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      arrange(suicide_per_100k)
    
    continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)
    
    continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global Suicides (per 100k), by Continent",
           x = "Continent", 
           y = "Suicides per 100k", 
           fill = "Continent") +
      theme(legend.position = "none", title = element_text(size = 10)) + 
      scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)
    
    
    continent_time <- data %>%
      group_by(year, continent) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
    
    continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)
    
    continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_per_100k, col = factor(continent))) + 
      facet_grid(continent ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Continent", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Continent") + 
      theme(legend.position = "none", title = element_text(size = 10)) + 
      scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)
    
    grid.arrange(continent_plot, continent_time_plot, ncol = 2)
  })
  
  
  output$bysex = renderPlot({
    sex_plot <- data %>%
      group_by(sex) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides (per 100k), by Sex",
           x = "Sex", 
           y = "Suicides per 100k") +
      theme(legend.position = "none") + 
      scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)
    
    ### with time
    sex_time_plot <- data %>%
      group_by(year, sex) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
      facet_grid(sex ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Sex", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Sex") + 
      theme(legend.position = "none") + 
      scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)
    
    grid.arrange(sex_plot, sex_time_plot, ncol = 2)
  })
  
  
  output$byage= renderPlot({
    age_plot <- data %>%
      group_by(age) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides per 100k, by Age",
           x = "Age", 
           y = "Suicides per 100k") +
      theme(legend.position = "none") + 
      scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)
    
    ### with time
    age_time_plot <- data %>%
      group_by(year, age) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
      facet_grid(age ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Age", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Age") + 
      theme(legend.position = "none") + 
      scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)
    
    
    grid.arrange(age_plot, age_time_plot, ncol = 2)
  })
  
  output$bycountry=renderPlot({
    country <- data %>%
      group_by(country, continent) %>%
      summarize(n = n(), 
                suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      arrange(desc(suicide_per_100k))
    
    country$country <- factor(country$country, 
                              ordered = T, 
                              levels = rev(country$country))
    
    ggplot(country, aes(x = country, y = suicide_per_100k, fill = continent)) + 
      geom_bar(stat = "identity") + 
      geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
      labs(title = "Global suicides per 100k, by Country",
           x = "Country", 
           y = "Suicides per 100k", 
           fill = "Continent") +
      coord_flip() +
      scale_y_continuous(breaks = seq(0, 45, 2)) + 
      theme(legend.position = "bottom")
  })
  
  
  output$bygdp = renderPlot({
    country_year_gdp <- data %>%
      group_by(country, year) %>%
      summarize(gdp_per_capita = mean(gdp_per_capita))
    
    country_year_gdp_corr <- country_year_gdp %>%
      ungroup() %>%
      group_by(country) %>%
      summarize(year_gdp_correlation = cor(year, gdp_per_capita))
    
    
    
    ## **Do richer countries have a higher rate of suicide?**
    
    # Instead of looking at trends within countries, here I take every country and calculate their mean GDP (per capita) across all the years in which data is available. I then measure how this relates to the countries suicide rate across all those years.
    
    # The end result is one data point per country, intended to give a general idea of the wealth of a country and its suicide rate.
    
    country_mean_gdp <- data %>%
      group_by(country, continent) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
                gdp_per_capita = mean(gdp_per_capita))
    
    # ggplot(country_mean_gdp, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) +
    #   geom_point() +
    #   scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) +
    #   labs(title = "Correlation between GDP (per capita) and Suicides per 100k",
    #        subtitle = "Plot containing every country",
    #        x = "GDP (per capita)",
    #        y = "Suicides per 100k",
    #        col = "Continent")
    model1 <- lm(suicide_per_100k ~ gdp_per_capita, data = country_mean_gdp)
    
    gdp_suicide_no_outliers <- model1 %>%
      augment() %>%
      arrange(desc(.cooksd)) %>%
      filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
      inner_join(country_mean_gdp, by = c("suicide_per_100k", "gdp_per_capita")) %>%
      select(country, continent, gdp_per_capita, suicide_per_100k)
    
    model2 <- lm(suicide_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)
    
    # summary(model2)
    
    # The **p-value** of the model is **`r round(glance(model2)$p.value, 4)`** < 0.05. This means we can *reject* the hypothesis that a countries GDP (per capita) has no association with it's rate of suicide (per 100k).
    
    # The r-squared is **`r round(glance(model2)$r.squared, 4)`**, so GDP (per capita) explains very little of the variance in suicide rate overall.
    
    # **What does all this mean?**
    
    # There is a weak but significant positive linear relationship - ***richer* countries are associated with *higher* rates of suicide**, but this is a ***weak* relationship** which can be seen from the graph below.
    
    ggplot(gdp_suicide_no_outliers, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
      geom_point() + 
      geom_smooth(method = "lm", aes(group = 1)) + 
      scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
      labs(title = "Correlation between GDP (per capita) and Suicides per 100k (Suicides=8.7718+0.1115*GDP)", 
           x = "GDP (per capita)", 
           y = "Suicides per 100k", 
           col = "Continent") + 
      theme(legend.position = "none")
    
  })
    ## Render Map
  # show map using googleVis
  output$map <-renderGvis({
    gvisGeoChart(data = df1(), locationvar = "country", colorvar = input$type,
                 options = list(region="world", displayMode="auto",
                                resolution="countries", width="100%", height="100%",
                                colorAxis="{colors:['#6f92e6', '#f9897e']}"))
  })

  output$maxBox <- renderInfoBox({
    max_value <- max(df1()$suicides100)
    max_state <- df1()$country[df1()$suicides100 == max_value]
    infoBox(max_state, max_value, icon = icon("hand-o-up"), color = "red")
  })
  output$avgBox <- renderInfoBox({
    avg_value <- round(mean(df1()$suicides100), 2)
    infoBox("Average Global Suicides (/100k)", avg_value, icon = icon("calculator"), color = "orange")
  })
  
  
  #### Graphs Tab ######################################################
  ## Graphs and Histograms
  # cont, sex, gdp, year
  df2 <- reactive({
    df2 = df %>% 
      filter(continent == input$cont) %>%
      filter(sex == input$sex) %>%
      filter(between(gdp.capita, input$gdp[1], input$gdp[2])) %>%
      filter(between(year, input$year[1], input$year[2])) %>% 
      arrange(desc(suicides))
  })
  
  output$line <- renderPlotly(
    # check by gdp.capita for each country in the eu of suicides per 100k by generation (density)
    ggplotly(df2() %>%
      group_by(country, year) %>% 
      summarise(suicides100 = mean(suicides.per.100k)) %>% 
      ggplot(aes(year, suicides100)) +
      geom_line(aes(color = country), show.legend = FALSE,
                  alpha = 0.25) +
      labs(title = "Suicides (/100k) vs. Year", x = "Year", y = "Suicides (/100k)") +
      scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
      theme_gdocs()) %>% 
      layout(legend = list(x = 100, y = 0.5))
  )
  
  output$hist <- renderPlotly(
    ggplotly(df2() %>% 
      group_by(country, year) %>% 
      ggplot(aes(year, gdp.capita)) + 
      geom_col(aes(fill = country), position = "dodge") +
      theme_gdocs() +
      labs(title = "GDP per Capita vs. Year", x = "Year", y = "GDP per Capita"))
    # ggplot(df1()) + geom_smooth()
  )
  
  output$scat <- renderPlotly(
    ggplotly(df2() %>%
      group_by(country) %>% 
      summarise(population = mean(population), suicides = sum(suicides)) %>% 
      ggplot() +
      geom_point(aes(x = population, y = suicides, size = population, color = country),
                 show.legend = FALSE, alpha = 0.5) +
      theme_gdocs() +
      labs(title = "Suicides vs. Population", x = "Population", y = "Suicides"))
  )

  
  
  #### Table Tab #######################################################
  ## Render Data Table with Filtering Options
  output$table = DT::renderDataTable({
    data
  })
  
  #### Reddit Tab ######################################################
  ## Shiny doesn't render images well so will just attach with tag
  # output$reddit = renderImage(
  #   df %>%
  #     filter(country %in% c("United States", "Canada", "Australia", "Mexico", "South Korea")) %>% 
  #     group_by(year,country) %>% 
  #     summarise(suicides = sum(suicides), population = mean(population), gdp.capita = mean(gdp.capita)) %>% 
  #     ggplot(aes(suicides, population, color = country, size = gdp.capita, shape = country)) + 
  #     geom_point(alpha = 0.7) +
  #     scale_shape_manual(values=c(0,1,2,5,6)) +
  #     theme_gdocs() +
  #     labs(title = 'Year: {frame_time}', x = 'Suicides', y = 'Population') +
  #     transition_time(as.integer(year)) +
  #     view_follow(fixed_y = TRUE) +
  #     shadow_wake(wake_length = 0.05, alpha = FALSE)
  # )
  # 
  # df %>%
  #   filter(country == "United States", sex == "male") %>%
  #   group_by(year, country) %>%
  #   summarise(suicides = sum(suicides), population = mean(population), gdp.capita = mean(gdp.capita)) %>%
  #   ggplot(aes(suicides, population, color = country, size = gdp.capita, shape = country)) +
  #   geom_point(alpha = 0.7, show.legend = FALSE) +
  #   theme_gdocs() +
  #   scale_color_manual(values="#56B4E9") +
  #   labs(title = 'United States Males Year: {frame_time}', x = 'Suicides', y = 'Population') +
  #   transition_time(as.integer(year)) +
  #   shadow_wake(wake_length = 0.05, alpha = FALSE)
  # 
  # df %>%
  #   filter(country == "United States", sex == "female") %>%
  #   group_by(year, country) %>%
  #   summarise(suicides = sum(suicides), population = mean(population), gdp.capita = mean(gdp.capita)) %>%
  #   ggplot(aes(suicides, population, color = country, size = gdp.capita, shape = country)) +
  #   geom_point(alpha = 0.7, show.legend = FALSE) +
  #   theme_gdocs() +
  #   labs(title = 'United States Females Year: {frame_time}', x = 'Suicides', y = 'Population') +
  #   transition_time(as.integer(year)) +
  #   shadow_wake(wake_length = 0.05, alpha = FALSE)
})












