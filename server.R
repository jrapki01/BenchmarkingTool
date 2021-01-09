
server <- function(input, output, session){
  
#######################
## Observe Events - Compare Areas
#######################
  
  ## Observe local authority grouping
  observe({
    ## Interesting note here - have to use the if()else{} method for this to work
    la_group <- if(input$ComparisonGroup_Areas == "STATNEIGH_GROUP"){STATNEIGH_GROUP} else 
      if(input$ComparisonGroup_Areas == "STATNEIGH_201920_GROUP"){STATNEIGH_201920_GROUP} else
      if(input$ComparisonGroup_Areas == "REGIONAL_GROUP"){REGIONAL_GROUP} else 
        if(input$ComparisonGroup_Areas == "EASTERN_GROUP"){EASTERN_GROUP} else 
          if(input$ComparisonGroup_Areas == "IMD_GROUP"){IMD_GROUP} else
            {LA_CUSTOM}
    
    ## This updates the Indicator input in the ui
    updateSelectInput(session, 
                      inputId = "LAGroup_Areas", 
                      label = "Select local authorities:", 
                      choices = la_group, 
                      selected = la_group)
})
  
  ## Observe data source to control topic list
  observe({
    group <- if(input$DataTopic_Areas == "CIN"){dfCIN_topics$metric} else 
      if(input$DataTopic_Areas == "LAC"){dfLAC_topics$metric} else 
        if(input$DataTopic_Areas == "FOST"){dfFOST_topics$metric} else
          if(input$DataTopic_Areas == "WORK"){dfWORK_topics$metric} else
      {dfCIN_topics$metric}
    
    updateSelectInput(session,
                      inputId = "IndicatorGroup_Areas",
                      label = "Select topic:",
                      choices = group)
  })
  
  
  ## Observe data source to control the indicator list
  observe({
    ## Interesting note here - have to use the if()else{} method for this to work
    group <- if(input$DataTopic_Areas == "CIN"){dfCIN_topics} else 
      if(input$DataTopic_Areas == "LAC"){dfLAC_topics} else 
        if(input$DataTopic_Areas == "FOST"){dfFOST_topics} else
          if(input$DataTopic_Areas == "WORK"){dfWORK_topics} else
        {dfCIN_topics}
    
    measure <- group %>% 
      filter(metric == input$IndicatorGroup_Areas)
    
    measure <- setNames(measure$distinct_id, measure$subset_category)
    
    ## This updates the Indicator input in the ui
    updateSelectInput(session, 
                      inputId = "Measure_Areas", 
                      label = "Select measure:", 
                      choices = measure)
  })

  
#######################
## Observe Events - Trends
#######################

  ## Observe data source to control topic list
  observe({
    group <- if(input$DataTopic_Trends == "CIN"){dfCIN_topics$metric} else 
      if(input$DataTopic_Trends == "LAC"){dfLAC_topics$metric} else 
        if(input$DataTopic_Trends == "FOST"){dfFOST_topics$metric} else
          if(input$DataTopic_Trends == "WORK"){dfWORK_topics$metric} else
      {dfCIN_topics$metric}
    
    updateSelectInput(session,
                      inputId = "IndicatorGroup_Trends",
                      label = "Select topic:",
                      choices = group)
  })
  
  
  ## Observe data source to control the indicator list
  observe({
    ## Interesting note here - have to use the if()else{} method for this to work
    group <- if(input$DataTopic_Trends == "CIN"){dfCIN_topics} else 
      if(input$DataTopic_Trends == "LAC"){dfLAC_topics} else 
        if(input$DataTopic_Trends == "FOST"){dfFOST_topics} else
          if(input$DataTopic_Trends == "WORK"){dfWORK_topics} else
      {dfCIN_topics}
    
    measure <- group %>% 
      filter(metric == input$IndicatorGroup_Trends)
    
    measure <- setNames(measure$distinct_id, measure$subset_category)
    
    ## This updates the Indicator input in the ui
    updateSelectInput(session, 
                      inputId = "Measure_Trends", 
                      label = "Select measure:", 
                      choices = measure)
  })
  
#######################
## Observe Events - Ranks and Quartiles
#######################
  
  ## Observe data source to control topic list
  observe({
    group <- if(input$DataTopic_RAQ == "CIN"){dfCIN_topics$metric} else 
      if(input$DataTopic_RAQ == "LAC"){dfLAC_topics$metric} else 
        if(input$DataTopic_RAQ == "FOST"){dfFOST_topics$metric} else
          if(input$DataTopic_RAQ == "WORK"){dfWORK_topics$metric} else
      {dfCIN_topics$metric}
    
    updateSelectInput(session,
                      inputId = "IndicatorGroup_RAQ",
                      label = "Select topic:",
                      choices = group)
  })
  
  
  ## Observe data source to control the indicator list
  observe({
    ## Interesting note here - have to use the if()else{} method for this to work
    group <- if(input$DataTopic_RAQ == "CIN"){dfCIN_topics} else 
      if(input$DataTopic_RAQ == "LAC"){dfLAC_topics} else 
        if(input$DataTopic_RAQ == "FOST"){dfFOST_topics} else
          if(input$DataTopic_RAQ == "WORK"){dfWORK_topics} else
      {dfCIN_topics}
    
    measure <- group %>% 
      filter(metric == input$IndicatorGroup_RAQ)
    
    measure <- setNames(measure$distinct_id, measure$subset_category)
    
    ## This updates the Indicator input in the ui
    updateSelectInput(session, 
                      inputId = "Measure_RAQ", 
                      label = "Select measure:", 
                      choices = measure)
  })
  
#########################
## Compare Areas Output
#########################
  
  ## Control the data source
  data_choice_Areas <- reactive({
    dfAreas <- if(input$DataTopic_Areas == "CIN"){dfCIN} else 
      if(input$DataTopic_Areas == "LAC"){dfLAC} else 
        if(input$DataTopic_Areas == "FOST"){dfFOST} else
          if(input$DataTopic_Areas == "WORK"){dfWORK} else
          {dfCIN}
  })
  
  ## Compare Areas Data Tidy
  df_areacomp <- reactive({
    ## This if else statement controls which comparator group we are going to use   
    Comp <- if(input$ComparisonGroup_Areas == "STATNEIGH_GROUP"){STATNEIGH_GROUP} else 
      if(input$ComparisonGroup_Areas == "STATNEIGH_201920_GROUP"){STATNEIGH_201920_GROUP} else
      if(input$ComparisonGroup_Areas == "EASTERN_GROUP"){EASTERN_GROUP} else
        if(input$ComparisonGroup_Areas == "IMD_GROUP"){IMD_GROUP} else
          if(input$ComparisonGroup_Areas == "REGIONAL_GROUP"){REGIONAL_GROUP} else
            {LA_CUSTOM}
    
    ## The main data manipulation
    z <- data_choice_Areas() %>% 
      filter(year == input$Year_Areas) %>% 
      filter(old_la_code %in% Comp) %>%
      filter(distinct_id == input$Measure_Areas) %>%       
      select(
        period,
        old_la_code,
        la_name,
        subset_category,
        Indicator,
        UCI,
        LCI) %>% 
      ## This part controls whether an area is significantly higher or lower than England value
      ## Create a column of England values to compare
      mutate(Eng = Indicator[old_la_code == "999"]) %>%
      ## Creates another columns based on an ifelse control.
      ## If the UCI of an area is less than the Eng then lower, if IndLCI is greater than Eng then higher
      ## otherwise not significantly different
      mutate(Cols = ifelse(old_la_code == "999", "England", ifelse(is.na(UCI), "Similar", ifelse(UCI < Eng, "Lower", ifelse(LCI > Eng, "Higher", "Similar"))))) %>%
      ## Take away the confidence intervals for England as this is the 'population'
      mutate(LCI = ifelse(old_la_code == "999", NA, LCI)) %>%
      mutate(UCI = ifelse(old_la_code == "999", NA, UCI)) %>%
      ## As a factor so that the colours work later on in the plot
      mutate(Cols = as.factor(Cols))
  })
  
  ## Compare Areas Data Table
  df_areacomptable <- reactive({
    ## Take the reactive data from above
    y <- df_areacomp() %>%
      ## Drop the ID, EngLCI, EngUCI and Cols columns
      select(-c(period, old_la_code, subset_category, Eng, Cols)) %>%
      arrange(Indicator) %>% 
      ## Perform some renaming
      rename(LA = la_name, "Value" = "Indicator", "Lower 95% CI" = "LCI", "Upper 95% CI" = "UCI") %>% 
      ## Convert to a data frame so that DT package can read it
      as.data.frame()
  })
  

  ## Compare Areas Bar Plot
  output$AreaComp <- renderPlot({ 
    ## This is the area comparison plot
    ## The main ggplot function the first part is the main column plot
    ggplot(df_areacomp(), aes(x = reorder(la_name, desc(Indicator)), y = Indicator, fill = Cols)) +
      geom_col(size = 2)+
      ## Add the error bars
      ## Removing values when there are no confidence intervals, work around to force them to be zero
      geom_errorbar(aes(ymin = ifelse(is.na(LCI), 0, LCI), ymax = ifelse(is.na(UCI), 0, UCI)), colour = "black")  +
      ## Make the x axis reflect the correct order of the names we defined earlier
      #scale_x_discrete(labels = rev(df_areacomp()$Name))+
      ## Make sure the fill colours match the pal settings above
      scale_fill_manual(
        name = 'Compared to England:',
        values = pal,
        limits = names(pal)
      ) +
      ## Axis labels are blank for now - want to automate these
      ggtitle(' ')+
      xlab('')+
      ylab('')+
      ## Flip the axes so that it is horizontal
      coord_flip()+
      ## Setting themes and axis text size
      theme_minimal()+
      theme(legend.position = "top",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
    
  })
  ## This is the download for the area comp plot
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$AreaComp_Down <- downloadHandler(
    filename =  function() {
      paste("AreaComp_", Sys.Date(), ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file,
          width = 1024,
          height = 750)
      print(ggplot(df_areacomp(), aes(x = reorder(la_name, desc(Indicator)), y = Indicator, fill = Cols)) +
                  geom_col(size = 2)+
                  ## Add the error bars
                  ## Removing values when there are no confidence intervals, work around to force them to be zero
                  geom_errorbar(aes(ymin = ifelse(is.na(LCI), 0, LCI), ymax = ifelse(is.na(UCI), 0, UCI)), colour = "black")  +
                  ## Make the x axis reflect the correct order of the names we defined earlier
                  #scale_x_discrete(labels = rev(df_areacomp()$Name))+
                  ## Make sure the fill colours match the pal settings above
                  scale_fill_manual(
                    name = 'Compared to England:',
                    values = pal,
                    limits = names(pal)
                  ) +
                  ## Axis labels are blank for now - want to automate these
                  ggtitle(' ')+
                  xlab('')+
                  ylab('')+
                  ## Flip the axes so that it is horizontal
                  coord_flip()+
                  ## Setting themes and axis text size
                  theme_minimal()+
                  theme(legend.position = "top",
                        legend.title = element_text(size = 14),
                        legend.text = element_text(size = 14),
                        axis.text.x = element_text(size = 16),
                        axis.text.y = element_text(size = 16))) # for GGPLOT
      dev.off()  # turn the device off
      
    } 
  )
  
  ## This controls the area comparison table
  output$AreaCompTable <- renderDataTable(
    ## Set the pagelength to by the maximum possible number of areas which is 15 based on IMD_Group
    df_areacomptable(), selection = 'none',rownames = FALSE,
    options = list(
      pageLength = 15,
      dom = "t")
  )
  
  ## This is the download for the area comp table
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$AreaCompTable_Down <- downloadHandler(
    filename =  function() {
      paste("AreaCompTable_", Sys.Date(), ".csv", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      write.csv(df_areacomptable(), file, row.names = FALSE)
    } 
  )
  
  ## Text output for the title of this page
  output$text_Areas <- renderText({
    ## Title for CIN
    if(input$DataTopic_Areas == "CIN"){as.character(dfCIN_topics$subset_category[dfCIN_topics$distinct_id == input$Measure_Areas])} else
      ## Title for LAC      
      if(input$DataTopic_Areas == "LAC"){as.character(dfLAC_topics$subset_category[dfLAC_topics$distinct_id == input$Measure_Areas])} else
        ## Title for Fostering
        if(input$DataTopic_Areas == "FOST"){as.character(dfFOST_topics$subset_category[dfFOST_topics$distinct_id == input$Measure_Areas])} else
          ## Title for Workforce
          if(input$DataTopic_Areas == "WORK"){as.character(dfWORK_topics$subset_category[dfWORK_topics$distinct_id == input$Measure_Areas])}
        else {"ERROR"} })
  
##############################
## Trends Outputs
##############################
  
  ## Control the data source
  data_choice_Trends <- reactive({
    dfTrends <- if(input$DataTopic_Trends == "CIN"){dfCIN} else 
      if(input$DataTopic_Trends == "LAC"){dfLAC} else
    if(input$DataTopic_Trends == "FOST"){dfFOST} else
      if(input$DataTopic_Trends == "WORK"){dfWORK} else
      {dfCIN}
  })
  
  
  ## Date for timeseries plot
  dftimeseries <- reactive({

    ## The main data manipulation
    y <- data_choice_Trends() %>% 
      filter(old_la_code %in% c("882", "999")) %>%
      filter(year %in% input$Year_Trends) %>% 
      filter(distinct_id == input$Measure_Trends) %>%       
      select(
        period,
        old_la_code,
        la_name,
        subset_category,
        Indicator,
        UCI,
        LCI) %>% 
      group_by(period) %>%
      arrange(desc(old_la_code)) %>% 
      ## This part controls whether an area is significantly higher or lower than England value
      mutate(Engts = ifelse(period == lag(period), Indicator[old_la_code == "999"], NA)) %>%
      ungroup() %>%
      ## Creates another columns based on an ifelse control.
      ## If the UCI of an area is less than the EngLCI then lower, if IndLCI is greater than EngUCI then higher
      ## otherwise not significantly different
      mutate(Cols = ifelse(old_la_code == "999", "England", ifelse(is.na(UCI), "Similar", ifelse(UCI < Engts, "Lower", ifelse(LCI > Engts, "Higher", "Similar"))))) %>%
      ## This will remove the confidence intervals for England as this is 'population'
      mutate(LCI = ifelse(old_la_code == "999", NA, LCI)) %>%
      mutate(UCI = ifelse(old_la_code == "999", NA, UCI)) %>%
      ## As a factor so that the colours work later on in the plot
      mutate(Cols = as.factor(Cols))
  })
  
  ## Timeseries plot
  output$TimeSeriesPlot <- renderPlot({
    
    ggplot(dftimeseries(), aes(x = period, y = Indicator, group = la_name))+
      geom_line() +
      geom_errorbar(aes(ymin = ifelse(is.na(LCI), 0, LCI), ymax = ifelse(is.na(UCI), 0, UCI)))  +
      geom_point(aes(colour = Cols), size = 5) +
      ## Make sure the fill colours match the pal settings above
      scale_colour_manual(
        name = 'Compared to England:',
        values = pal2,
        limits = names(pal2)
      ) +
      ## Axis labels are blank for now - want to automate these
      ggtitle(' ')+
      xlab('')+
      ylab('')+
      theme_minimal()+
      theme(legend.position = "top",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
  })
  
  ## Download options for the time series plot
  output$TimeSeries_Down <- downloadHandler(
    filename =  function() {
      paste("TimeSeries_", Sys.Date(), ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file,
          width = 1024,
          height = 750)
      print(ggplot(dftimeseries(), aes(x = period, y = Indicator, group = la_name))+
              geom_point(aes(colour = Cols), size = 5) +
              geom_line() +
              geom_errorbar(aes(ymin = ifelse(is.na(LCI), 0, LCI), ymax = ifelse(is.na(UCI), 0, UCI)))  +
              ## Make sure the fill colours match the pal settings above
              scale_colour_manual(
                name = 'Compared to England:',
                values = pal2,
                limits = names(pal2)
              ) +
              ## Axis labels are blank for now - want to automate these
              ggtitle(' ')+
              xlab('')+
              ylab('')+
              theme_minimal()+
              theme(legend.position = "top",
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 14),
                    axis.text.x = element_text(size = 16),
                    axis.text.y = element_text(size = 16)))
      dev.off()  # turn the device off
    } 
  )
  
  ## Create a separate dataset for the time series table
  ## Need to rotate so easier to make this separately
  dftimeseriestable <- reactive({
    y <- dftimeseries() %>% 
      mutate(la_name = as.factor(la_name), period = as.factor(period)) %>% 
      mutate(LCI = ifelse(old_la_code == "999", "-", LCI)) %>%
      mutate(UCI = ifelse(old_la_code == "999", "-", UCI)) %>% 
      rename("Year" = period) %>% 
      rename("Name" = la_name) %>% 
      rename("Value" = Indicator) %>% 
      rename("Lower 95% CI" = LCI) %>%
      rename("Upper 95% CI" = UCI) %>% 
      select(!c(old_la_code,subset_category, Engts, Cols)) %>% 
      setDT() %>% 
      dcast.data.table(Year ~ Name, value.var = c("Value", "Lower 95% CI", "Upper 95% CI")) %>% 
      setcolorder(c(1, 2, 4, 6, 3, 5, 7)) %>% 
      select(!c(3, 4)) %>% 
      setnames(c("Year", " England", "Southend", "Lower 95% CI", "Upper 95% CI"))
    
  })
  
  ## Time series data table  
  output$TimeSeriesTable <- renderDataTable(
    ## Set the pagelength to by the maximum possible number of areas which is 15 based on IMD_Group
    dftimeseriestable(), selection = 'none',rownames = FALSE,
    options = list(
      pageLength = 15,
      dom = "t")
  )
  
  ## This is the download for the area comp table
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$TimeSeriesTable_Down <- downloadHandler(
    filename =  function() {
      paste("TimeSeriesTable_", Sys.Date(), ".csv", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      write.csv(dftimeseries(), file, row.names = FALSE)
    } 
  )
  
  ## Text output for the title of this page
  output$text_Trends <- renderText({
    ## Title for CIN
    if(input$DataTopic_Trends == "CIN"){as.character(dfCIN_topics$subset_category[dfCIN_topics$distinct_id == input$Measure_Trends])} else
      ## Title for LAC      
      if(input$DataTopic_Trends == "LAC"){as.character(dfLAC_topics$subset_category[dfLAC_topics$distinct_id == input$Measure_Trends])} else
        ## Title for Fostering
        if(input$DataTopic_Trends == "FOST"){as.character(dfFOST_topics$subset_category[dfFOST_topics$distinct_id == input$Measure_Trends])} else
          ## Title for Workforce
          if(input$DataTopic_Trends == "WORK"){as.character(dfWORK_topics$subset_category[dfWORK_topics$distinct_id == input$Measure_Trends])}
    else {"ERROR"} })
  
##############################
## Ranks and Quartile Outputs
##############################
  
  ## Control the data source
  ## Have to use another option for the boxplot later on - otherwise need to update multiple pages
  data_choice_RAQ <- reactive({
    
    dfRAQ <- if(input$DataTopic_RAQ == "CIN"){dfCIN} else 
      if(input$DataTopic_RAQ == "LAC"){dfLAC} else
    if(input$DataTopic_RAQ == "FOST"){dfFOST} else
      if(input$DataTopic_RAQ == "WORK"){dfWORK} else
       {dfCIN}
    
  })
  
  ## Boxplot Base Data
  dfrankplot <- reactive({
    ## The main data manipulation
    y <- data_choice_RAQ() %>% 
      ## Filter to exclude England and regions
      filter(!old_la_code %in% c("999","A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")) %>%
      filter(year %in% input$Year_RAQ) %>% 
      filter(distinct_id == input$Measure_RAQ) %>%       
      select(
        old_la_code,
        subset_category,
        period,
        Indicator) 
    
  })
  
  ## Boxplot Southend Data
  dfrankplot_sos <- reactive({
    ## The main data manipulation
    y <- data_choice_RAQ() %>% 
      ## Filter to be just Southend
      filter(old_la_code == "882") %>% 
      filter(year %in% input$Year_RAQ) %>% 
      filter(distinct_id == input$Measure_RAQ) %>%
      select(
        old_la_code,
        subset_category,
        period,
        Indicator
      ) 
    
  })
  
  ## Box plot for LA distribution
  output$DistributionPlot <- renderPlot({
    
    ggplot(dfrankplot(), aes(x = reorder(period, desc(period)), y = Indicator))+
      geom_boxplot()+
      geom_point(data = dfrankplot_sos(), aes(x = reorder(period, desc(period)), y = Indicator, colour = "Southend-on-Sea"), size = 4)+
      coord_flip() +
      ## Make sure the fill colours match the pal settings above
      scale_colour_manual(
        name = ' ',
        values = c("Southend-on-Sea" = "darkorange2")
      ) +
      ## Axis labels are blank for now - want to automate these
      ggtitle('')+
      labs(caption = "Range of all local authority values with Southend-on-Sea value shown in orange")+
      xlab('')+
      ylab('')+
      theme_minimal()+
      theme(legend.position = "top",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
  })
  
  
  ## Create a separate dataset for the ranks and quartiles table
  ## Need to rotate so easier to make this separately
  dfranktable <- reactive({
    ## The main data manipulation
    y <- data_choice_RAQ() %>% 
      filter(old_la_code == "882") %>%
      filter(year %in% input$Year_RAQ) %>% 
      filter(distinct_id == input$Measure_RAQ)  %>%
      select(
        period,
        la_name,
        Indicator,
        Rank,
        Quartile
      )  %>%       
      rename("Period" = 1, "LA" = 2, "Value" = 3) %>% 
      ## mutate the value column to two decimal places
      mutate(Value = round(Value, 3)) %>% 
      ## In all columns and rows where there is a NA replace with '-'
      mutate_all(funs(replace(., is.na(.), '-'))) %>% 
      setDT()
  })
  
  ##  Rank and quartile data table  
  output$RankTable <- renderDataTable(
    ## Set the pagelength to by the maximum possible number of areas which is 15 based on IMD_Group
    dfranktable(), selection = 'none', rownames = FALSE,
    options = list(
      pageLength = 15,
      dom = "t")
  )
  
  ## This is the download for the rank table
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$RankTable_Down <- downloadHandler(
    filename =  function() {
      paste("Rank_Quartile_Table_", Sys.Date(), ".csv", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      write.csv(dfranktable(), file, row.names = FALSE)
    } 
  )
  
  ## This is the download for the distribution plot
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$Distrib_Down <- downloadHandler(
    filename =  function() {
      paste("Distribution_", Sys.Date(), ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file,
          width = 1024,
          height = 750)
      print(ggplot(dfrankplot(), aes(x = reorder(period, desc(period)), y = Indicator))+
              geom_boxplot()+
              geom_point(data = dfrankplot_sos(), aes(x = reorder(period, desc(period)), y = Indicator, colour = "Southend-on-Sea"), size = 4)+
              coord_flip() +
              ## Make sure the fill colours match the pal settings above
              scale_colour_manual(
                name = ' ',
                values = c("Southend-on-Sea" = "darkorange2")
              ) +
              ## Axis labels are blank for now - want to automate these
              ggtitle('')+
              labs(caption = "Range of all local authority values with Southend-on-Sea value shown in orange")+
              xlab('')+
              ylab('')+
              theme_minimal()+
              theme(legend.position = "top",
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 14),
                    axis.text.x = element_text(size = 16),
                    axis.text.y = element_text(size = 16))) # for GGPLOT
      dev.off()  # turn the device off
      
    } 
  )
  
  ## Text output for the title of this page
  output$text_RAQ <- renderText({
    ## Title for CIN
    if(input$DataTopic_RAQ == "CIN"){as.character(dfCIN_topics$subset_category[dfCIN_topics$distinct_id == input$Measure_RAQ])} else
      ## Title for LAC      
      if(input$DataTopic_RAQ == "LAC"){as.character(dfLAC_topics$subset_category[dfLAC_topics$distinct_id == input$Measure_RAQ])} else
        ## Title for Fostering
        if(input$DataTopic_RAQ == "FOST"){as.character(dfFOST_topics$subset_category[dfFOST_topics$distinct_id == input$Measure_RAQ])} else
          ## Title for Workforce
          if(input$DataTopic_RAQ == "WORK"){as.character(dfWORK_topics$subset_category[dfWORK_topics$distinct_id == input$Measure_RAQ])}
    else {"ERROR"} })
  
##############################
## Population Outputs
##############################
  
  ## Population title
  output$populationtitle <- renderText({
    paste("Southend-on-Sea mid-", input$PopYearEst, " childrens population estimate")
  })
  
  ## Population data
  dfpopestimate <- reactive({
    
    ## The main data manipulation
    y <- dfPop %>% 
      ## Filter based on the year
      filter(Year == input$PopYearEst)
  })
  
  ## Population plot
  output$PopulationPlot <- renderPlot({
    ggplot(dfpopestimate()) +
      geom_bar(aes(Age, SouthendPerc, group = Gender, fill = "Female"), stat = 'identity', subset(dfpopestimate(),Gender=="Female")) +
      geom_bar(aes(Age, -SouthendPerc, group = Gender, fill = "Male"), stat = 'identity', subset(dfpopestimate(),Gender=="Male")) +
      geom_line(aes(Age, EnglandPerc, group = Gender, colour = "England"), stat = 'identity', subset(dfpopestimate(), Gender=="Female")) +
      geom_line(aes(Age, -EnglandPerc, group = Gender, colour = "England"), stat = 'identity', subset(dfpopestimate(), Gender=="Male")) +
      scale_y_continuous(breaks=seq(-0.04,0.04,0.01),labels=scales::percent(abs(seq(-0.04,0.04,0.01)), accuracy = 1)) +
      scale_colour_manual(name = "", values = cols) +
      scale_fill_manual(name = "", values = cols) +
      ylab("% of 0-17 population") +
      coord_flip() +
      theme_minimal()
  })
  
  ## Population data for Southend
  dfpopvalue <- reactive({
    
    ## The main data manipulation
    y  <- dfPop %>% 
      ## Filter based on the compartor group
      filter(Year == input$PopYearEst) %>% 
      select(Southend) %>% 
      summarise(Southend = sum(Southend))
  })
  
  ## Population output box
  output$populationBox <- renderValueBox({
    valueBox(
      dfpopvalue(), "Total Number of Children"
    )
  })
  
  ## Population download
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$Pop_Down <- downloadHandler(
    filename =  function() {
      paste("Population_", Sys.Date(), ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file,
          width = 1024,
          height = 750)
      print(    ggplot(dfpopestimate()) +
                  geom_bar(aes(Age, SouthendPerc, group = Gender, fill = "Female"), stat = 'identity', subset(dfpopestimate(),Gender=="Female")) +
                  geom_bar(aes(Age, -SouthendPerc, group = Gender, fill = "Male"), stat = 'identity', subset(dfpopestimate(),Gender=="Male")) +
                  geom_line(aes(Age, EnglandPerc, group = Gender, colour = "England"), stat = 'identity', subset(dfpopestimate(), Gender=="Female")) +
                  geom_line(aes(Age, -EnglandPerc, group = Gender, colour = "England"), stat = 'identity', subset(dfpopestimate(), Gender=="Male")) +
                  scale_y_continuous(breaks=seq(-0.04,0.04,0.01),labels=scales::percent(abs(seq(-0.04,0.04,0.01)), accuracy = 1)) +
                  scale_colour_manual(name = "", values = cols) +
                  scale_fill_manual(name = "", values = cols) +
                  ylab("% of 0-17 population") +
                  coord_flip() +
                  theme_minimal()) # for GGPLOT
      dev.off()  # turn the device off
      
    } 
  )
  
}
