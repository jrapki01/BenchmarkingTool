ui <- fluidPage(
                tags$head(HTML("<title>Social Care Benchmarking Tool</title> <link rel='icon' type='image/gif/png' href='favicon.png'>")),
                br(),
                theme = "bootstrap.css",
  
  h1("Children's Social Care Benchmarking Tool"),
  br(),
  ## Have to read in some javascript code to direct the image to be at the right of the navbar
  ## Change image source in the javascript code - found in www folder called code.js
  tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js"))),
  navbarPage("",

####################################
## Compare Areas Tab
####################################
             
  tabPanel('Compare Areas',
           fluidRow(
             box(width = 12,
                 splitLayout(cellArgs = list(style = "overflow: visible;"),
                             ## SelectInput - drop down list of indicator topics (this is to make the indicator list smaller)
                             selectInput("DataTopic_Areas", "Select data source:",
                                         c("Children in Need Census (inc CP & LAC)" = "CIN",
                                           "Looked After Children (903) Census" = "LAC",
                                           "Ofsted Fostering Dataset" = "FOST",
                                           "Social Work Workforce Dataset" = "WORK"),
                                         selected = "CIN", 
                                         multiple = FALSE),
                             
                             ## SelectInput - drop down list to years to choose from
                             selectInput("Year_Areas", "Select financial year to view:",
                                         c("2012/13" = "201213",
                                           "2013/14" = "201314",
                                           "2014/15" = "201415",
                                           "2015/16" = "201516",
                                           "2016/17" = "201617",
                                           "2017/18" = "201718",
                                           "2018/19" = "201819",
                                           "2019/20" = "201920"), 
                                         selected = "201920", 
                                         multiple = FALSE),
                             
                             ## SelectInput - drop down list to choose comparator group
                             selectInput("ComparisonGroup_Areas", "Select comparator group:",
                                         c("Statistical Neighbours (Pre 2019/20)" = "STATNEIGH_GROUP",
                                           "Statistical Neighbours (2019/20 onwards)" = "STATNEIGH_201920_GROUP",
                                           "Similar IMD Group" = "IMD_GROUP",
                                           "Eastern Region Authorities" = "EASTERN_GROUP",
                                           "Regional Averages" = "REGIONAL_GROUP",
                                           "Custom" = "CUSTOM_GROUP"),
                                         selected = "STATNEIGH_201920_GROUP", 
                                         multiple = FALSE),
                             
                             selectInput("LAGroup_Areas", "Select local authorities:",
                                         STATNEIGH_GROUP,
                                         multiple = TRUE)
                 )
             )
           ),
           fluidRow(
             box(width = 12,
                 splitLayout(cellArgs = list(style = "overflow: visible;"),
                             ## SelectInput - drop down list to choose measure group
                             selectInput("IndicatorGroup_Areas", "Select topic:",
                                         c(levels(dfCIN_topics$metric)),
                                         multiple = FALSE,
                                         width = "100%")
                 )
             )
           ),
           fluidRow(
             box(width = 12,
                 splitLayout(cellArgs = list(style = "overflow: visible;"),
                             ## selectInput - drop down list to choose measure
                             selectInput("Measure_Areas", "Select measure:",
                                         c(levels(dfCIN_topics$subset_category)),
                                         multiple = FALSE,
                                         width = "100%")
                 )
             )
           ),
           br(),
           # An additional row to enter the indicator label as a header
           fluidRow(
             box(width = 12,
                 h3(textOutput("text_Areas"))
             )
           ),
br(),
br(),
fluidRow(
  box(width = 12,
      splitLayout(
        DTOutput("AreaCompTable"),
        plotOutput("AreaComp")
      )
  )
),
br(),
fluidRow(
  box(width = 12,
      downloadButton('AreaComp_Down','Download Plot'),
      downloadButton('AreaCompTable_Down','Download Data'))
),
br(),
br(),
h4("Methodology"),
br(),
h5("Crude Rate: Calculated by dividing the total number of cases in a given time period by the total number of children in the population multiplied by 10,000"),
br(),
h5("Confidence Intervals: A confidence interval is a range of values that are used to quantify the imprecision in the estimate of a particular indicator.  
            Specifically a confidence interval quantifies the imprecision resulting from random variation in the measurement of the indicator. The wider the confidence interval 
            the less precise the indicator value is likely to be of the true underlying value."),
br()
),

#############################
## Trends
#############################

tabPanel('Trends',
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## SelectInput - drop down list of indicator topics (this is to make the indicator list smaller)
                           selectInput("DataTopic_Trends", "Select data source:",
                                       c("Children in Need Census (inc CP & LAC)" = "CIN",
                                         "Looked After Children (903) Census" = "LAC",
                                         "Ofsted Fostering Dataset" = "FOST",
                                         "Children and family social work workforce dataset" = "WORK"),
                                       selected = "CIN", 
                                       multiple = FALSE),
                           
                           ## SelectInput - drop down list to years to choose from
                           selectInput("Year_Trends", "Select financial year to view:",
                                       c("2012/13" = "201213",
                                         "2013/14" = "201314",
                                         "2014/15" = "201415",
                                         "2015/16" = "201516",
                                         "2016/17" = "201617",
                                         "2017/18" = "201718",
                                         "2018/19" = "201819",
                                         "2019/20" = "201920"), 
                                       selected = c("201213", "201314", "201415", "201516", "201617", "201718", "201819", "201920"), 
                                       multiple = TRUE)
               )
           )
         ),
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## SelectInput - drop down list to choose measure group
                           selectInput("IndicatorGroup_Trends", "Select topic:",
                                       c(levels(dfCIN_topics$metric)),
                                       multiple = FALSE,
                                       width = "100%")
               )
           )
         ),
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## selectInput - drop down list to choose measure
                           selectInput("Measure_Trends", "Select measure:",
                                       c(levels(dfCIN_topics$subset_category)),
                                       multiple = FALSE,
                                       width = "100%")
               )
           )
         ),
         br(),
         # An additional row to enter the indicator label as a header
         fluidRow(
           box(width = 12,
               h3(textOutput("text_Trends"))
           )
         ),
         br(),
         br(),
         fluidRow(
           box(width = 12,
               splitLayout(
                 plotOutput("TimeSeriesPlot"),
                 DTOutput("TimeSeriesTable")
               )
           )
         ),
         br(),
         fluidRow(
           box(width = 12,
               downloadButton('TimeSeries_Down','Download Plot'),
               downloadButton('TimeSeriesTable_Down','Download Data'))
         ),
         br(),
         br(),
         h4("Methodology"),
         br(),
         h5("Crude Rate: Calculated by dividing the total number of cases in a given time period by the total number of children in the population multiplied by 10,000"),
         br(),
         h5("Confidence Intervals: A confidence interval is a range of values that are used to quantify the imprecision in the estimate of a particular indicator.  
            Specifically a confidence interval quantifies the imprecision resulting from random variation in the measurement of the indicator. The wider the confidence interval 
            the less precise the indicator value is likely to be of the true underlying value."),
         br()
),

#############################
## Ranks and Quartiles
#############################

tabPanel('Ranks and Quartiles',
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## SelectInput - drop down list of indicator topics (this is to make the indicator list smaller)
                           selectInput("DataTopic_RAQ", "Select data source:",
                                       c("Children in Need Census (inc CP & LAC)" = "CIN",
                                         "Looked After Children (903) Census" = "LAC",
                                         "Ofsted Fostering Dataset" = "FOST",
                                         "Children and family social work workforce dataset" = "WORK"),
                                       selected = "CIN", 
                                       multiple = FALSE),
                           
                           ## SelectInput - drop down list to years to choose from
                           selectInput("Year_RAQ", "Select financial year to view:",
                                       c("2012/13" = "201213",
                                         "2013/14" = "201314",
                                         "2014/15" = "201415",
                                         "2015/16" = "201516",
                                         "2016/17" = "201617",
                                         "2017/18" = "201718",
                                         "2018/19" = "201819",
                                         "2019/20" = "201920"), 
                                       selected = c("201213", "201314", "201415", "201516", "201617", "201718", "201819", "201920"), 
                                       multiple = TRUE)
               )
           )
         ),
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## SelectInput - drop down list to choose measure group
                           selectInput("IndicatorGroup_RAQ", "Select topic:",
                                       c(levels(dfCIN_topics$metric)),
                                       multiple = FALSE,
                                       width = "100%")
               )
           )
         ),
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## selectInput - drop down list to choose measure
                           selectInput("Measure_RAQ", "Select measure:",
                                       c(levels(dfCIN_topics$subset_category)),
                                       multiple = FALSE,
                                       width = "100%")
               )
           )
         ),
         br(),
         # An additional row to enter the indicator label as a header
         fluidRow(
           box(width = 12,
               h3(textOutput("text_RAQ"))
           )
         ),
         br(),
         br(),
         fluidRow(
           box(width = 12,
               splitLayout(
                 DTOutput("RankTable"),
                 plotOutput("DistributionPlot")
               )
           )
         ),
         br(),
         fluidRow(
           box(width = 12,
               downloadButton('Distrib_Down','Download Plot'),
               downloadButton('RankTable_Down','Download Data'))
         ),
         br(),
         br(),
         h4("Methodology"),
         br(),
         h5("Crude Rate: Calculated by dividing the total number of cases in a given time period by the total number of children in the population multiplied by 10,000"),
         br(),
         h5("Rank: Data is arranged from smallest to largest."),
         br(),
         h5("Quartile: A quartile divides data into three points (a lower quartile, median and upper quartile) to form four groups of the data. Each quartile contains 25% of the total observations. Data is arranged from smallest to largest: First quartile (Q1) contains the lowest 25% of numbers; Second quartile (Q2) between 25.1% and 50% (median); Third quartile (Q3) between 50.1% to 75%; and Fourth quartile (Q4) contains the highest 25% of numbers."),
         br(),
         h5("Boxplot: A standardized way of displaying the distribution of data based on the minimum value, the lower quartile, the median (middle value), the upper quartile and the maximum value. Outliers are represented by circles. Boxplots can tell you if data is symmetrical, how tightly it is grouped and if data is skewed. See below for an explanation on how to understand a boxplot and how the quartiles relate to the quartiles in the table."),
         img(src='IQR.png', align = "left"),
         br()
),
#############################
## Population
#############################

tabPanel('Population',
         fluidRow(
           box(width = 12,
               splitLayout(cellArgs = list(style = "overflow: visible;"),
                           ## SelectInput - drop down list to years to choose from
                           selectInput("PopYearEst", "Select mid-year estimate:", 
                                       c("Mid-2019" = "2019",
                                         "Mid-2018" = "2018",
                                         "Mid-2017" = "2017",
                                         "Mid-2016" = "2016",
                                         "Mid-2015" = "2015",
                                         "Mid-2014" = "2014",
                                         "Mid-2013" = "2013",
                                         "Mid-2012" = "2012"), 
                                       selected = "2019",
                                       multiple = FALSE)
               )
           )
         ),
         br(),
         fluidRow(
           box(width = 12,
               h3(textOutput("populationtitle"))
           )
         ),
         br(),
         fluidRow(
           box(width = 12,
               splitLayout(
                 plotOutput("PopulationPlot"),
                 valueBoxOutput("populationBox")
               )
           )
         ),
         br(),
         fluidRow(
           box(width = 12,
               downloadButton('Pop_Down','Download Plot'))
         ),
         br(),
         fluidRow(
           box(width = 12,
               h4(tags$a("Interactive Population Tool for Southend-on-Sea", href="https://sbcdata.shinyapps.io/Southend_population_tool_IMD2019/", target="_blank"))
           )
         ),
         br()
),
#############################
## Definitions and Sources
#############################

tabPanel('Definitions and Sources',
         br(),
         h3("Definitions"),
         br(),
         h5("The children in need census covers all children who are referred to children's social care services even if no further action is taken. 
            This includes children looked after (CLA), those supported in their families or independently (CSF/I), and children who are the subject of a child protection plan."),
         h5("A child in need is defined under the Children Act 1989 as a child who is unlikely to reach or maintain a satisfactory level
            of health or development, or their health or development will be significantly impaired without the provision of services, or the child is disabled."),
         h5("The children in need census includes all vulnerable children including: unborn children; babies; older children; young carers; disabled children; and those who are
            in secure settings. Please note that whilst most children will be aged under 18, there will be statistics related to young people aged 18 or over who are still
            receiving care and accomodation or post-care support from children's services"),
         h5("The looked after children census (SSDA903) includes nformation on children looked after in England, including numbers of looked after children adopted, care leavers and looked after children who are missing."),
         br(),
         h3("Sources"),
         br(),
         h5("The statistics used in this benchmarking tool have been sourced from the Department for Education and Ofsted."),
         br(),
         h4("Children in Need census:"),
         h5(tags$a("2019-20 Children in Need Census", href="https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/2020")),
         h5(tags$a("2018-19 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-2018-to-2019")),
         h5(tags$a("2017-18 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-2017-to-2018")),
         h5(tags$a("2016-17 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-2016-to-2017")),
         h5(tags$a("2015-16 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-2015-to-2016")),
         h5(tags$a("2014-15 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-2014-to-2015")),
         h5(tags$a("2013-14 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-2013-to-2014")),
         h5(tags$a("2012-13 Children in Need Census", href="https://www.gov.uk/government/statistics/characteristics-of-children-in-need-in-england-2012-to-2013")),
         br(),
         h4("Looked after Children census:"),
         h5(tags$a("2019-20 Looked after Children Census", href = "https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption-2019-to-2020")),
         h5(tags$a("2018-19 Looked after Children Census", href = "https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption-2018-to-2019")),
         h5(tags$a("2017-18 Looked after Children Census", href="https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption-2017-to-2018")),
         h5(tags$a("2016-17 Looked after Children Census", href="https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption-2016-to-2017")),
         h5(tags$a("2015-16 Looked after Children Census", href="https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption-2015-to-2016")),
         h5(tags$a("2014-15 Looked after Children Census", href="https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption-2014-to-2015")),
         h5(tags$a("2013-14 Looked after Children Census", href="https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption--2")),
         h5(tags$a("2012-13 Looked after Children Census", href="https://www.gov.uk/government/statistics/children-looked-after-in-england-including-adoption")),
         br(),
         h4("Ofsted Fostering Dataset:"),
         h5(tags$a("2019-20 Ofsted Fostering Dataset", href = "https://www.gov.uk/government/statistics/fostering-in-england-1-april-2019-to-31-march-2020")),
         h5(tags$a("2018-19 Ofsted Fostering Dataset", href = "https://www.gov.uk/government/statistics/fostering-in-england-1-april-2018-to-31-march-2019")),
         h5(tags$a("2017-18 Ofsted Fostering Dataset", href="https://www.gov.uk/government/statistics/fostering-in-england-1-april-2017-to-31-march-2018")),
         h5(tags$a("2016-17 Ofsted Fostering Dataset", href="https://www.gov.uk/government/statistics/fostering-in-england-1-april-2016-to-31-march-2017")),
         h5(tags$a("2015-16 Ofsted Fostering Dataset", href="https://www.gov.uk/government/statistics/fostering-in-england-1-april-2015-to-31-march-2016")),
         h5(tags$a("2014-15 Ofsted Fostering Dataset", href="https://www.gov.uk/government/statistics/fostering-in-england-1-april-2014-to-31-march-2015")),
         h5(tags$a("2013-14 Ofsted Fostering Dataset", href="https://www.gov.uk/government/statistics/fostering-in-england-1-april-2013-to-31-march-2014")),
         h5(tags$a("2012-13 Ofsted Fostering Dataset", href="https://www.gov.uk/government/statistics/fostering-in-england-1-april-2012-to-31-march-2013")),
         br(),
         h4("Social Work Workforce Dataset:"),
         h5(tags$a("2018-19 Social Work Workforce Dataset", href = "https://www.gov.uk/government/statistics/childrens-social-work-workforce-2019")),
         h5(tags$a("2017-18 Social Work Workforce Dataset", href="https://www.gov.uk/government/statistics/childrens-social-work-workforce-2018")),
         h5(tags$a("2016-17 Social Work Workforce Dataset", href = "https://www.gov.uk/government/statistics/childrens-social-work-workforce-2017"))
         
)
)
)

