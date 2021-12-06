shinyUI(
  fluidPage(
    dashboardPage(
      dashboardHeader(
        title = "Suicide Rates",
        titleWidth = 230
        
        ),
      dashboardSidebar(
        sidebarUserPanel(
          "VKU",
          image = "Me.png",
          subtitle = 'Data analysis with R'
        ),
        sidebarMenu(
          menuItem(text = 'Maps', icon = icon('globe'), tabName = 'maps'),
          menuItem(text = 'Graphs', icon = icon('chart-bar'), tabName = 'graphs'),
          menuItem(text = 'Data', icon = icon('database'), tabName = 'data'),
          menuItem(text = 'Global Analysis', icon = icon('database'), tabName = 'global')
        )
      ),
      
      dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        tabItems(
          tabItem(tabName = "maps",
                  fluidRow(infoBoxOutput("maxBox"),
                           infoBoxOutput("avgBox"),
                           infoBox("Average US Suicides", subtitle = "in 2018 (/100k)", 13.9,
                                   icon = icon("globe-americas"), fill = TRUE,
                                   href = "https://www.americashealthrankings.org/explore/annual/measure/Suicide/state/ALL", color = "light-blue")),
                  fluidRow(
                    column(9, box(htmlOutput("map"), height = "auto", width = "auto")),
                    column(3,
                      radioButtons("type", label = h3("Display map by: "),
                                   choices = list("Suicides" = "suicides",
                                                  "Suicides (/100k)" = "suicides100"),
                                   selected = "suicides"),
                      checkboxGroupInput("checkGroup", label = h3("Filter by age group: "),
                                   choices = list("5-14 years", "15-24 years",
                                                  "25-34 years", "35-54 years",
                                                  "55-74 years", "75+ years"),
                                   selected = c("5-14 years", "15-24 years", "25-34 years",
                                                "35-54 years", "55-74 years","75+ years")),
                      sliderInput("slider", label = h3("Year Range"), min = 1985, 
                                  max = 2016, value = c(1985, 2016), sep = "")
                      
                    )
                  ),
                  
          ),
          tabItem(tabName = "graphs",
                  fluidRow(
                    column(2, h3("Filter graphs by:"), align = "center"),
                    column(2,
                      selectInput("cont", label = h3("Continent"), 
                                  choices = list("Americas", "Africa",
                                                 "Asia", "Europe", "Oceania"))),
                    column(2,
                      radioButtons("sex", label = h3("Sex"), 
                                  choices = list("Female" = "female", "Male" = "male"))),
                    column(3,
                      sliderInput("gdp", label = h3("GDP Range"), min = 251, 
                                    max = 126352, value = c(251, 126352))),
                    column(3,
                      sliderInput("year", label = h3("Year Range"), min = 1985, 
                                    max = 2016, value = c(1985, 2016), sep = ""))
                    
                  ),
                  fluidRow(
                    box(plotlyOutput("line"), height = 420, width = 12)
                  ),
                  fluidRow(
                    box(plotlyOutput("hist"), height = 420, width = 12)
                  ),
                  fluidRow(
                    box(plotlyOutput("scat"), height = 420, width = 12)
                  )
          ),
          tabItem(tabName = "data",
                  fluidRow(box(DT::dataTableOutput("table"), width = 12))
          ),
          
          tabItem(tabName = "global",
                  
                    tabsetPanel(
                    tabPanel("Global Trend", plotOutput("globaltrend")),
                    tabPanel("By Continent", plotOutput("bycontinent")),
                    tabPanel("By Sex", plotOutput("bysex")),
                    tabPanel("By Age", plotOutput("byage")),
                    tabPanel("By Country", box(plotOutput("bycountry", height = "900", width = "800")))
                    ),
                  fluidRow(
                   plotOutput("bygdp", height = "570"))
                    
          )
        )
        
      )
    )
  )
)









