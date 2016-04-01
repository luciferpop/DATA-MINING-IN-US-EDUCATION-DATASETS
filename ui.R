library(leaflet)

shinyUI(navbarPage("Which college is your favorite?",

       tabPanel("University Overview",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("plot", h4("Choose one to plot:"),
                                 choices = c("Choose an operation"="", "SAT Scores","ACT Scores", "Admission Rate", "Cost of Attendance", "Undergraduate Enrollment",
                                                 "Top 15 meadian earnings")
                      ),
                    hr(),
                    helpText("Source: Department of Education, College Scorecard Data.")
                    ),
                  mainPanel(
                      tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Summuary", verbatimTextOutput("summary"))
                        )
                    )
                )
              ),
       
       tabPanel("Explore Your Favorite",
                  sidebarLayout(
                  sidebarPanel(
                    selectInput("type", h4("Governance Structure"), c("School type"="", "Public", "Private for-profit", "Private nonprofit"), multiple=TRUE),
                    selectInput("states", h4("States"), c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE),
                    sliderInput("sat", label = h4("SAT Scores"), min = 350.0, max = 785.0, value = c(520.0, 600.0)),
                    sliderInput("rate", label = h4("Admission Rate"), min = 0, max = 1.0, value = c(0.6, 0.8)),
                    sliderInput("cost", label = h4("Attendance Cost"), min = 6603, max = 62636, value = c(10000, 20000)),
                    sliderInput("earning", label = h4("Anticipate Earning"), min = 50000, max = 180000, value = c(100000, 150000))
                    ),
                  mainPanel(leafletOutput("map", height=700))
                )
        ),
       tabPanel("Component 3"
        ),
       navbarMenu("More",
       tabPanel("Sub-Component A"),
       tabPanel("Sub-Component B"))
))