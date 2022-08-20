library(shiny)
library(sonicscrewdriver)
library(unpdata)
library(RColorBrewer)
library(plotrix)
library(curl)
library(schite)

options(warn=-1)

sites <- sites()
year <- as.POSIXlt(Sys.Date())$year + 1900
trait_data <- audioblast("data", "traits", check=F, trait="Time of Day of Call")


ui <- fluidPage(
  titlePanel("audioBlast Diel traits example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date",
                  "Date:",
                  min = as.Date(paste0(year,"-01-01"),"%Y-%m-%d"),
                  max = as.Date(paste0(year,"-12-31"),"%Y-%m-%d"),
                  value=Sys.Date(),
                  timeFormat="%Y-%m-%d",
                  step = 3,
                  animate = animationOptions(interval = 250,loop=TRUE)
      ),
      selectInput("loc",
                  "Location:",
                  sites$names,
                  selected = sites$names[1],
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      checkboxGroupInput("times",
                         "Time of day:",
                         choices = c(
                           "Sunrise",
                           "Sunset",
                           "Solar Noon",
                           "Civil Twilight",
                           "Nautical Twilight",
                           "Astronomical Twilight",
                           "Night",
                           "Nadir"
                         ),
                         selected = c(
                           "Sunrise",
                           "Sunset",
                           "Civil Twilight",
                           "Nautical Twilight",
                           "Astronomical Twilight",
                           "Night",
                           "Solar Noon"
                         ),
      ),
      selectInput("display",
                  "Diel data display:",
                  c("Main", "Core", 'Ring'),
                  selected = "Main",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tags$h4("Time of potential acoustic activity by species"),
      HTML("<p>This example pulls some acoustic activity trait data from
           <a href='http://audioblast.org'>audioBlast</a> that (in part) uses relative definitions
           for specifying times of day (e.g. dawn, sunset, night). For a specified day and location these times
           are converted to times and plotted. (These species do not actually occur in the locations specified,
           or at all times of year.)</p>

           <p>It is recomended to press the play button under the day slider to see the viualisation.</p>"),


      plotOutput("distPlot"),
      HTML("<br><br><br>"),
      tabsetPanel(type = "tabs",
                  tabPanel("How to cite", htmlOutput("citation")),
                  tabPanel("Data Providers", htmlOutput("citecalc")),
                  tabPanel("Data Visualisation", htmlOutput("citevis")),
                  tabPanel("Misc", htmlOutput("citmisc"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  citation <- list(
    cite_bibentry(
      bibentry(
        bibtype="Misc",
        title="audioBlast trait examples for Urban Nature Project sites",
        author="Ed Baker",
        url="http://shiny.ebaker.me.uk/audioblast-diel-traits/",
        year=2022
      )
    )
  )
  output$citation <- citationTabUI(citation)

  citcalc <- list(
    cite_r_package("suncalc"),
    cite_r_package("unpdata"),
    cite_bibentry(
      bibentry(
        bibtype="Misc",
        title="audioBlast!",
        author="Ed Baker",
        url="http://audioblast.org",
        year=2022
      )
    )
  )
  output$citecalc <- citationTabUI(citcalc)

  citvis <- list(
    cite_r_package("sonicscrewdriver"),
    cite_r_package("plotrix")
  )
  output$citevis <- citationTabUI(citvis)

  citmisc <- list(
    cite_r_package("schite"),
    cite_r_package("shiny")
  )
  output$citmisc <- citationTabUI(citmisc)

  output$distPlot <- renderPlot({
    lat <- as.numeric(sites[sites$names==input$loc,]$lat)
    lon <- as.numeric(sites[sites$names==input$loc,]$lon)

    if (input$display == "Main") {
      inner = 0
      outer = 2
      d.inner= 0.5
      d.outer = 2
    }
    if (input$display == "Core") {
      inner = 0
      outer = 1
      d.inner =1
      d.outer = 2
    }
    if (input$display == "Ring") {
      inner = 1.75
      outer = 2
      d.inner = 0.5
      d.outer = 1.75
    }
    t<- ab_diel_traits(
      trait_data,
      input$date,
      53,
      0)
    t <- t[!is.na(t$value_min) & !is.na(t$value_max),]

    #Process some reuslts
    matches <- 1
    i <- 1
    matched_rows <- rep(NA, 5)
    matched_values <- rep(NA, 5)
    while (matches < 11 & i < nrow(t)) {
      if (!t[i,"value_min"] %in% matched_values) {
        matched_values[matches] <- t[i,"value_min"]
        matched_rows[matches] <- i
        matches <- matches + 1
      }
      i <- i + 1
    }
    matched_rows <- matched_rows[!is.na(matched_rows)]
    t <- t[matched_rows,]

    dielPlot(as.POSIXct(input$date),lat,lon,c(inner,outer),input$times)

    cols<-brewer.pal(nrow(t),"Dark2")

    dielRings(t$taxon, t$value_min, t$value_max, cols=cols, limits=c(d.inner, d.outer))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
