require(shiny)
require(shinythemes)
require(networkD3)
require(rWordCloud)

## A hidden input for IP address
#  https://groups.google.com/forum/#!topic/shiny-discuss/EGQhEyoEk3E
inputIp <- function(inputId, value = '') {
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}

## Shiny UI
shinyUI(
  fluidPage(
    theme = shinytheme("readable"),
    # singleton(tags$head(tags$script(src = "message-handler.js"))),
    singleton(tags$head(tags$script(src = "js/google-analytics.js"))),
    titlePanel("CanvasNet"),
    sidebarLayout(
      sidebarPanel(
        h6("This app helps you see the big picture of our class discussion on Canvas."),
        h6(paste("-- built for CI4311W, with ♥")),
        
        br(),
        inputIp("ipid"),
        selectInput("section", "Course Section", choices = c("Section 001" = token_sec001, "Section 002" = token_sec002)),
        dateRangeInput('dateRange', label = "Date Range", start = Sys.Date() - 14, end = Sys.Date(), min = "2015-09-01", max = "2015-12-24"),
        textInput("userId", "Your Canvas Id"),
        checkboxInput("hideTeacher", "Hide Teacher", value=TRUE),
        actionButton("update", "Submit", class = "btn-primary btn-sm")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs", id = "tab",
                    tabPanel("Network", visNetworkOutput("force")), #, textOutput("testtext")
                    tabPanel("Terms", 
                             column(5, sliderInput("freq", "Minimum Frequency:", min = 1,  max = 50, value = 15)),
                             column(7, sliderInput("max", "Maximum Number of Words:", min = 1,  max = 100,  value = 20)),
                             h5(htmlOutput("wordCount")), 
                             d3CloudOutput("d3Plot", width = "100%", height = 500)
                    )
        )
      )
    )
  ))