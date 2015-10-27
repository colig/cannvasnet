library(shiny)
library(googlesheets)
library(dplyr)
library(networkD3)
library(tm)
library(memoise)

shinyServer(function(input, output, session) {
  
#   output$testtext <- renderText(paste("ip: ", input$ipid))
  
  ## Logs
  log_to_gsheets <- function(verb, object) {
    log_ss %>% gs_add_row(input = c(input$ipid, as.character(Sys.time()), 
                                    input$section, input$userId, verb, object))
  }
  
  ## JS to enable mouse click on network nodes
  onClickScript <- 'alert("This is " + d.name + " who has posted " +
       ((d3.select(this).select("circle").attr("r")-3)^2/4*Math.PI) +  " notes in our class");'
  
  ## Load data when clicking on button
  get_gdoc <- eventReactive(input$update, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
    ss <- googlesheets::gs_key(input$section)
    log_to_gsheets("opens", "app")
    gdoc = gs_read(ss)
    gdoc$created_at = as.Date(gdoc$created_at, "%m/%d/%Y")
    gdoc %>% filter(created_at >= input$dateRange[1] & created_at <= input$dateRange[2] )
  })
  
  ## Build network when clicking on button
  force_network <- eventReactive(input$update, {
    ## get nodes
    #  source nodes
    gdoc = get_gdoc()
    nodes = select(gdoc, vert1_name, vert1_id) %>%
      group_by(vert1_id) %>%
      dplyr::summarise(size = n()) %>%
      mutate(group = "Student")
    names(nodes) = c("name", "size", "group")
    
    # target nodes
    targets = select(gdoc, vert1_id, vert2_id) %>%
      filter(!(vert2_id %in% vert1_id)) %>%
      group_by(vert2_id) %>%
      dplyr::summarise(size = n()) %>%
      select(vert2_id) %>%
      mutate(size = 0, group = "Student")
    names(targets) = c("name", "size", "group")
    
    # combine them together
    nodes = rbind(nodes, targets) %>% arrange(name)
    nodes$group[which(nodes$name == 4629181)] = "Teacher"
    nodes$group[which(nodes$name == input$userId)] = "You"
    nodes$ID = seq(0, nrow(nodes)-1)
    
    ## get edges
    edges <- CreateSNADataFrame(gdoc, from="vert1_id", to="vert2_id", 
                                linkNames="reply")
    names(edges) = c("source", "target", "value")
    # get IDs (could get names instead)
    edges = left_join(edges, select(nodes, name, ID), by = c("source" = "name")) %>%
      left_join(select(nodes, name, ID), by = c("target" = "name"))
    
    forceNetwork(Links = data.frame(edges), Nodes = data.frame(nodes), 
                 Source = "ID.x", Target = "ID.y", Group = "group", 
                 Value = "value", Nodesize = "size", NodeID = "name", 
                 radiusCalculation = JS(" Math.sqrt(d.nodesize*4/Math.PI) + 3"), 
                 linkWidth = JS("function(d) { return Math.sqrt(d.value)-0.5; }"),
                 opacity = 0.8, charge = -1000, legend = TRUE,
                 fontFamily = "Georgia, serif", clickAction = onClickScript)
  })
  
  ## Show force directed layout
  output$force <- renderForceNetwork({
    log_to_gsheets("views", "network")
    force_network()
  })
  
  # Using "memoise" to automatically cache the results
  getTermMatrix <- memoise(function(book) {
    
    gdoc = get_gdoc()
    text = paste(gdoc$message_text, collapse = ' ')
    
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "dont", stops))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  })
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    #     input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        #         getTermMatrix(input$selection)
        getTermMatrix()
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4, 0.5),
                  min.freq = input$freq, max.words=input$max,
                  rot.per = 0, colors=brewer.pal(8, "Dark2"))
  })
  
  output$d3Plot <- renderd3Cloud({
    log_to_gsheets("views", "cloud")
    v <- head(terms(), input$max)
    v <- v[v >= input$freq]
    d3Cloud(text = names(v), size = v)
  })
  output$wordCount <- renderText({
    v <- terms()
    if(!any(names(input)=='d3word')) return ("Click on a word for count")
    else log_to_gsheets("clicks", input$d3word)
    paste(input$d3word, "-", v[input$d3word])
  })
})
