require(shiny)
require(googlesheets)
require(dplyr)
require(tm)
require(visNetwork)
require(igraph)
# require(rWordCloud)
require(shinyjs)

## Slice igraph networks by # days
## Return a list: 1) date brackets; 2) igraph objects
slice_igraph_network <- function(g, byDays = 7) {
  
  edges = as_data_frame(g, 'edges')
  dates = seq(min(edges$created_at), max(edges$created_at), byDays)
  brackets = sapply(dates, function(x) {
    paste(as.Date(x, origin = "1970-01-01"), "~", 
          as.Date(x + byDays - 1, origin = "1970-01-01"))
  })
  graphs = lapply(dates, function(start) {
    end = start + byDays - 1
    g - E(g)[created_at < start | created_at >= end]
  })
  list(brackets, graphs)
}


shinyServer(function(input, output, session) {
  
  ## Logs
  log_to_gsheets <- function(verb, object) {
    log_ss %>% gs_add_row(input = c(input$ipid, as.character(Sys.time()), 
                                    input$section, input$userId, verb, object))
  }
  
  ## Read data from GDrive
  read_from_gdoc <- eventReactive(input$section, {
    # only run this when input$section has changed
    # not every time when input$update is clicked on
    isolate(
      withProgress({
        setProgress(message = "Loading Canvas data...") # set progress
        
        # read selected section's data
        ss <- googlesheets::gs_key(input$section)
        log_to_gsheets("launches", "app")
        gdoc = gs_read(ss)
        
        # format data
        gdoc$created_at = as.Date(gdoc$created_at, "%m/%d/%Y")
        gdoc$vert1_id = as.integer(gdoc$vert1_id)
        gdoc$vert2_id = as.integer(gdoc$vert2_id)
        
        gdoc
      })
    )
  })
  
  ## (Re-)Load data when clicking on "Update" button 
  get_gdoc <- reactive({
    # filter data by date range
    read_from_gdoc() %>% filter(created_at >= input$dateRange[1] & 
                                  created_at <= input$dateRange[2])
  })
  
  ## Create SNA graph
  get_graph <- reactive({
    # get tabular data
    gdoc = get_gdoc() %>%
      select(vert1_id, vert2_id, topic_id, created_at, message_id)
    
    # gdoc$vert1_id_old = as.integer(gdoc$vert1_id)
    # gdoc$vert2_id_old = as.integer(gdoc$vert2_id)
    # 
    # dict = data.frame(id = unique(c(gdoc$vert1_id, gdoc$vert2_id)))
    # dict$key = 1:nrow(dict)
    # 
    # require(plyr)
    # gdoc$vert1_id <- mapvalues(gdoc$vert1_id_old, from=dict$id, to=dict$key)
    # gdoc$vert2_id <- mapvalues(gdoc$vert2_id_old, from=dict$id, to=dict$key)
    
    # whether to hide teacher from the network
    if(input$hideTeacher)
      gdoc = gdoc %>% filter(vert1_id != teacher_id & vert2_id != teacher_id)
    
    g = graph_from_data_frame(gdoc, directed = TRUE)
    g %>% 
      set_vertex_attr("value", value = igraph::degree(g, mode="out")) %>% 
      set_vertex_attr("group", value = sapply(V(g)$name, function(x) {
        if(x == teacher_id) return("T")
        if(x == input$userId) return("U")
        "S"
      }))
  })
  
  ## Build network when clicking on button
  force_network <- reactive({
    
    # get the igraph object
    g = get_graph()
    
    # get nodes and edges
    nodes = as_data_frame(g, 'vertices')
    nodes$label = 1:nrow(nodes)
    nodes$id = nodes$name
    nodes$title = paste0("<p><b>Id:</b> ", nodes$label,"<br><b>Replies:</b> ", nodes$value, "</p>")
    edges = as_data_frame(g, 'edges') %>%
      dplyr::group_by(from, to) %>%
      dplyr::summarise(value = n())
    
    # plot visNetwork
    visNetwork(nodes = nodes, edges = edges) %>%
      visGroups(groupname = "S", color = "#abdda4") %>%
      visGroups(groupname = "U", color = "#d7191c") %>%
      visGroups(groupname = "T", color = "#fdae61") %>%
      visLegend(enabled = TRUE) %>%
      visNodes(scaling = list(min=5, max=25), id = NULL) %>%
      visEdges(arrows =list(
        to = list(enabled = TRUE, scaleFactor = .5)), 
        scaling = list(min=0.1, max=3), 
        color = list(color='gray', highlight='red', opacity=.25),
        smooth = list(enabled=TRUE, type="curvedCW")) %>%
      visOptions(width = "100%", height = 500,
                 nodesIdSelection = TRUE, 
                 highlightNearest = TRUE) %>%
      visPhysics(
        stabilization = list(enabled=TRUE, iterations=100),
        barnesHut = list(
          enabled = TRUE,
          gravitationalConstant = -250,
          centralGravity = 0.5,
          springLength = 150,
          springConstant = 0.032,
          damping = 0.235
        ))
  })
  
  ## Show force directed layout
  output$force <- renderVisNetwork({
    # Take a dependency on input$update
    input$update
    
    isolate(log_to_gsheets("views", "network"))
    isolate(force_network())
  })
  
  output$test <- renderText({
    paste(input$network_selected, input$network_selectedBy)
  })
  
  ## network metrics
  output$networkMertrics <- renderUI({
    # Take a dependency on input$update
    input$update
    
    withProgress({
      setProgress(message = "Computing network measures...") # set progress
      
      g <- get_graph()
      isolate(log_to_gsheets("views", "network metrics"))
      tags$div(
        tags$p(paste0(length(V(g)), " participants have made ", 
                      length(E(g)), " links so far, with a density of ",
                      round(graph.density(g) * 100, 1), "%."))
      )
    })
  })
  
  output$personalMetrics <- renderTable({
    # Take a dependency on input$update
    input$update
    
    g <- get_graph()
    
    isolate(
      if(!is.null(input$userId) & input$userId %in% V(g)$name) {
        isolate(log_to_gsheets("views", "personal metrics"))
        id = V(g)$name
        idx = which(id == input$userId)
        indegree = igraph::degree(g, mode = 'in')
        outdegree = igraph::degree(g, mode = 'out')
        df = data.frame(c(outdegree[idx], mean(outdegree), max(outdegree)),
                        c(indegree[idx], mean(indegree), max(indegree)),
                        row.names = c("Your Score", 'Class Average', 'Class Maximum'))
        names(df) = c("Replies", "Being replied")
        df
      }
    )
  }, digits = 0)
  
  output$personalChange <- renderTable({
    # Take a dependency on input$update
    input$update
    
    g <- get_graph()
    
    isolate(
      if(!is.null(input$userId) & input$userId %in% V(g)$name) {
        graph_slices = slice_igraph_network(g)
        
        df = data.frame(t(sapply(graph_slices[[2]], function(gi){
          id = V(gi)$name
          idx = which(id == input$userId)
          c(igraph::degree(gi, mode = 'out')[idx], 
            igraph::degree(gi, mode = 'in')[idx])
        })), row.names = graph_slices[[1]])
        names(df) = c("Replies", "Being replied")
        df
      }
    )
  }, digits = 0)
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    #     input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        gdoc = get_gdoc()
        text = paste(gdoc$message_text, collapse = ' ')
        
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, content_transformer(tolower))
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,
                          c(stopwords("SMART"), stops))
        
        myDTM = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 1))
        
        m = as.matrix(myDTM)
        sort(rowSums(m), decreasing = TRUE)
      })
    })
  })
  
  get_user_posts <- reactive({
    get_gdoc() %>%
      filter(vert1_id == input$userId)
  })
  
  ## Plot wordcloud
  # output$d3Plot <- renderd3Cloud({
  #   log_to_gsheets("views", "cloud")
  #   v <- head(terms(), input$max)
  #   v <- v[v >= input$freq]
  #   d3Cloud(text = names(v), size = v)
  # })
  
  output$termCoverage <- renderText({
    
    if(input$userId == "")
      return(NULL)
    v <- head(terms(), input$max)
    v <- v[v >= input$freq]
    freqTerms = names(v)
    posts = get_user_posts()$message_text
    
    covered = sapply(freqTerms, function(t) {
      sum(grepl(t, posts, ignore.case = TRUE)) > 0
    })
    toCover = paste(freqTerms[!covered], collapse = ",  ")
    if(nchar(toCover) == 0)
      return("Congrats! You covered all these terms.")
    else
      return(paste0("<br>Hey! You've coverted:  ", paste(freqTerms[covered], collapse = ",  "),
                    "<br>More terms for you to think about:  <b>", toCover, "</b>"))
  })
  
  ## Log mouseclicks on terms
  output$wordCount <- renderText({
    v <- terms()
    if(!any(names(input)=='d3word')) return ("Click on a word for count")
    else isolate( log_to_gsheets("clicks word", input$d3word) )
    paste(input$d3word, "-", v[input$d3word])
  })
  
  ## About panel
  output$about <- renderUI({
    isolate(log_to_gsheets("views", "about"))
    tags$div(
      tags$h4('Built by the CI4311W team, with ♥'),
      tags$p("Communication and teamwork are essential skills for your academic and (future) professional life. 
             In CI4311W, these skills are reflected in your participation in discussion forums. 
             This app is to help you assess and monitor your progress, and to set personal goals for participation."),
      tags$p('Below are some helpful tips:'),
      tags$ul(
        tags$li('Use this app for personal reflection, rather than peer comparison'),
        tags$li("'Chat' with your teacher or peers about your thoughts on your results"),
        tags$li('Type in your Canvas id to see your personal results'),
        tags$li("In the network graph, a bigger size means more 'replies' to others"),
        tags$li('Click on a node to see its connections'),
        tags$li("In the word cloud, a bigger size means higher frequency in class discussion"),
        tags$li("Think about words you've NOT engaged with yet"),
        tags$li('Track your progress in different weeks, by tinkering with different date ranges'),
        tags$li('Check back to the app every week...')),
      tags$p('Have questions? Contact Prof. Bodong Chen: chenbd.umn.edu')
    )
  })
})
