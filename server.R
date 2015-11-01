require(shiny)
require(googlesheets)
require(dplyr)
require(networkD3)
require(tm)
require(visNetwork)
require(igraph)

# ## JS to enable mouse click on network nodes
# onClickScript <- 'alert("This is " + d.name + " who has posted " + 
# ((d3.select(this).select("circle").attr("r")-3)^2/4*Math.PI) +  
# " notes in our class");'

shinyServer(function(input, output, session) {
  
  ## Logs
  log_to_gsheets <- function(verb, object) {
    log_ss %>% gs_add_row(input = c(input$ipid, as.character(Sys.time()), 
                                    input$section, input$userId, verb, object))
  }
  
  ## (Re-)Load data when clicking on "Update" button
  get_gdoc <- eventReactive(input$update, {
    withProgress({
      setProgress(message = "Loading Canvas data...") # set progress
      
      # read selected section's data
      ss <- googlesheets::gs_key(input$section)
      log_to_gsheets("opens", "app")
      gdoc = gs_read(ss)
      
      # format data
      gdoc$created_at = as.Date(gdoc$created_at, "%m/%d/%Y")
      gdoc$vert1_id = as.integer(gdoc$vert1_id)
      gdoc$vert2_id = as.integer(gdoc$vert2_id)
      
      # filter data by date range
      gdoc %>% filter(created_at >= input$dateRange[1] & 
                        created_at <= input$dateRange[2])
    })
  })
  
  ## Given a vector of Canvas ids, return a vector of course roles (e.g., S, T, U)
  get_course_role <- function(v) {
    
  }
  
  ## Create SNA graph
  get_graph <- reactive({
    # get tabular data
    gdoc = get_gdoc() %>%
      select(vert1_id, vert2_id, topic_id, created_at, message_id)
    
    # whether to hide teacher from the network
    if(input$hideTeacher)
      gdoc = gdoc %>% filter(vert1_id != teacher_id & vert2_id != teacher_id)
    
    graph_from_data_frame(df, directed = TRUE) %>%
      set_vertex_attr("size", value = degree(g, mode="out")) %>%
      set_vertex_attr("group", value = sapply(get.vertex.attribute(g)$name, function(x) {
        if(x == teacher_id) return("T")
        if(x == input$userId) return("U")
        "S"
      }))
  })
  
  ## Build network when clicking on button
  force_network <- eventReactive(input$update, {
    
    ## get data
    gdoc = get_gdoc() %>% filter(!is.na(vert1_id) & !is.na(vert2_id))
    if(input$hideTeacher) { # whether to hide teacher from the network
      gdoc = gdoc %>% filter(vert1_id != teacher_id & vert2_id != teacher_id)
    }
    
    ## get nodes
    #  source nodes
    nodes = select(gdoc, vert1_name, vert1_id) %>%
      group_by(vert1_id) %>%
      dplyr::summarise(size = n()) %>%
      mutate(group = "S")
    names(nodes) = c("name", "size", "group")
    
    # target nodes
    targets = select(gdoc, vert1_id, vert2_id) %>%
      filter(!(vert2_id %in% vert1_id)) %>%
      group_by(vert2_id) %>%
      dplyr::summarise(size = n()) %>%
      select(vert2_id) %>%
      mutate(size = 0, group = "S")
    names(targets) = c("name", "size", "group")
    
    # combine sources and targets together
    nodes = rbind(nodes, targets) %>% arrange(name)
    # tag teacher
    if(!input$hideTeacher) {
      nodes$group[which(nodes$name == teacher_id)] = "T"
    }
    # tag current user
    nodes$group[which(nodes$name == input$userId)] = "U"
    nodes$group = factor(nodes$group, levels=c("S", "U", "T"))
    nodes$ID = seq(0, nrow(nodes)-1)
    
    ## get edges
    edges <- CreateSNADataFrame(gdoc, from="vert1_id", to="vert2_id", 
                                linkNames="reply")
    names(edges) = c("source", "target", "value")
    # get IDs (could get names instead)
    edges = left_join(edges, select(nodes, name, ID), by = c("source" = "name")) %>%
      left_join(select(nodes, name, ID), by = c("target" = "name"))
    
    nodes$id = nodes$ID
    nodes$label = nodes$name
    edges2 = edges
    edges2$from = edges2$ID.x; edges2$to = edges2$ID.y
    visNetwork(nodes, edges2, height = "500px", width = "100%") %>% 
      visOptions(nodesIdSelection = TRUE)
    
    nodes = get.data.frame(g, 'vertices')
    nodes$id = nodes$name
    edges = get.data.frame(g, 'edges')
    visNetwork(
      nodes  = nodes,
      edges  = edges,
      legend = TRUE) %>%
      visGroups(groupname = "S", color = "#d7191c") %>%
      visGroups(groupname = "U", color = "#abdda4") %>%
      visGroups(groupname = "T", color = "#fdae61") %>%
      visEdges(
        arrows =list(to = list(enabled = TRUE, scaleFactor = .5)),
        scaling=list(min=0.1, max=3),
        color = list(color='gray', opacity=.25),
        smooth=list(enabled=TRUE, type="curvedCW"))  %>%
      visOptions(
        width  = "100%",
        height = 500,
        # smoothCurves = TRUE,
        # stabilizationIterations = 100,
        nodesIdSelection = TRUE,
        highlightNearest = TRUE
      ) %>%
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
#     forceNetwork(Links = data.frame(edges), Nodes = data.frame(nodes), 
#                  Source = "ID.x", Target = "ID.y", Group = "group", 
#                  Value = "value", Nodesize = "size", NodeID = "name", 
#                  radiusCalculation = JS(" Math.sqrt(d.nodesize*4/Math.PI) + 3"), 
#                  linkWidth = JS("function(d) { return Math.sqrt(d.value)-0.5; }"),
#                  opacity = 0.8, charge = -500, legend = TRUE, colourScale = JS("d3.scale.category10()"),
#                  fontFamily = "Georgia, serif", clickAction = onClickScript)
  })
  
  
  
  ## Show force directed layout
  output$force <- renderVisNetwork({
    log_to_gsheets("views", "network")
    force_network()
  })
  
  # Define a reactive expression for the document term matrix
  terms <- eventReactive(input$update, {
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
                          c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "dont", stops))
        
        myDTM = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 1))
        
        m = as.matrix(myDTM)
        sort(rowSums(m), decreasing = TRUE)
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
    else isolate({ log_to_gsheets("clicks", input$d3word) })
    paste(input$d3word, "-", v[input$d3word])
  })
})
