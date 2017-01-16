#This file is part of WordTree  

#WordTree is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#WordTree is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with WordTree.  If not, see <http://www.gnu.org/licenses/>.

options(shiny.maxRequestSize = 200*1024^2) # allow large files
#outputOptions(output, "downloadLog", suspendWhenHidden=FALSE)
#https://groups.google.com/forum/#!topic/shiny-discuss/TWikVyknHYA



corpus <- ""
term_count <- NULL
model <- NULL

denull <- function(x,y=0){
  if (is.null(x)) return(y)
  return(x)
}

# this function needs the global index
get_description <- function(x) {
  if(is.null(x$var1)) return(NULL)
  #paste0(names(x), ": ", format(x), collapse = " ")
  paste(x$var1,term_count$n_count[term_count$word == x$var1])
  #paste0(str(varName))
}

wv <- function(word){
  #if (str_length(word) > 5) {return("here")}
  if (is.na(model[[word]][1,1])) { #nothing there
    return( paste("Too little data:", term_count$n_count[term_count$word == word], "uses of this term."  ))
  }
  w <- round(nearest_to(model,model[[word]], n = 20),3)
  t <- as.data.frame(w)
  names(t) <- c("dist")
  t$word <- names(w)
  counts <- NULL
  for( w in row.names(t)){
    if (length(term_count$n_count[term_count$word == w]) == 0  ) {
      counts <- c(counts, 0)
    } else {  
      counts <- c(counts, term_count$n_count[term_count$word == w])
    }
  }
  t$N <- as.character(counts)
  #print(t)
  
  return(t)
  # now find some context snips
  index <- unlist(str_locate_all(corpus,paste0(" ",word," ")))
  index <- sample(index, min(10,length(index)))
  for( i in index){
    print(substr(corpus,i-50,i+50))
  }
}

quote <- function(word){
  #find some context snips
  df <- data.frame(quote = "", stringsAsFactors = F)
  index <- unlist(str_locate_all(corpus,paste0(" ",word," ")))
  index <- sample(index, min(20,length(index)))
  for( i in index){
    df <- rbind(df, substr(corpus,i-50,i+50))
  }
  return(df)
}

resolve <- function(str){
  # to star matching too!
  facts <- unlist(str_split(str,'&'))
  
  if(length(facts) == 1) {
    return(get_vec(facts))
  }
  
  count <- 0
  vector <- 0
  for( f in facts){
    if (str_length(f)<3) {next} # need at least three letters
    newvec <- get_vec(f)
    if (!is.null(newvec)){
      vector <- vector + newvec
      count <- count + 1
    }
  }
  if (count == 0) { return(0) }
  return(vector/count)
}

get_vec <- function(word) { # also pattern matches, using a trailing underscore
  
  pos = regexpr('_', word) # check for an asterisk
  # at the moment, only support asterisk at the end
  if (pos[1] == -1 ) {          # no match
    if (is.na(model[[word]][1,1])) { return(NULL)}
    return(model[[word]]) 
  }
  wn <- row.names(model@.Data)
  target <- paste0('^',substr(word,1,pos[1]-1))
  matched <- wn[ grepl( target, wn) ]
  
  if(length(matched)== 0) { return(NULL)}
  sum <- 0
  for(w in matched){
    sum <- sum + model[[w]]
  }
  sum <- sum / length(matched)
  return(sum)
}

create_vec <- function(name, vector){
  model@.Data <<- rbind(model@.Data,vector) 
  row.names(model@.Data)[nrow(model@.Data)] <<- name
  
  # add the cached magnitude
  if(!is.null(model@.cache$magnitudes)) {
    model@.cache$magnitudes[nrow(model@.Data)] <<- rowSums(vector^2)
    names(model@.cache$magnitudes)[nrow(model@.Data)] <<- name
  }
}

my_parse <- function(str) {
  str <- gsub(" +","",str)
  
  if (!is.na(model[[str]][1,1])){
    return(str) # it may already exist
  }
  
  lr <- unlist(str_split(str,'='))
  
  if (length(lr) == 2) { # name change
    name <- lr[1]
    str <- lr[2]
  } else {
    name <- str
  }
  
  terms <- unlist(str_split(str,'[+-]'))
  ops <- str_extract_all(str, "[+-]")
  
  vector <- resolve(terms[1])
  if (length(terms) > 1) {
    op_count <- 1
    for(t in terms[2:length(terms)]){ # resolve the averages
      if (ops[op_count] == "+") {
        vector <- vector + resolve(t) 
      } else {
        vector <- vector - resolve(t) 
      }
      op_count <- op_count + 1
    }
  }
  
  create_vec(name, vector)
  return(name)
}


#######################################################################################
#                                    SERVER CODE                                      #
#######################################################################################

shinyServer(function(input, output, session) {
  
    #REACTIVE VALUES-------------------------------------------------------------------
    rvals <-reactiveValues(dataDoneButton=FALSE,dataLoaded=FALSE,refreshData=FALSE,
                           updateDataInfo = 0,interestCB=0,logitModel=0,newVarExists=0,
                           graphUpdate=c(0,0),graphAUC=c(0,0),graphReady=0)
  
    ############################### FILE UPLOADS #####################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
       
    #INPUT------------- Select index file for upload 

    output$indexFileUploader <- renderUI({
      if(rvals$dataLoaded) return("")
  
        fileInput('indexFile', 'Choose Text Corpus',
            accept=c('text/plain', 'text/plain', '.txt'))
    })
     
    #OBSERVER---------- Process index data after upload
    loadIndex <- observe({
    
      inFile <- input$indexFile
      if (is.null(inFile)) return("")
      
      corpus <<- read_file(inFile$datapath)
      
      corpus <<- str_replace_all(corpus,"[^[:graph:]]", " ")  # get rid of bad chars
      
      corp <- VCorpus(VectorSource(corpus))
      
      dtm <- DocumentTermMatrix(corp,control = list(removePunctuation = TRUE))
      
      term_count <<- data.frame(word = dtm$dimnames$Terms, n_count = dtm$v)

    })
  
    #INPUT------------- Select data file for upload -----------------------
    output$dataFileUploader <- renderUI({
      if(rvals$dataLoaded) return("")
      
      
      fileInput('dataFile', 'Choose Word Vector File (.bin)',
                accept=c('application/x-binary', 'application/x-binary', '.bin'))
    })
    
    
    #OBSERVER----------- Process data file -------------------------------------
    loadDataFile <- observe({ 
      if (is.null(input$dataFile)) return("")
      model <<- read.vectors(input$dataFile$datapath, binary=T)
      
      # set up caching
      if (!.hasSlot(model, ".cache")) {
        #model@.cache <<- new.env()
        attributes(model)$.cache <<- new.env()
        #attr(model,'.cache') <<- new.env()
      }
      if (is.null(model@.cache$magnitudes)) {
          model@.cache$magnitudes <<- rowSums(model^2)
      }
      
      isolate(rvals$dataLoaded <- 1)
      })
    
    
    #--------------------------------------------------------------------------------#
    ############################### END FILE UPLOADS #################################
  
    ############################### DATA PANEL  ######################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    
  
    
################################ Word Retrieval ###################################

    #INPUT--------------------- Select the Target Word to predict-----------------------
    output$targetVar <- renderUI({ 
      # turning off the newvars update for now--it's quite annoying
      #rvals$newVarExists # update the list of names if a new var is created
      if (!rvals$dataLoaded) return("")
      textInput("targetVar", "Which word to inspect?" )
    })
    

    #INPUT -------------------- Button to update predictors-----------------------------
    output$action <- renderUI({
      if (!rvals$dataLoaded) return("")
      actionButton("action","Go")
    })
   
    
    #--------------------------------------------------------------------------------#
    ############################ END ANALYSIS CONTROLS ###############################
    
    ############################### Summary TAB ####################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    #OUTPUT------------------------Display word frequencies
    output$wordStats <- renderDataTable({
      rvals$dataLoaded # reactive
      term_count
    })
    
    
    #OUTPUT----------------------- Display Vars information
    output$indexInfo <- renderTable({
      input$action # trigger
      word <- isolate(input$targetVar)
      if (is.null(word)){ return("")}
      if (word == "") {return("")}
      word <- my_parse(word)
      wv(word)
    }, 
    include.rownames=FALSE)
    
    #OUTPUT----------------------- Display Vars information
    output$quotes <- renderTable({
      input$action # trigger
      word <- isolate(input$targetVar)
      if (is.null(word)){ return("")}
      if (word == "") {return("")}
      word <- my_parse(word)
      quote(word)
    })

    

#--------------------------------------------------------------------------------#
############################ END PREDICTOR PANEL #################################


  ################################## Network Tab ##################################################
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  #INPUT------------------------------slider to set cut-off for predictor model ------------------------
  output$numberSelector <- renderUI({
    sliderInput("numberSelector", label = strong("How close?"),
                min = 0, max = 100, value = c(1,31), step = 1,ticks=TRUE)
  })
  
  #INPUT------------------------------slider to set cut-off for num predictors ------------------------
  output$numberPredictors <- renderUI({
    sliderInput("numberPredictors", label = strong("How many words?"),
                min = 1, max = 50, value = 10, step = 1,ticks=TRUE)
  })

  #INPUT------------------------ Button to generate network anew --------------------------
  output$networkButton <- renderUI({
    actionButton("networkButton","Generate Network")
  })

  #OUTPUT------------------------ Text to tell us how close the words are----------------------
  output$maxCor <- renderUI({
    val = rvals$graphUpdate # set dependency, brings back the min correlation
    paste0(round(val[1],3)," >= |similarity| >= ",round(val[2],3))
  })


  #INPUT------------------- Checkbox for fixed layout -----------------------------------
  output$forceLayout <- renderUI({
   
    checkboxInput("forceLayout", label = "Fixed Layout", value = FALSE)  
    
  })

  #REACTIVE ---------------------- Generate network graph---------------------------------------------
  networkObserver <- observeEvent(input$networkButton,{
    #input$action # trigger
    word <- isolate(input$targetVar) # must hit button to update
    word <- my_parse(word)
    num_words <- isolate(input$numberPredictors)
    
    if (is.na(model[[word]][1,1])) { #nothing there
      return("")
    }
 #     if(denull(input$networkButton[1],0)==0) return()  # dependency on the incrementing of the regression button clicks
      lower <- denull(isolate(input$numberPredictors[1]),1)
      upper <- denull(isolate(input$numberPredictors[2]),16)
      

        correlation <- NULL
        n <- NULL
        status <- NULL
        description <- NULL
        jointAUC <- NULL
      
        
        
        # generate similarity matrix ##########################
        n <- nearest_to(model,model[[word]],n = num_words) %>% names
        m <- model[[n, average = F]] # without the flag, it averages the vectors together
        
        ct <- cosineSimilarity(m,m)
        
        ct[upper.tri(ct,diag=TRUE)] <- 0
        
        # convert row names to a column
        ct<-data.frame(rownames(ct),ct)
        colnames(ct)[1]="var1"
        
        v <<- ct %>% gather(var2,correlation,-var1) %>% filter(correlation != 0) 
      
        v$color <<- 'red'  #rgb(1,0,0,abs(v$correlation[v$correlation <0])) #"red"
        v$color[v$correlation > 0] <<- 'black' # rgb(0,0,0,abs(v$correlation[v$correlation >0])) #"black"
        v$correlation <<- abs(v$correlation)
      
        g <<- graph.data.frame(v, directed=FALSE, vertices=NULL)
        graph_layout_fixed <<- layout.fruchterman.reingold(g,area=800*(vcount(g)^2))
        isolate(rvals$graphReady <- rvals$graphReady + 1)
        return(0)
  })

#output$ggvis_networkResults <- reactive({
graph_edges <- reactive({
  rvals$graphReady
  if(denull(isolate(input$networkButton[1]),0)==0) return(data.frame(var1='No Data',x=0,y=0,correlation=0,color='black',group=1))  # reactive on button
  nvert <- input$numberSelector[2] # reactive 
  clip <- input$numberSelector[1]
  # select top N vertices
  #top <- top_n(E(g)$correlation,nvert,correlation) # best improvers, E(g) brings back edges of g
  
  # http://igraph-help.nongnu.narkive.com/m0456GxD/threshold
  #top <- sort(abs(E(g)$correlation), decreasing=TRUE)
  #top <- top[clip:min(nvert,length(top))]
  top <- v %>% arrange(desc(correlation)) #sort(v$correlation, decreasing=TRUE)
  top <- top[clip:min(nvert,nrow(top)),]
  threshold <- min(top$correlation)
  up_threshold <- max(top$correlation)
  #g <- delete.edges(g,  c(which(E(g)$correlation < threshold),which(E(g)$correlation > up_threshold)))
                    #which(E(g)$correlation < threshold | E(g)$correlation > up_threshold)-1)
 # g <- delete.edges(g, which(E(g)$correlation > up_threshold)-1)
  #g <- remove.edge.attribute(g, "correlation")
  #g <- remove.edge.attribute(g, "color")
  #g <- delete.vertices(g,which(degree(g)<1))
 
  # now retrieve the names of the remaining ones and find the max/min AUC 
  vnames <- v$var1

  
  if (input$forceLayout == 1) {
    graph_layout <- graph_layout_fixed
  } else {
    g <- graph.data.frame(top, directed=FALSE, vertices=NULL)
    graph_layout <- layout.fruchterman.reingold(g,area=800*(vcount(g)^2)) # generates different graph each invocation
  }
  g$layout <<- 2*graph_layout # this is an easy way to spread it out
  
  rvals$graphUpdate = c(up_threshold,threshold) # reactive value to trigger text update
 
  # the following lines add the reverse directions and group each edge
  top$group <- 1:nrow(top)
  top2 <- top 
  colnames(top2) <- c("var2","var1","correlation","color","group")
  top2 <- top2[c("var1","var2","correlation","color","group")]
  top <- rbind(top,top2)
 
  top_names <- V(g)$name
  g_df <- data.frame(var1=top_names, x=graph_layout[,1],y=graph_layout[,2]) 
  
#  g_df <- g_df %>% left_join(term_count, by = c("var1" = "word")) # new ----------------
 # g_df$Size <- 15 #min(g_df$n_count,20) 
 
  #graph_edges <- 
  #return value 
  top %>% left_join(g_df,by="var1") #%>% left_join(term_count, by = c("var1" = "word")) # new ----------------
  #top$size <- min(top$n_count,20) 
  #top
  
  #plot(g, asp=0,vertex.label.dist=0,vertex.label.cex=1.5,vertex.color = get.edge.attribute(g,"color"),vertex.label.color = "black",vertex.size=1, edge.width=3)
})#, height = 1000, width = 1000)

graph_edges %>% ggvis(~x,~y) %>% layer_text(text := ~var1, dx:=0, dy :=0 , fontSize := 15) %>% # fontsize was 20
  group_by(group) %>% layer_lines( stroke := ~color, strokeWidth := ~correlation, opacity := ~correlation^2) %>% 
  hide_axis("x") %>% hide_axis("y") %>% add_tooltip(get_description, "hover") %>%  bind_shiny("ggvis_networkResults")


  
  #OBSERVE---------------------------------- debugging function -----------------------------------------
  observe(label="console",{
    if(denull(input$console) != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
  }
})

})


