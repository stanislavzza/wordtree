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

library(shiny)
library(binom)
library(dplyr)
library(tidyr)
library(igraph)
library(ggvis)

library(wordVectors) #http://bookworm.benschmidt.org/posts/2015-10-25-Word-Embeddings.html
# https://github.com/bmschmidt/wordVectors
library(stringr)
library(tsne)
library(tm)
library(readr)

shinyUI( #fluidPage(
  pageWithSidebar(
  #theme = "bootstrap.css",
  headerPanel(""),
  sidebarPanel(
    h3("WordTree"),
    
    actionButton("console","server console"), # uncomment this to enable debugging <------------
    
    ########### Loading Data ##################################
    
    helpText(a("[See User Guide]",href="http://highered.blogspot.com/2014/07/survey-prospector.html")),
    helpText("Send feedback to deubanks.office@gmail.com"),
    htmlOutput("indexFileUploader"),  # upload optional index files
    htmlOutput("dataFileUploader"),   # upload data files
   
    ########### Analysis of Data ##############################
        
    htmlOutput("targetVar"),         # select the variable to predict
    htmlOutput("action")
  ), #end of sidebar panel
  mainPanel(
    # turn off the messages. They still appear in the console. 
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
          tabsetPanel("Main",
            tabPanel("Frequencies",
                dataTableOutput("wordStats")
            ),
            
            tabPanel("Neighbors",
                tableOutput("indexInfo")     
            ), # end of Data Tabpanel
            

            
            tabPanel("Quotes",
                     tableOutput("quotes")     
            ), # end of Data Tabpanel
            
            tabPanel("Network",
                     fluidRow(
                       column(4,htmlOutput('numberPredictors')),
                       column(4,htmlOutput('networkButton')),
                       column(4,htmlOutput('numberSelector'))
                     ),
                     fluidRow(
                       column(4,htmlOutput('maxAUC')),
                       column(4,htmlOutput('forceLayout')),
                       column(4,htmlOutput('maxCor'))
                     ),
                     fluidRow(
                       column(12,ggvisOutput('ggvis_networkResults'))
                     )
            )
          ) # end of tabsetPanel

  ) # end of main panel
) # end of page with sidebar
) # end of ShinyUI