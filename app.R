#############################################################################
## Shiny App with Reactive URL Input to get target drugs for query gene id ##
#############################################################################
## Author: Yuzhu Duan & Thomas Girke
## Last update: 06-Jul-17

## Usage: save this file under name app.R to your shiny app directory, e.g. geneTargetAnno
## Run in parent directory the following from within R:
## > library(shiny)
## > runApp(geneTargetAnno)

library(shiny); library(tidyverse); library(RSQLite); library(DT)
source("function.R")

## User interface
ui <- function(request){
  fluidPage(
    titlePanel(strong("Drugs and Target Proteins"), windowTitle = "Drugs and Target Proteins"),
    sidebarPanel(
      textInput("symbol", "Ensembl Gene ID"),
      actionButton(inputId = "go", label = "Submit"),
      width = 2
    ),
    mainPanel(
      tabsetPanel(id="database",
        tabPanel("DrugBank", 
                 h3(uiOutput("tab_title")),
                 dataTableOutput('mytabular')),
        tabPanel("STITCH", 
                 h3(uiOutput("tab_title_sti")),
                 dataTableOutput('mytabular_sti'))
      ),
      width = 10
    )
  
  )
}

## Server instructions
server <- function(input, output, session) {
  setBookmarkExclude(c("mytabular_cell_clicked", "mytabular_rows_all", "mytabular_rows_current", "mytabular_rows_selected", "mytabular_search", 
                       "mytabular_search_columns", "mytabular_state", "go", "mytabular_sti_cell_clicked", "mytabular_sti_rows_all", "mytabular_sti_rows_current", 
                       "mytabular_sti_rows_selected", "mytabular_sti_search", "mytabular_sti_search_columns", "mytabular_sti_state"))

  observeEvent(input$go, {
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  observeEvent(input$go, {
    valuetoupdate <- input$symbol
    result <- db_table_render(valuetoupdate)
    result_sti <- stitch_table_render(valuetoupdate)
    if(! is.null(result)){
      output$tab_title <- renderText(result$title)
      output$mytabular <- DT::renderDataTable({
        result$table
      }, options = list(lengthMenu = c(5, 50, 500), pageLength = 8, dom = 'lfrtipB', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
      filter = 'top', escape = FALSE, extensions = 'Buttons')
    }
    if(! is.null(result_sti)){
      output$tab_title_sti <- renderText(result_sti$title)
      output$mytabular_sti <- DT::renderDataTable({
        result_sti$table
      }, options = list(lengthMenu = c(5, 50, 500), pageLength = 8, dom = 'lfrtipB', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
      filter = 'top', escape = FALSE, extensions = 'Buttons')
    }
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    isolate({
      for (i in 1:(length(reactiveValuesToList(input)))) {
        nameval = names(reactiveValuesToList(input)[i])
        if (!is.null(query[[nameval]])) {
          valuetoupdate = rev(unlist(strsplit(query[[nameval]], "\"")))[1]
          updateTextInput(session, nameval, value = valuetoupdate)
        } else {
          valuetoupdate <- NULL
        }
      }
      if(!is.null(query$tab)){
        tab = unlist(strsplit(query[["tab"]], "\""))
        updateTabsetPanel(session, "database", selected = tab)
      }
    })
    result <- db_table_render(valuetoupdate)
    result_sti <- stitch_table_render(valuetoupdate)
    
    if(! is.null(result)){
      output$tab_title <- renderText(result$title)
      output$mytabular <- DT::renderDataTable({
        result$table
      }, options = list(lengthMenu = c(5, 50, 500), pageLength = 8, dom = 'lfrtipB', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
      filter = 'top', escape = FALSE, extensions = 'Buttons')
    }
    
    if(! is.null(result_sti)){
      output$tab_title_sti <- renderText(result_sti$title)
      output$mytabular_sti <- DT::renderDataTable({
        result_sti$table
      }, options = list(lengthMenu = c(5, 50, 500), pageLength = 8, dom = 'lfrtipB', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
      filter = 'top', escape = FALSE, extensions = 'Buttons')
    }
  })
}

## Run shiny app 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
