#############################################################################
## Shiny App with Reactive URL Input to get target drugs for query gene id ##
#############################################################################
## Author: Yuzhu Duan & Thomas Girke
## Last update: 06-Jul-17

## Usage: save this file under name app.R to your shiny app directory, e.g. geneTargetAnno
## Run in parent directory the following from within R:
## > library(shiny)
## > runApp(geneTargetAnno)

## The toy database df1 in this example is created on the fly
## Example URL for testing: 127.0.0.1:5726/?symbol=g7
## Note, the value 5726 in URL might need to be changed depending on your session
## Query IDs in the URL can be g1 - g10

library(shiny); library(tidyverse); library(RSQLite); library(DT)

## User interface
ui <- fluidPage(
    titlePanel("Query table/db by key value"),
    sidebarPanel(
        textInput("symbol", "Gene Ensembl ID", ""),
        bookmarkButton(),
        width = 2
        ),
    mainPanel(
        h2(uiOutput("tab_title")),
        dataTableOutput('mytabular'),
        h2(uiOutput("invalid_ensb"))
    )
)

## Server instructions
server <- function(input, output, session) {
  
  mydb <- dbConnect(SQLite(), "data/geneTargetAnno.db")
  valid_ensb <- unique(dbGetQuery(mydb, 'SELECT ensemble_id FROM ensb_drugs')[[1]]) # 2219
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    isolate(
      for (i in 1:(length(reactiveValuesToList(input)))) {
        nameval = names(reactiveValuesToList(input)[i])
        valuetoupdate = query[[nameval]]
        if (!is.null(query[[nameval]])) {
          if (is.na(as.character(valuetoupdate))) {
            updateTextInput(session, nameval, value = valuetoupdate)
          }
          else {
            updateTextInput(session, nameval, value = as.character(valuetoupdate))
          }
        }
      }
    )
    
    if(! is.null(valuetoupdate)){
      if(valuetoupdate %in% valid_ensb){
        query_uni_id <- as.character(dbGetQuery(mydb, paste0('SELECT uniprot_id FROM ensb_uniprot_map WHERE ensemble_id = "', valuetoupdate, '"')))
        query_uni_id_href <- paste0("<a href='http://www.uniprot.org/uniprot/", query_uni_id, "' target='_blank'>", query_uni_id, "</a>")
        target_drugs <- dbGetQuery(mydb, paste0('SELECT target_drugs FROM ensb_drugs WHERE ensemble_id = "', valuetoupdate, '"'))[[1]]
        target_drugs_href <- paste0("<a href='https://www.drugbank.ca/drugs/", target_drugs, "' target='_blank'>", target_drugs, "</a>")
        str_href <- paste0("<img src='https://www.drugbank.ca/structures/", target_drugs, "/image.svg' height=200 width=200 target='_blank'>", "</img>")
        drug_targets <- NULL
        for(i in 1:length(target_drugs)){
          tmp <- dbGetQuery(mydb, paste0('SELECT uniprot_id FROM drug_targets_sub WHERE target_drugs = "', as.character(target_drugs[i]), '"'))
          uniprot_id_href <- paste0("<a href='http://www.uniprot.org/uniprot/", tmp$uniprot_id, "' target='_blank'>", tmp$uniprot_id, "</a>")
          for(i in seq_along(tmp$uniprot_id)){
            org <- paste0(as.character(dbGetQuery(mydb, paste0('SELECT Organism FROM uniprot_org WHERE uniprot = "', tmp$uniprot_id[i], '"'))), collapse = " | ")
            tmp$uniprot_id[i] <- paste(paste(uniprot_id_href[i], org, sep = " [ "), "]")
          }
          tmp2 <- paste(tmp$uniprot_id, collapse = "\t")
          drug_targets <- c(drug_targets, tmp2)
        }
        dbDisconnect(mydb)
        result <- data_frame(Uniprot_id = query_uni_id_href, target_drugs = target_drugs_href, structure = str_href, drug_targets = drug_targets)
        valuetoupdate_href <- paste0("<a href='http://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", valuetoupdate, "' target='_blank'>", valuetoupdate, "</a>")
        output$tab_title <- renderText(paste0("Target drugs for ", valuetoupdate_href, " and their protein targets")) 
        output$mytabular <- DT::renderDataTable({
              result
          }, options = list(lengthMenu = c(5, 50, 500), pageLength = 8, dom = 'lfrtipB', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), filter = 'top', escape = FALSE, extensions = 'Buttons')
      } else {
        valuetoupdate_href <- paste0("<a href='http://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", valuetoupdate, "' target='_blank'>", valuetoupdate, "</a>")
        query_uni_id <- as.character(dbGetQuery(mydb, paste0('SELECT uniprot_id FROM ensb_uniprot_map WHERE ensemble_id = "', valuetoupdate, '"')))
        query_uni_id_href <- paste0("<a href='http://www.uniprot.org/uniprot/", query_uni_id, "' target='_blank'>", query_uni_id, "</a>")
        output$invalid_ensb <- renderText(paste0("No target drugs for ", valuetoupdate_href, " (", query_uni_id_href, ")", " in DrugBank database"))
      }
    }
  })
}

enableBookmarking(store = "url")
## Run shiny app 
shinyApp(ui = ui, server = server)
