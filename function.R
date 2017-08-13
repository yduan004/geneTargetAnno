db_table_render <- function(valuetoupdate){
  mydb <- dbConnect(SQLite(), "data/drugbank_dtmap.db")
  valid_ensb <- dbGetQuery(mydb, 'SELECT ensemble_id FROM ensb_uniprot')[[1]] # 2219
  if(! is.null(valuetoupdate)){
    if(valuetoupdate %in% valid_ensb){
      query_uni_id <- dbGetQuery(mydb, paste0('SELECT uniprot_id FROM ensb_uniprot WHERE ensemble_id = "', valuetoupdate, '"'))[[1]]
      query_uni_id_href <- paste0("<a href='http://www.uniprot.org/uniprot/", query_uni_id, "' target='_blank'>", query_uni_id, "</a>")
      target_drugs <- dbGetQuery(mydb, paste0('SELECT target_drugs FROM drug_targets_org WHERE uniprot_id = "', query_uni_id, '"'))[[1]]
      target_drugs_href <- paste0("<a href='https://www.drugbank.ca/drugs/", target_drugs, "' target='_blank'>", target_drugs, "</a>")
      str_href <- paste0("<img src='https://www.drugbank.ca/structures/", target_drugs, "/image.svg' height=200 width=200 target='_blank'>", "</img>")
      drug_targets <- NULL
      for(i in 1:length(target_drugs)){
        tmp <- dbGetQuery(mydb, paste0('SELECT uniprot_id FROM drug_targets_org WHERE target_drugs = "', as.character(target_drugs[i]), '"'))
        uniprot_id_href <- paste0("<a href='http://www.uniprot.org/uniprot/", tmp$uniprot_id, "' target='_blank'>", tmp$uniprot_id, "</a>")
        for(i in seq_along(tmp$uniprot_id)){
          org <- paste0(as.character(unique(dbGetQuery(mydb, paste0('SELECT Organism FROM drug_targets_org WHERE uniprot_id = "', tmp$uniprot_id[i], '"'))[[1]])), collapse = " | ")
          tmp$uniprot_id[i] <- paste(paste(uniprot_id_href[i], org, sep = " [ "), "]")
        }
        tmp2 <- paste(tmp$uniprot_id, collapse = "\t")
        drug_targets <- c(drug_targets, tmp2)
      }
      table <- data_frame(Uniprot_id = query_uni_id_href, target_drugs = target_drugs_href, structure = str_href, drug_targets = drug_targets)
      valuetoupdate_href <- paste0("<a href='http://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", valuetoupdate, "' target='_blank'>", valuetoupdate, "</a>")
      title <- paste0("Drugs and target proteins for ", valuetoupdate_href)
    } else {
      valuetoupdate_href <- paste0("<a href='http://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", valuetoupdate, "' target='_blank'>", valuetoupdate, "</a>")
      title <- paste0("No target drugs for ", valuetoupdate_href, " in DrugBank database")
      table <- NULL
    }
    dbDisconnect(mydb)
    result <- list()
    result$title <- title
    result$table <- table
    return(result)
  }
  dbDisconnect(mydb)
  return(NULL)
}

## For target_drugs in STITCH dataset, deliminate drugs that have more than 100 targets.
stitch_table_render <- function(valuetoupdate){
  mydb_sti <- dbConnect(SQLite(), "data/stitch_dtmap.db")
  valid_ensb_sti <- unique(dbGetQuery(mydb_sti, 'SELECT ensembl_gene_id FROM sti_dtlink')[[1]]) # 17910
  if(! is.null(valuetoupdate)){
    if(valuetoupdate %in% valid_ensb_sti){
      query_eprot_id <- unique(dbGetQuery(mydb_sti, paste0('SELECT ensembl_protein_id FROM sti_dtlink WHERE ensembl_gene_id = "', valuetoupdate, '"')))[[1]]
      query_eprot_id_href <- paste0("<a href='http://www.ensembl.org/Human/Search/Results?q=", query_eprot_id, ";site=ensembl;facet_species=Human' target='_blank'>", query_eprot_id, "</a>")
      table <- NULL
      for(i in 1:length(query_eprot_id)){
        target_drugs <- unique(dbGetQuery(mydb_sti, paste0('SELECT pubchem_cid FROM sti_dtlink WHERE ensembl_protein_id = "', query_eprot_id[i], '"'))[[1]])
        target_drugs_href <- paste0("<a href='https://pubchem.ncbi.nlm.nih.gov/compound/", target_drugs, "' target='_blank'>", target_drugs, "</a>")
        str_href <- paste0("<img src='https://pubchem.ncbi.nlm.nih.gov/image/imagefly.cgi?cid=", target_drugs, "&width=200&height=200' target='_blank'>", "</img>")
        drug_targets <- NULL
        for(j in 1:length(target_drugs)){
          tmp <- unique(dbGetQuery(mydb_sti, paste0('SELECT ensembl_protein_id FROM sti_dtlink WHERE pubchem_cid = "', as.character(target_drugs[j]), '"'))[[1]])
          tmp_href <- paste0("<a href='http://www.ensembl.org/Human/Search/Results?q=", tmp, ";site=ensembl;facet_species=Human' target='_blank'>", tmp, "</a>")
          tmp2 <- paste(tmp_href, collapse = ", ")
          drug_targets <- c(drug_targets, tmp2)
        }
        table_new <- data_frame(ensembl_protein_id = query_eprot_id_href[i], target_drugs = target_drugs_href, structure = str_href, drug_targets = drug_targets)
        table <- bind_rows(table, table_new)
      }
      valuetoupdate_href <- paste0("<a href='http://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", valuetoupdate, "' target='_blank'>", valuetoupdate, "</a>")
      title <- paste0("Drugs and target proteins for ", valuetoupdate_href)
    } else {
      valuetoupdate_href <- paste0("<a href='http://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", valuetoupdate, "' target='_blank'>", valuetoupdate, "</a>")
      title <- paste0("No target drugs for ", valuetoupdate_href, " in STITCH database")
      table <- NULL
    }
    dbDisconnect(mydb_sti)
    result <- list()
    result$title <- title
    result$table <- table
    return(result)
  }
  dbDisconnect(mydb_sti)
  return(NULL)
}

