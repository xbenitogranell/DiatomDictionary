#------------------------------------------------------------
# diatom_dictionary_v1
#------------------------------------------------------------
diatom_dictionary_v1 <- function(diatnames="", 
                                 biodata="",
                                 eddi_taxon_table="",
                                 neotoma_master_list="",
                                 output_file = ""){
  
  
  #If any required package is missing from your local environment, `pacman` will automatically download and install it before loading
  if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")} 
  pacman::p_load(tidyverse, stringr, qdapTools, worrms, openxlsx, stringdist, diathor, taxize)
  
  ## Step 1: take the column where the user's diatom names are, and create the output table called conversion_df. This step also makes sure that the column is named "user_taxa"
  conversion_df <- user_list %>%
    dplyr::rename(user_taxa=contains("user")) 
  
  ## Import individual diatom taxon lists from available sources
  # DIATHOR R package
  diathor_taxalist <- as.data.frame(diat_taxaList())
  colnames(diathor_taxalist) <- "taxa"
  
  # BIODATA
  diat_biodata <- as.data.frame(BioData[,"BiodataTaxonName"])
  colnames(diat_biodata) <- "taxa"
  
  # EDDI
  diat_eddi <- as.data.frame(eddi_diatoms[,"TaxonName"])
  colnames(diat_eddi) <- "taxa"
  
  #####
  ##### HERE more taxon lists can be added: European diatom list from PalaeOpen literature review, SeeD etc
  #####
  
  # Here creates a unique taxon list combining all the previously imported csv files
  taxaList <- unique(c(diathor_taxalist$taxa, diat_biodata$taxa, diat_eddi$taxa))
  taxaList <- as.data.frame(taxaList)
  colnames(taxaList) <- "taxa"
  
  # (un)capitalize genera names and keep unique names
  taxaList$taxa <- capitalize(taxaList$taxa) 
  taxaList <- taxaList %>%
    dplyr::distinct(taxa)
  
  ## Step 2: make sure var. cf., and forma, etc are written correctly for Neotoma standards. The result is lev1 column
  conversion_df$user_taxa_lev1 <- gsub(conversion_df[,"user_taxa"], pattern=" var ", replacement=" var. ", fixed = TRUE)
  conversion_df$user_taxa_lev1 <- sub(conversion_df$user_taxa_lev1, pattern="^\\s*\\?\\s*", replacement="")
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern="(%)", replacement="", fixed=TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" cf ", replacement=" cf. ", fixed=TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" for. ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" f. ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" f ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" sp ", replacement=" sp. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern="spp(?!\\.)", replacement="spp.", perl =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" spec.", replacement=" spp. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" aff ", replacement=" aff. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" sl ", replacement=" s.l. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" ss ", replacement=" s.s. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" s. l. ", replacement=" s.l. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- trimws(gsub("\\s+", " ", conversion_df$user_taxa_lev1))
  #conversion_df$user_taxa_lev1 <- str_squish(conversion_df$user_taxa_lev1)
  
  # Apply function to remove authorities if any were provided by user taxa list and if so place them in a new column
  conversion_df <- conversion_df %>%
    mutate(
      cleaned_input = gsub("(sp\\.|spp\\.)\\s*\\.+.*$", "\\1", user_taxa_lev1, ignore.case = TRUE),
      cleaned_input = gsub("\\s*\\(?(girdle\\s*view|girdle\\s*vie|chrysophyte|statocysts?|3?girdle\\s*view)\\)?.*$", "", cleaned_input, ignore.case = TRUE),
      taxon=sapply(cleaned_input, truncAuthor_v5),
      user_authority=str_trim(str_remove(cleaned_input, fixed(taxon)))) %>%
    dplyr::select(-user_taxa_lev1, -cleaned_input) %>%
    rename(user_taxa_lev1=taxon)
  
  #performs fuzzy matching between user's species name (lev1) and all taxa in all internal csv diatom taxon lists
  pb <- txtProgressBar(min = 1, max = nrow(conversion_df), style = 3)
  for (i in 1:nrow(conversion_df)){
    spname <- conversion_df[,"user_taxa_lev1"][i]
    # Check if the name represents a general sp. or spp. (e.g., contains "sp.", "spp.", or "sp.1")
    is_general_sp <- grepl("\\bspp?\\b|\\bsp\\.?\\d+", tolower(spname))
    if (is_general_sp) {
      # Exact match only for sp. / spp. items to avoid bad fuzzy matches
      taxasearch <- taxaList[tolower(taxaList[,"taxa"]) == tolower(spname), ]
    } else {
      # Standard fuzzy matching for fully identified species
      taxasearch <- taxaList[stringdist::ain(tolower(taxaList[,"taxa"]), tolower(spname), maxDist = 1, matchNA = FALSE), ]
    }
    if (length(taxasearch) == 0){ # if species not found in any internal taxon list
      conversion_df[i,"conversion_notes"] <- "Taxon not found in any taxon list"
      print(paste("Taxon not found in any taxon list:", spname))
    }
    # correct the species name     
    else if (length(taxasearch) >= 1){
      conversion_df[i,"user_taxa_lev2"] <- taxasearch[1] # take the first match
      # add conversion notes if species name has been corrected
      if (conversion_df$user_taxa_lev1[i] != conversion_df$user_taxa_lev2[i]) {
        conversion_df$conversion_notes[i] <- "Name updated"
      } else {
        conversion_df$conversion_notes[i] <- "Taxon found"
      }
    }
    # update progressbar 
    setTxtProgressBar(pb, i)
  }
  
  #close progressbar
  print(paste("A total of",sum(conversion_df$conversion_notes=="Taxon not found in any taxon list", na.rm = TRUE),"taxa out of",dim(conversion_df)[1],"are not found in any internal taxon list"))
  close(pb)
  
  # Replace NAs in those diatom taxa not found with the original diatom user names
  # Handling sp. vs spp.: take the original user taxa designations
  if (!exists("conversion_df")) {
    stop("'conversion_df' does not exist in the environment. Please check your user_list.")
  }
  
  if (!"user_taxa_lev2" %in% names(conversion_df)) {
    stop("Column 'user_taxa_lev2' not found in 'conversion_df'. A plausible reason is that any name has been found in the internal list")
  }
  
  if (nrow(conversion_df) == 0) {
    stop("Error: 'conversion_df' has 0 rows. A plausible reason is that any name has been found in the internal list")
  }
  
  conversion_df[is.na(conversion_df$user_taxa_lev2)==TRUE,]$user_taxa_lev2 <- conversion_df[is.na(conversion_df$user_taxa_lev2)==TRUE,]$user_taxa_lev1
  
  #Perform gsub with the correct argument order
  conversion_df$user_taxa_lev2 <- gsub(conversion_df$user_taxa_lev2, pattern="sp\\..*$", replacement="sp.")
  conversion_df$user_taxa_lev2 <- gsub(conversion_df$user_taxa_lev2, pattern="sp\\?", replacement="sp.")
  
  message("Internal list taxon name updated successfully.")
  
  # Creates two separate dataframes with found & updated taxa, and taxa not found
  taxaExcluded <- conversion_df[which(conversion_df$conversion_notes == "Taxon not found in any taxon list"),]
  taxaExcluded$conversion_notes <- NULL
  taxaIncluded <- conversion_df[conversion_df$conversion_notes %in% c("Name updated", "Taxon found"), ]
  taxaIncluded$conversion_notes <- NULL
  
  
  ## START taxon look up in external databases using the taxize R package
  #synonymSearch <- list() #initialize the list where to store the global verifier results search-->synonyms
  externalSearch <- list() #initialize the list where to store the global verifier results search
  pb <- txtProgressBar(min = 1, max = nrow(conversion_df), style = 3)
  
  # data sources: https://resolver.globalnames.org/data_sources
  # all_matches: When TRUE, return all found matches, not only the best one. Multiple results are returned in results if all_matches=TRUE. These results are sorted by matching quality, the first result is the same as bestResult.
  for (i in 1:nrow(conversion_df)) {
    spname <- conversion_df[,"user_taxa_lev2"][i]
    externalSearch[[i]] <- try(gna_verifier(conversion_df[,"user_taxa_lev2"][i], data_sources = c(9,11,12,13,133,195), all_matches = TRUE))
    
    # get the first five best taxon results that contain accepted and synonym
    if (!inherits(externalSearch[[i]], "try-error") && !is.null(externalSearch[[i]])) {
      
      # Extract the results data frame 
      status_df <- externalSearch[[i]] 
      
      if (!is.null(status_df) && nrow(status_df) > 0) {
        
        status_res <- status_df[tolower(status_df$taxonomicStatus) %in% c("accepted", "synonym"), ]
        # get only the first five occurrence of "accepted" and the first occurrence of "synonym" if any
        externalSearch[[i]] <- status_res %>%
          group_by(taxonomicStatus) %>%
          slice_head(n=5) %>%
          ungroup()
        
        #this is to save all the synonyms in a different list
        #synonymSearch[[i]] <- status_df[tolower(status_df$taxonomicStatus) %in% c("synonym"), ] 
        
      } else {
        externalSearch[[i]] <- data.frame() # Return empty df if no results
      }
    }
    print(paste("Taxon found using Global Name Verifier:", spname))
    #update progressbar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # extract the results from the externalList() and select the columns of interest
  # the objetive is to obtain the taxonomy table showing the best 5 occurrences of accepted and synonyms found for each user taxon name
  # NOTES:
  # matchedcanonicalFull: successul match
  # currentName: successful match with authority
  
  required_cols <- c("submittedName", "recordId", "dataSourceTitleShort", "matchedCanonicalFull", "currentName", "taxonomicStatus")
  missing_cols <- setdiff(required_cols, names(do.call(rbind, externalSearch)))
  
  if (length(missing_cols) > 0) {
    stop(paste("No taxa to extract taxonomic information from. Please check your input dataframe"))
  }
  
  taxonomy <- do.call(rbind, externalSearch) %>%
    dplyr::select("submittedName", "recordId","dataSourceTitleShort", "matchedCanonicalFull", "currentName",
                  "taxonomicStatus") %>%
    rename(user_taxa_lev2=submittedName) %>%
    distinct(currentName,dataSourceTitleShort, .keep_all = TRUE) #distinct current names and taxonomic source
  
  # Extract the result from the externalList() to create the conversion table.
  # IMPORTANT: this resulting table must have the same number of rows than conversion_df
  conversion_table <- conversion_df %>%
    left_join(
      do.call(rbind, externalSearch) %>%
        dplyr::select(
          submittedName, recordId, dataSourceTitleShort,
          matchedCanonicalFull, currentName, taxonomicStatus
        ) %>%
        mutate(
          available_synonym = if_else(
            any(taxonomicStatus == "Synonym") &
              any(taxonomicStatus == "Accepted"),
            "YES",
            "NO"),.by = submittedName), by = c("user_taxa_lev2" = "submittedName")) %>%
    distinct(user_taxa,dataset, .keep_all = TRUE)
  
  # rename taxize df to conversion_df
  conversion_df <- conversion_table
  
  # Neotoma
  ## here look up against Neotoma diatom names and get the codes
  neotoma_diatList <- as.data.frame(unique(Neotoma[,c("TaxonCode","TaxonName","Author")]))
  
  print(
    paste(
      "Searching in Neotoma diatom master list:",
      list.files()[grepl("neotoma", list.files(), ignore.case = TRUE)]
    )
  )
  
  ## Neotoma exact matching and extract names, code and author
  # print("Searching in Neotoma diatom master list")
  pb <- txtProgressBar(min = 1, max = nrow(conversion_df), style = 3)
  
  for (i in 1:nrow(conversion_df)) {
    spname <- conversion_df[,"user_taxa_lev2"][i]
    
    #look up at Neotoma diatom master list and codes
    conversion_df$neotoma_code[i] <- lookup_e(spname, neotoma_diatList[,c("TaxonName","TaxonCode")])
    conversion_df$neotoma_name[i] <- lookup_e(spname, neotoma_diatList[,c("TaxonName","TaxonName")])
    conversion_df$neotoma_author[i] <- lookup_e(spname, neotoma_diatList[,c("TaxonName","Author")])
    
    if (is.na(conversion_df$neotoma_code[i])){ #if species not found in Neotoma
      print(paste("Taxon not found in Neotoma diatom master list (updated XXX):", spname))
    }
    #update progressbar
    setTxtProgressBar(pb, i)
  }
  print(paste("A total of",sum(is.na(conversion_df$neotoma_code)),"taxa out of",dim(conversion_df)[1],"are not found in Neotoma"))
  close(pb)
  # 
  
  ## Neotoma authority fuzzy matching
  conversion_df <- conversion_df %>%
    mutate(
      authority_match = stringdist(
        clean_auth(user_authority),
        clean_auth(neotoma_author),
        method = "jw") <= 0.2)
  
  ## create the Neotoma output table
  neotoma_df <- conversion_df[,names(conversion_df) %in% c("diatom_id","dataset", "user_taxa","user_taxa_lev2", "user_authority","neotoma_code","neotoma_name","neotoma_author","authority_match")]
  
  ## Worrms has publication details 
  # WORRMS loop
  print("Searching in DiatomBase for extracting further taxonomic information")
  
  d <- seq_along(conversion_df$matchedCanonicalFull) #create an index for each taxa name
  d2 <- unique(split(conversion_df$matchedCanonicalFull, d)) #make a list--one element for each spp
  results <- list() #initialize the list where to store the results
  #
  # # make a loop using try() to skip errors (i.e., those taxa not found)
  for (i in seq_along(d2)) {
    x <- d2[[i]]
    results[[i]]<- try(wm_records_name(x, fuzzy=FALSE,marine_only=FALSE), silent = TRUE) #return varieties
    print(paste("Searching taxon:", x))
    
  }
  
  ## Clean results: remove try-errors, NULLs, and empty data frames
  results_clean <- Filter(function(x) {
    !inherits(x, "try-error") && is.data.frame(x) && nrow(x) > 0
  }, results)
  
  if (length(results_clean) == 0) {
    stop("Error: No valid records found in DiatomBase/WoRMS for any taxa.")
  }
  
  # extract the results from the list
  #worms <- do.call(rbind, results)
  worms <- dplyr::bind_rows(results_clean)
  worms <- worms[!grepl("Error", worms$AphiaID),]
  worms$AphiaID <- as.integer(worms$AphiaID)
  
  # Create table with variables needed--> valid names
  worms <- as.data.frame(worms)
  dtmBase <- worms %>%
    #filter(status=="accepted") %>%
    dplyr::select("AphiaID", "scientificname", "valid_name", "valid_authority")
  
  # Get source information for the set of AphiaIDs created in the code chunk above (from Don Charles)
  print("Obtaining source information from the set of AphiaIDs found")
  dtmBase_source <- wm_sources_(id = dtmBase$AphiaID)
  
  # Returns sources for several variables, one of which is specified as "original description" in the "use" column
  dtm_Base_origDes <- dtmBase_source %>%
    filter(use=="original description") %>%
    dplyr::select(id, reference, page, doi) %>%
    mutate(id=as.integer(id))
  
  # Combine main taxa table (dtmBase_out) and source table (dtm_Base_origDes1)
  dtm_Base_comb <- left_join(dtmBase, dtm_Base_origDes, by = c("AphiaID" = "id")) %>%
    distinct(AphiaID,.keep_all = TRUE) 
  
  #verify that valid_name has zero duplicates left
  #stopifnot(nrow(dtm_Base_comb) == n_distinct(dtm_Base_comb$valid_name))
  
  # Combine worms table with the original conversion_df table
  diatomBase <- left_join(conversion_df, dtm_Base_comb, by = c("matchedCanonicalFull" = "valid_name")) %>%
    mutate(
      # Create a condition for unwanted names:
      # 1. Contains "unknown" or "undetermined" (case-insensitive)
      # 2. Contains an alphanumeric species code of 4 or more characters (adjust regex if needed)
      is_code_or_unknown = grepl("unknown|undetermined|unidentified", user_taxa_lev2, ignore.case = TRUE) | 
        grepl("^[a-zA-Z]{2}[a-z]{2,5}\\.?$|^[a-zA-Z]+(\\s+)?sp\\.?$", user_taxa_lev2),
      
      # If the condition is met, blank out (set to NA) the pulled columns
      AphiaID         = if_else(is_code_or_unknown, NA_integer_, AphiaID), # use NA_real_ if AphiaID is numeric/double
      scientificname  = if_else(is_code_or_unknown, NA_character_, scientificname),
      valid_authority = if_else(is_code_or_unknown, NA_character_, valid_authority),
      reference       = if_else(is_code_or_unknown, NA_character_, reference),
      page            = if_else(is_code_or_unknown, NA_character_, page), # change to NA_integer_ if page is numeric
      doi             = if_else(is_code_or_unknown, NA_character_, doi)
    ) %>%
    dplyr::select(diatom_id,user_taxa,dataset,user_authority,user_taxa_lev2,AphiaID,scientificname,valid_authority,reference,page,doi)  
  
  # Create the export list of results
  ## First select columns of interest from the conversion table
  conversion_df <- conversion_df[,names(conversion_df) %in% c("diatom_id","dataset", "user_taxa","user_authority", "user_taxa_lev1","user_taxa_lev2", "conversion_notes",
                                                              "available_synonym")]
  
  resultList <- list(conversion_df, taxonomy, diatomBase, neotoma_df)
  names(resultList) <- c("conversion_table", "taxonomy", "diatomBase", "neotoma_check")
  
  # save the final Excel list of harmonization results
  write.xlsx(resultList,
             file = output_file,
             overwrite = TRUE)
  
  return(resultList)
}