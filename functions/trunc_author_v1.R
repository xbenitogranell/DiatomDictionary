#------------------------------------------------------------
# trunc_author_v1
#------------------------------------------------------------

truncAuthor_v1 <- function(x){
  
  # clean HTML and whitespace
  x <- gsub("&nbsp;", " ", x)
  x <- gsub("&amp;", "&", x)
  x <- trimws(gsub("\\s+", " ", x))
  
  # Remove ...61 or similar trailing dot-number artifacts following sp./spp.
  x <- gsub("(sp\\.|spp\\.)\\s*\\.+.*$", "\\1", x, ignore.case = TRUE)
  
  parts <- str_split(x, "\\s+")[[1]]
  
  # special cases
  if (length(parts) == 1)
    return(parts)
  
  if (length(parts) > 0 && tolower(parts[1]) %in% c("unknown", "undetermined")) {
    keep <- c(parts[1])
    i <- 2
    while (i <= length(parts)) {
      token <- parts[i]
      # Stop if we hit an author name (all uppercase words or starting with parenthesis)
      if (grepl("^\\(", token) || (grepl("^[A-Z]+$", token) && !(token %in% c("F1","F2","F3")))) {
        break
      }
      keep <- c(keep, token)
      i <- i + 1
    }
    return(paste(keep, collapse = " "))
  }
  
  # Check for sp. / spp. including attached/separated numbers and optional '?'
  sp_pos <- which(parts %in% c("sp.", "spp.") | grepl("^(sp|spp)\\.?\\d+\\??$", parts))
  if (length(sp_pos) > 0) {
    idx <- sp_pos[1]
    
    if (parts[idx] %in% c("sp.", "spp.")) {
      if (idx < length(parts) && grepl("^\\d+\\??$", parts[idx + 1])) {
        idx <- idx + 1
        if (idx < length(parts) && parts[idx + 1] == "?") {
          idx <- idx + 1
        }
      } else if (idx < length(parts) && parts[idx + 1] == "?") {
        idx <- idx + 1
      }
    } else {
      if (idx < length(parts) && parts[idx + 1] == "?") {
        idx <- idx + 1
      }
    }
    return(paste(parts[1:idx], collapse = " "))
  }
  
  # Handle leading cf., aff., or ? at the very beginning (e.g., ? Neidium ...)
  if (parts[1] %in% c("cf.", "aff.", "?") && length(parts) > 1) {
    keep <- c(parts[1], parts[2])
    i <- 3
  } else {
    keep <- c(parts[1]) # genus
    i <- 2
  }
  
  while (i <= length(parts)) {
    
    token <- parts[i]
    
    # authority beginning with parenthesis
    if (grepl("^\\(", token))
      break
    
    # Handle names separated by / (e.g., / Cavinula variostriata)
    if (token == "/") {
      keep <- c(keep, token)
      
      # Grab the next token (Genus 2, capitalized)
      if (i < length(parts)) {
        keep <- c(keep, parts[i + 1])
        i <- i + 1
      }
      
      # Grab the token after that if it's a species epithet (lowercase)
      if (i < length(parts) && !grepl("^[A-Z]", parts[i + 1])) {
        keep <- c(keep, parts[i + 1])
        i <- i + 1
      }
      
      i <- i + 1
      next
    }
    
    # rank and uncertainty qualifiers
    if (token %in% c("cf.", "aff.")) {
      
      keep <- c(keep, token)
      
      if (i < length(parts))
        keep <- c(keep, parts[i + 1])
      
      i <- i + 2
      next
    }
    
    if (token %in% c("var.", "f.", "fo.")) {
      
      keep <- c(keep, token)
      
      if (i < length(parts))
        keep <- c(keep, parts[i + 1])
      
      i <- i + 2
      next
    }
    
    if (token %in% c("agg.", "complex", "s.l.", "s.s.")) {
      
      keep <- c(keep, token)
      break
    }
    
    # stop if token looks like an author
    if (grepl("^[A-Z]", token) &&
        !(token %in% c("F1","F2","F3")))
      break
    
    keep <- c(keep, token)
    
    i <- i + 1
  }
  
  paste(keep, collapse = " ")
}
