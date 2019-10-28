conv_table <- function(table_text = readClipboard()){
  tmp <- table_text
  tmp <- paste0(tmp, collapse = "")
  tmp <- strsplit(tmp, "\\\\{2,}")[[1]]
  tmp <- lapply(tmp, function(x){strsplit(x, "&")[[1]]})
  n_col <- max(sapply(tmp, length))
  out <- list(trimws(tmp[[1]]))
  for(i in 2:length(tmp)){
    this_string <- tmp[[i]]
    this_string <- gsub("^\\s+$", "", this_string)
    this_string <- gsub("\\\\hline", "", this_string)
    if(length(this_string[!this_string == ""]) < n_col){
      replace_cells <- which(!this_string == "")
      out[[length(out)]][replace_cells] <- paste(out[[length(out)]][replace_cells], this_string[replace_cells], sep = "\n")
    } else {
      out <- c(out, list(trimws(this_string)))
    }
  }
  tab <- do.call(rbind,out)
  colnames(tab) <- tab[1, ]
  tab[-1, ]
}

