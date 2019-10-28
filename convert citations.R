tmp <- readClipboard()

cite <- tmp[grepl("\\cite", tmp)]
#x <- cite[4]
#cite <- backup<-cite
tmp[grepl("\\cite", tmp)] <- sapply(cite, function(x){
  x <- gsub("\\\\citep\\{", "@CLOSINGBRACKET", x)
  x <- gsub("\\\\cite\\{", "@", x)
  segments <- strsplit(x, "@")[[1]]
  segments[-1] <- sapply(segments[-1], function(x) paste0("@", x))
  segments[-1] <- sapply(segments[-1], function(i){
    #i <- segments[2]
    refs <- regmatches(i, regexpr("\\}", i), invert = TRUE)[[1]]
    if(grepl(",", refs[1])){
      refs[1] <- gsub(",", "; @", refs[1])
    }

    if(any(grepl("CLOSINGBRACKET", refs))){
      bracks <- which(grepl("CLOSINGBRACKET", refs))
      refs[bracks] <- gsub("@CLOSINGBRACKET", "\\[@", refs[bracks])
      refs[bracks] <- gsub("$", "\\]", refs[bracks])
    }
    paste0(refs, collapse = "")
  }, USE.NAMES = FALSE)
  paste0(segments, collapse = "")
})

writeClipboard(tmp)

tmp <- readClipboard()

tmp <- gsub("@(.+?):(\\d+?)", "@\\1\\2", tmp)
