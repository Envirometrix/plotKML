
.df2htmltable <- function(x, fix.enc = TRUE, columns = TRUE) {

    # if the user passed in TRUE, then we want all of the columns
    # AG: AFAIK, we shouldn't use class(x) == "something" to test for class
    # Maybe: if (is.logical(columns)) ?
    if (class(columns) == 'logical'){
        columns <- seq_len(ncol(x)) # use all columns from the dataframe row
    }
    else {     # otherwise, keep only requested columns
        x <- x[, columns]
    }                  
    
    # fix encoding:
    if (fix.enc) {
        x <- data.frame(lapply(x, iconv, to = "UTF8"))
    }
    
    # get selected table data:
    # AG: AFAIK, sapply shouldn't be used in a package since it's not type-safe.
    # I would use vapply(..., FUN.VALUE = character(1))
    att.names <- sapply(
      names(x),
      function(i) {
        paste(
          '<span style="font-weight: bold; color: #000000; padding: 3px;">',
          as.character(i),
          "</span>:&nbsp;",
          sep = ""
        )
      }
    )
    att.values <- as.vector(
      t(
        sapply(
          x,
          function(i) {
            paste(
              '<span style="color: #000000; padding:3px;">',
              as.character(i),
              "</span><br>",
              sep = ""
            )
          }
        )
      )
    )
    # combine by interleaving:
    att <- matrix(
        paste(att.names, att.values, sep = "\n"), 
        ncol = length(names(x)), 
        byrow = TRUE
    )
    html.table <- apply(att, 1, paste, collapse="\n")
    
    html.table
}

# end of script;
