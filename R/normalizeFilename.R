
## Fix characters following the naming conventions [http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx]:
normalizeFilename <- function(x, form = c("default", "8.3")[1], fix.encoding = TRUE, sub.sign = "_"){

   # reserved characters:
   sel = c("<", ">", ":", '\\"', "/", "\\|", "\\?", "\\*", "[[:space:]]", "\\s+$", "\\[", "\\]")
   for(i in sel){ 
      x <- gsub(pattern=i, replacement=sub.sign, x)
   }
   # shorten the path:
   if(form == "8.3"){
      if(.Platform$OS.type == "windows") {
       x <- utils::shortPathName(x)
      }
   }
   if(fix.encoding==TRUE){
      x <- iconv(x, to = "UTF8")
   }

   return(x)
}

## https://rdrr.io/github/r-spatial/RSAGA/src/R/gridtools.R
.set.file.extension = function(filename, extension, fsep=.Platform$file.sep) {
   if (extension=="") extension = "."
   if (substr(extension,1,1)!=".") extension = paste(".",extension,sep="")
   if (Sys.info()["sysname"] == "Windows")
      filename = gsub("\\",fsep,filename,fixed=TRUE)
   the.extension = .get.file.extension(filename)
   filename = substr(filename,1,nchar(filename)-nchar(the.extension))
   return( paste( filename, extension, sep="") )
}

.get.file.extension = function(filename, fsep=.Platform$file.sep) {
   ext = rep("",length(filename))
   has.dot.extension = substring(filename, nchar(filename))=="."
   if (Sys.info()["sysname"] == "Windows")
      filename = gsub("\\",fsep,filename,fixed=TRUE)
   split = strsplit(filename,fsep,fixed=TRUE)
   split = sapply( split, function(x) x[length(x)] )
   split = strsplit(split,".",fixed=TRUE)
   ext = sapply( split, function(x) x[length(x)] )
   has.extension = sapply(split,length) > 1
   ext[ has.extension ] = paste(".",ext[has.extension],sep="")
   ext[ !has.extension ] = ""
   ext[ has.dot.extension ] = "."
   return(ext)
}

