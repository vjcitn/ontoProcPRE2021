
nroots = function(x) {
 obs <- if (is.null(x$obsolete)) 
        rep(FALSE, length(x$id))
    else x$obsolete
    roots <- x$id[!obs & sapply(x$parents, length) == 0]
length(roots)
}

funcs = grep("^get", ls("package:ontoProc"), value=TRUE)
desc = function(x) {
  ona = function(x) if(length(x)==0 || nchar(x)==0) NA else x
  dsub = function(x) sub("data-version: ", "", x)
  fsub = function(x) sub("format-version: ", "", x)
  ont = do.call(x, list()); 
   dv = grep("^data-version", attr(ont, "version"), value=TRUE)
   fv = grep("^format-version", attr(ont, "version"), value=TRUE)
   data.frame(func=x, nclass=length(ont$name), nprop=length(names(ont)), nroots=nroots(ont), datav=ona(dsub(dv)), fmtv=ona(fsub(fv)), stringsAsFactors=FALSE)
}
packdesc = do.call(rbind, lapply(funcs, desc))
write.csv(packdesc, file="packdesc.csv")
