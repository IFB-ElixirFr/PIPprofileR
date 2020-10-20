output$sessionText = renderUI({
  HTML(paste(si[[1]]$version.string,",", si[[1]]$platform, "<br>","<br>",
             "<b>Locale</b><br>", paste(si[[3]], collapse = " , "), "<br>","<br>",
             "<b>Attached base packages</b><br>", paste(si[[5]], collapse = " , "),"<br>","<br>",
             "<b>Other attached packages</b><br>", paste(unlist(lapply(si$otherPkgs, function(x){paste(x$Package, x$Version)})), collapse = " , "), "<br>","<br>",
             "<b>Loaded via a namespace (and not attached)</b><br>" ,paste(unlist(lapply(si$loadedOnly, function(x){paste(x$Package, x$Version)})), collapse = " , ") 
  ))
})