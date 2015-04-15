library(base64enc);


## Collect the marks from a Rprof file,
## for use by analyzer
collect.marks <- function (filename = "Rprof.out",
                           chunksize = 5000) {

    ## Check the log type, ensure it contains continuation marks, get sample interval
    con <- file(filename, "rt");
    on.exit(close(con));
    firstline <- readLines(con, n = 1L);
    if(!length(firstline))
        stop(gettextf("no lines found in %s", sQuote(filename)), domain = NA);
    sample.interval <- as.numeric(strsplit(firstline, "=")[[1L]][2L])/1e6
    marks.profiling <- grepl("marks profiling", firstline)
    if(!marks.profiling)
        stop(gettextf("Marks not recorded in %s", sQuote(filename)), domain = NA);

    traces <- list()

    ## Grab file in chunks
    repeat {
       chunk <- readLines(con, n = chunksize);
       if (length(chunk) == 0L)
           break

       ## Grab list of continuation marks
       chunk <- strsplit(chunk, " ")
       chunk <- lapply(chunk, function(trace) {
           lapply(trace[grepl("^marks\\{", trace)], function(marks) {
               marks <- substr(marks, 7, nchar(marks) - 2);
               marks <- strsplit(marks, ",");
               res <- new.env()
               for(mark in marks) {
                   mark <- substr(mark, 1, nchar(mark) - 1);
                   mark <- strsplit(mark, ":")[[1]];
                   key <- rawToChar(base64decode(mark[1]));
                   value <- rawToChar(base64decode(mark[2]));
                   res[[key]] <- value;
               }
               res;
           })
       })

       ## Append chunk to traces
       traces <- c(traces, chunk);
   }


    ## Return result of parse
    traces
}

feature.profile <- function(body, tags) {

}


collect.marks("profile1.out")
