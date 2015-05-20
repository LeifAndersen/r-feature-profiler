## Feature Specific Profiling for R language
##
## This file does analyses the results collected from Rprof.
##
## To collect samples with Rprof, do something like this:
## Rprof("profile1.out")
## <code-here>
## Rprof(NULL)
##
## Analyse results with feature.profile()

library(base64enc);

## Collect the marks from a Rprof file,
## for use by analyzer
collect.marks <- function (filename = "Rprof.out",
                           features = c(),
                           chunksize = 5000) {

    ## Check the log type, ensure it contains continuation marks, get sample interval
    con <- file(filename, "rt");
    on.exit(close(con));
    firstline <- readLines(con, n = 1L);
    if(!length(firstline)) {
        stop(gettextf("no lines found in %s", sQuote(filename)), domain = NA);
    }
    sample.interval <- as.numeric(strsplit(firstline, "=")[[1L]][2L])/1e6
    marks.profiling <- grepl("marks profiling", firstline)
    if(!marks.profiling) {
        stop(gettextf("Marks not recorded in %s", sQuote(filename)), domain = NA);
    }

    traces <- list()

    ## Grab file in chunks
    repeat {
       chunk <- readLines(con, n = chunksize);
       if (length(chunk) == 0L) {
           break;
       }

       ## Grab list of continuation marks
       chunk <- strsplit(chunk, " ")
       chunk <- lapply(chunk, function(trace) {
           lapply(trace[grepl("^marks\\{", trace)], function(marks) {
               marks <- substr(marks, 7, nchar(marks) - 2);
               marks <- strsplit(marks, ",");
               res <- c();
               for(mark in marks) {
                   if(length(mark) > 0) {
                       mark <- substr(mark, 1, nchar(mark) - 1);
                       mark <- strsplit(mark, ":")[[1]];
                       key <- rawToChar(base64decode(mark[1]));
                       value <- rawToChar(base64decode(mark[2]));
                       if(is.element(key, features)) {
                           res[[key]]<-value;
                       }
                   }
               }
               res;
           })
       })

       ## Append chunk to traces
       traces <- c(traces, chunk);
   }

    ## Return result of parse
    list(traces=traces,interval=sample.interval);
}

## Returns the desired mark from the list:
filter.mark <- function(traces, mark) {
    lapply(traces, function(trace) {
        trace <- lapply(trace, function(frame) {
            frame[mark];
        })
        Filter(function(x) !(is.null(x) || is.na(x)), trace);
    })
}

## Feature profiler
feature.profile <- function(filename = "Rprof.out",
                            features = c("s3-dispatch", "summary"),
                            chunksize = 5000) {

    ## Collect Samples
    samples <- collect.marks(filename, features, chunksize);
    traces <- samples$traces;
    interval <- samples$interval;

    ## Calculate times
    exectime <- interval * length(traces);
    featureAnalysis <- lapply(setNames(features,features), function(feature) {
        featureMarks <- filter.mark(traces,feature);

        ## Get the bottom mark of each of the stacks
        bottomMark <- sapply(featureMarks, function(trace) {
            if(length(trace) > 0) {
                last <- trace[[length(trace)]];
                if(last != "antimark") {
                    last;
                }
            }
        })

        featureTime <- length(Filter(Negate(is.null), bottomMark))*interval;
        table(unlist(bottomMark))*interval;
    })

    ## Return Results
    list(samples=length(traces),
         time=exectime,
         features=featureAnalysis);
}

feature.profile(filename="micromethod.out")
