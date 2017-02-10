aswap_join <- function(x, y, by = NULL, maxDist = 2,
                       tolerance = 1,
                       method = c("lv", "osa", "dl", "hamming", "lcs", "qgram",
                                  "cosine", "jaccard", "jw", "soundex"),
                       mode = "inner",
                       ignore_case = FALSE,
                       match_col = NULL,
                       dist_col = NULL,
                           # c(NULL,"each","total","both"),
                       word_col = NULL, ...) {
    
    method <- match.arg(method)
    dist_col <- match.arg(dist_col)
    
    if (!between(tolerance,0,1)) {
        stop("Must give tolerance between 0 and 1")
    }
    
    if (method == "soundex") {
        # soundex always returns 0 or 1, so any other maxDist would
        # lead either to always matching or never matching
        maxDist <- .5
    }
    
    
    match_fun <- function(v1, v2) {
        if (ignore_case) {
            v1 <- stringr::str_to_lower(v1)
            v2 <- stringr::str_to_lower(v2)
        }
        
        spl1 <- strsplit(v1, " +")
        spl2 <- strsplit(v2, " +")
        
        # if we are looking for exact match, then the words in spl1
        # must have at least as many words as in spl2
        if (tolerance == 1) {

            match.count <- sapply(seq_along(spl1), function(i) {
                if (length(spl1[[i]])  <= length(spl2[[i]])) {
                    sum(
                        stringdist::ain(spl1[[i]], spl2[[i]],
                                        method = method,
                                        maxDist = maxDist,
                                        ...),
                        na.rm=TRUE)
                } else {
                    0
                }
            })
            
        } else {
            match.count <- sapply(seq_along(spl1), function(i) {
                sum(
                    stringdist::ain(spl1[[i]], spl2[[i]],
                                    method = method,
                                    maxDist = maxDist,
                                    ...),
                    na.rm=TRUE)
            })
            
        }
        
        
        # take the word count to compare with the tolerance
        word.count <- sapply(seq_along(spl1), function(i) {
            length(spl1[[i]])  
        })
        
        word.count[word.count==0] <- 1
        ret <- dplyr::data_frame(include = 
                                     (match.count/word.count >= tolerance))
        if (!is.null(match_col)) {
            ret[[match_col]] <- match.count/word.count
        }
        
        if (!is.null(dist_col) | !is.null(word_col)) {
            
            word.matches <- lapply(seq_along(spl1), function(i) {
                    word.indices <- 
                        stringdist::amatch(spl1[[i]], spl2[[i]],
                                    method = method,
                                    maxDist = maxDist,
                                    ...)
                    sapply(seq_along(word.indices),
                           function(j) spl2[[i]][word.indices[j]]
                    )
            })
            
            
        }
        
        if (!is.null(dist_col)) {
            dist <- lapply(
                seq_along(word.matches),
                function(i) {
                    stringdist::stringdist(
                        spl1[[i]],
                        word.matches[[i]],
                        method=method,
                        ...)
                })

            if (dist_col == "both") {
            ret[["each"]] <- sapply(
                seq_along(dist),
                function(i) {
                    paste(dist[[i]],
                          collapse = ", ")
                })
            
            ret[["total"]] <- sapply(
                seq_along(dist),
                function(i) {
                    sum(dist[[i]],
                          na.rm = TRUE)
                })
            
            } else if (dist_col == "each") {
                ret[[dist_col]] <- sapply(
                    seq_along(dist),
                    function(i) {
                        paste(dist[[i]],
                              collapse = ", ")
                    })
            } else if (dist_col == "total") {
                ret[[dist_col]] <- sapply(
                    seq_along(dist),
                    function(i) {
                        denom <- (length(dist[[i]]) +
                            length(dist[[i]][is.na(dist[[i]])]) *
                                2
                        )* maxDist
                        numer <- sum(dist[[i]],
                            na.rm = TRUE) +
                            length(dist[[i]][is.na(dist[[i]])]) *
                            2 * maxDist
                        1-(numer/denom)
                    })
            }
        }
        
        if (!is.null(word_col)) {
            ret[[word_col]] <- sapply(
                seq_along(word.matches),
                function(i) {
                    paste(word.matches[[i]], collapse = " ")
                    })
        }
        
        ret
    }
    fuzzyjoin::fuzzy_join(x, y, by = by, mode = mode, match_fun = match_fun)
}
