#' Process fastq file
#'
#' Process fastq file and create a data.frame of quality score
#'
#' @name prcfq
#' @usage prcfqs(file, n)
#' @usage prcfqlen(file)
#' @usage prcfqbox(file, n, ...)
#'
#' @param file character
#' @param n Size of sampling [default: 10000]
#'
#' @param ... additional plot options [required for `prcfqbox`]
#'
#' @return data frame / integer vector
#'
#' @examples \dontrun{
#' #
#'  in_f <- "test.fastq.bz2"
#'  res <- prcfqs(in_f)
#'
#'
#' }
#'
#' @rdname prcfq
#' @export
prcfqs <- function(file, n){

  # FUN1: Check file type
  ft <- function(path){
    f = file(path)
    ext = summary(f)$class
    close.connection(f)
    ext
  }

  # FUN2: Convert quality value to quality score
  sub_fqs <- function(x){
    # quality value
    pattern <- c("!", "\"", "#", "$", "%", "&", "'", "(", ")", "*",
                 "+", ",", "-", ".", "/", "0", "1", "2", "3", "4",
                 "5", "6", "7", "8", "9", ":", ";", "<", "=", ">",
                 "?", "@", "A", "B", "C", "D", "E", "F", "G", "H",
                 "I")
    # quality score
    y <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
           21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
           31, 32, 33, 34, 35, 36, 37, 38, 39, 40)

    # Check if there are any elements that do not match the quality values.
    x <- ifelse(x %in% pattern, x, NA)
    if (any(is.na(x))) {
      stop("There is an unknown string as quality value")
    }

    # Location of elements in quality score that do not correspond to the target qv.
    pos <- sapply(pattern, function(s){
      if (any(x %in% s)) {
        which(x %in% s)
      } else {
        NA
      }
    }, simplify = F )

    # Exclude elements from qscore and qvalue that do not exist in the target
    y <- y[!is.na(pos)]
    pos <- pos[!is.na(pos)]

    # Convert a quarity value to a quarity score
    qs <- x
    invisible(lapply(seq_along(pos), function(i) qs[pos[[i]]] <<- y[i] ))
    qs
  }

  # FUN3: Combine lists of different lengths filling with NA.
  bindfill <- function(x){
    mlen <- max(sapply(x, length))
    bindfilled_x <- do.call(cbind, lapply(x, function(y) c(y, rep(NA, mlen - length(y)))))
  }

  ### MAIN ###
  # create file connection, and get line number
  if (ft(file) == "gzfile") {
    con = gzfile(description = file, open = "r")
    nfq <- as.integer(system(paste("gunzip -c", file, "| wc -l"), intern = T))/4
  } else if (ft(file) == "bzfile") {
    con = bzfile(description = file, open = "r")
    nfq <- as.integer(system(paste("bunzip2 -c", file, "| wc -l"), intern = T))/4
  } else {
    con = file(description = file, open = "r")
    nfq <- as.integer(system(paste("cat", file, "| wc -l"), intern = T))/4
  }

  # Extract quality value from a connection of fastq
  i <- 1; j <- 1
  si <- sort(sample(1:nfq, n))
  qs <- vector("list", length = n)

  while ( TRUE ) {
    # Read line
    line = readLines(con, n = 4)

    # Breake at the end of file
    if ( length(line) == 0 ) {
      break
    }

    # Sampling line
    if ( i == si[j] && j <= n) {
      qv <- unlist(strsplit(line[4], ""))
      qs[[j]] <- as.numeric(sub_fqs(x = qv))
      j <- j + 1
    } else if (is.na(si[j])) {
      break
    }

    i <- i + 1
  }
  close(con)

  return(data.frame(t(bindfill(qs))))
}

#' @rdname prcfq
#' @export
prcfqlen <- function(file){
  # FUN1: Check file type
  ft <- function(path){
    f = file(path)
    ext = summary(f)$class
    close.connection(f)
    ext
  }

  # create file connection
  if (ft(file) == "gzfile") {
    con = gzfile(description = file, open = "r")
    nfq <- as.integer(system(paste("gunzip -c", file, "| wc -l"), intern = T))/4
  } else if (ft(file) == "bzfile") {
    con = bzfile(description = file, open = "r")
    nfq <- as.integer(system(paste("bunzip2 -c", file, "| wc -l"), intern = T))/4
  } else {
    con = file(description = file, open = "r")
    nfq <- as.integer(system(paste("cat", file, "| wc -l"), intern = T))/4
  }

  # Extract quality value from a connection of fastq
  i <- 1
  rlen <- vector("integer", length = nfq)
  while ( TRUE ) {
    line = readLines(con, n = 4)

    if ( length(line) == 0 ) {
      break
    }

    rlen[i] <- nchar(line[2])
    i <- i + 1
  }
  close(con)
  return(rlen)
}

#' @rdname prcfq
#' @export
prcfqbox <- function(file, n, ...){
  res <- prcfqs(file, n)

  graphics::boxplot(res, outline = F,
          xaxt="n", ylim = c(0, 40),
          xlab = "Cycle", ylab = "Quality Score", ...)
  graphics::abline(h=c(30,10), col = "gray60", lty = 3)
  graphics::axis(1, seq(0, ncol(res), 10))

}




