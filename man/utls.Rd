% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utls.R
\name{utls}
\alias{utls}
\alias{ul}
\alias{dup}
\alias{char_sort}
\alias{char_fct}
\alias{str_tail}
\alias{str_head}
\title{My R utility functions.}
\usage{
ul(x)

dup(x)

char_sort(x)

char_fct(x)

str_tail(x, n)

str_head(x, n)

dup(x)

char_sort(x)

char_fct(x)

str_tail(x, n)

str_head(x, n)
}
\arguments{
\item{x}{atomic}

\item{n}{integer}
}
\description{
My R utility functions.
}
\examples{
# unique length
set.seed(12)
v1 <- sample(letters[1:5], size = 5, TRUE)
ul(v1)

# return the list of position of duplicated value.
v2 <- sample(letters[1:5], size = 5, FALSE)
dup(v1)
dup(v2)

# Sort string vectors in numerical order
v3 <- sort(paste0("V", as.character(1:13)))
char_sort(v3)

# create factor with levels by numerical order
char_fct(v3)

# head and tail for strings
v4 <- sapply(1:3, function(i) paste(sample(c("A","T","G","C"), 10, TRUE), collapse = ""))
str_head(v4, 3)
str_tail(v4, 3)

}
