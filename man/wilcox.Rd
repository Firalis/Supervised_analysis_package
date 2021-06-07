\name{wilcox}
\alias{wilcox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wilcox(start, end, group_1, group_2, log, base, comparison)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{group_1}{
%%     ~~Describe \code{group_1} here~~
}
  \item{group_2}{
%%     ~~Describe \code{group_2} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{comparison}{
%%     ~~Describe \code{comparison} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (start, end, group_1, group_2, log, base, comparison) 
{
    if (is.numeric(start) == FALSE) {
        stop("The argument start need to be a numeric argument")
    }
    if (is.numeric(end) == FALSE) {
        stop("The argument end need to be a numeric argument")
    }
    if (end == start) {
        stop("You need to have at least 2 variable in your data frame or matrix")
    }
    if (is.matrix(group_1) == FALSE & is.data.frame(group_1) == 
        FALSE) {
        stop("The argument group_1 need to be a data frame or a matrix")
    }
    if (is.matrix(group_2) == FALSE & is.data.frame(group_2) == 
        FALSE) {
        stop("The argument group_2 need to be a data frame or a matrix")
    }
    if (is.logical(log) == FALSE) {
        stop("The argument log need to be a logical argument")
    }
    if (is.numeric(base) == FALSE) {
        stop("The argument base need to be a numeric argument")
    }
    if (is.character(comparison) == FALSE) {
        stop("The argument comparison need to be a characther")
    }
    test <- NULL
    for (i in start:end) {
        pvalue <- NULL
        pvalue <- c(variable <- colnames(group_2)[i], moygroup2 <- mean(group_2[, 
            i], na.rm = TRUE), sdgroup2 <- sd(group_2[, i], na.rm = TRUE), 
            moygroup1 <- mean(group_1[, i], na.rm = TRUE), sdgroup1 <- sd(group_1[, 
                i], na.rm = TRUE), pvalue <- wilcox.test(group_1[, 
                i], group_2[, i], na.rm = TRUE)$p.value)
        foldchange <- NULL
        if (log == TRUE) {
            foldchange <- mean(base^group_1[, i], na.rm = TRUE)/mean(base^group_2[, 
                i], na.rm = TRUE)
        }
        else {
            foldchange <- mean(group_1[, i], na.rm = TRUE)/mean(group_2[, 
                i], na.rm = TRUE)
        }
        pvalue <- c(pvalue, foldchange <- foldchange)
        test <- rbind(test, pvalue)
    }
    write.csv(test, paste("Summary_", comparison, ".csv", sep = ""), 
        row.names = FALSE)
    return(test)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
