\name{normality}
\alias{normality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normality(start, end, dataX, project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{dataX}{
%%     ~~Describe \code{dataX} here~~
}
  \item{project}{
%%     ~~Describe \code{project} here~~
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
function (start, end, dataX, project) 
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
    if (is.matrix(dataX) == FALSE & is.data.frame(dataX) == FALSE) {
        stop("The argument group_1 need to be a data frame or a matrix")
    }
    if (is.character(project) == FALSE) {
        stop("The argument comparison need to be a characther")
    }
    test <- NULL
    for (i in start:end) {
        if (sd(dataX[, i], na.rm = T) == 0) {
            test <- rbind(test, c(colnames(dataX)[i], NA))
        }
        else {
            test <- rbind(test, c(colnames(dataX)[i], shapiro.test(dataX[, 
                i])$p.value))
        }
    }
    return(test)
    write.csv(test, paste("normality test for", project, ".csv", 
        sep = ""))
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
