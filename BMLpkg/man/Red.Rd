\name{Red}
\alias{Red}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Red Car Moves
}
\description{
Takes in a matrix and outputs one with unblock red car indicators moved to the right by one grid.
}
\usage{
Red(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
%%     ~~Describe \code{matrix} here~~
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
Shuhua Liang
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Blue
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (matrix) 
{
    changed.num.ratBack = swap(matrix)
    changed.num.ratBack
    Ready2Up = Turn2(changed.num.ratBack)
    Ready2Up
    Moved.right = oneUp.matrix(Ready2Up)
    Moved.right
    Origin.Dir = TurnBack2(Moved.right)
    Origin.Dir
    ready2plot = swap(Origin.Dir)
    return(ready2plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ red }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
