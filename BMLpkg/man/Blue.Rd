\name{Blue}
\alias{Blue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Blue Car Moves
}
\description{
Takes in a matrix and outputs one with unblock blue car indicators moved up by one grid.
}
\usage{
Blue(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
The car plane.
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
Red
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (matrix) 
{
    rightDirection = turn.mat.90left(matrix)
    rightDirection
    moved.up = oneUp.matrix(rightDirection)
    rotBack = turn.back.90right(moved.up)
    return(rotBack)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ blue }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
