pkgname <- "BMLpkg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('BMLpkg')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Blue")
### * Blue

flush(stderr()); flush(stdout())

### Name: Blue
### Title: Blue Car Moves
### Aliases: Blue
### Keywords: blue ~kwd2

### ** Examples

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



cleanEx()
nameEx("Drive")
### * Drive

flush(stderr()); flush(stdout())

### Name: Drive
### Title: All Car Moves
### Aliases: Drive
### Keywords: BML ~kwd2

### ** Examples

Drive(50, 80, 90, 0.7)



cleanEx()
nameEx("Red")
### * Red

flush(stderr()); flush(stdout())

### Name: Red
### Title: Red Car Moves
### Aliases: Red
### Keywords: red ~kwd2

### ** Examples

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



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
