Blue <-
function(matrix){
	rightDirection = turn.mat.90left(matrix);rightDirection
	moved.up = oneUp.matrix(rightDirection)
	rotBack = turn.back.90right(moved.up)
	return(rotBack)
}
