swap <-
function(mat){
	mat[which(mat==1)] <- 3
	mat[which(mat==2)] <- 1
	mat[which(mat==3)] <- 2
	return(mat)
}
