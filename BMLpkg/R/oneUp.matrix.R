oneUp.matrix <-
function(mat){
	li = tapply(mat,rep(1:ncol(mat),each=nrow(mat)),function(i)i)
	sapply(li, move.up)
}
