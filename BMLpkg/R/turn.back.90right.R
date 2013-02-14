turn.back.90right <-
function(mat){
	list.back = tapply(mat,rep(1:ncol(mat),each=nrow(mat)),function(i)i)
	revert = lapply(list.back,rev)
	matrix(unlist(revert),byrow = TRUE,nrow=length(revert))
}
