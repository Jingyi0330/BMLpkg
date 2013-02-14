one.up <-
function(vec){
	vec_len = length(vec)
	out <- .C("oneUp", vec = as.integer(vec), vec_len = as.integer(vec_len))
	out$vec
}
