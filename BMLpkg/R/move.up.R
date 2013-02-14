move.up <-
function(vec){
	ones = which(vec == 1)
	if(length(ones)==0){
		vec = rotvec(vec)
		return(vec)
	}
	if(length(ones) == 1){move.up.one1(vec)}
	else{move.up.more1(vec)}
}
