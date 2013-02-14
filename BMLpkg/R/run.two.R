run.two <-
function(vec, ones){
	for(i in 1:(length(ones)-1)){
		if(0 %in% vec[(ones[i]):(ones[i+1])]){
			vec[(ones[i]+1):(ones[i+1]-1)] = two.zero.and.more(vec[(ones[i]+1):(ones[i+1]-1)])}
	}
	return(vec)
}
