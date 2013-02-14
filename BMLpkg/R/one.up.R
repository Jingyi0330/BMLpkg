one.up <-
function(vec){
	if(length(vec) <= 1) return(vec)
	else c(vec[2:length(vec)],0)
}
