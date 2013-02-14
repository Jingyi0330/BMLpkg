two.zero.and.more <-
function(part){
	if(all(part==2) || length(part) <= 1) return(part)
	else {
		first.zero = min(which(part == 0))
		part[first.zero:length(part)] = one.up(part[first.zero:length(part)])
		return(part)
	}
}
