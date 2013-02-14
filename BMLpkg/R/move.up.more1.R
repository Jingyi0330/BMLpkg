move.up.more1 <-
function(vec){
	ones = which(vec==1)
	if(vec[1]==1 && vec[length(vec)]==1){
		vec = run.two(vec,ones)
		return(vec)
	}
	if(vec[1]==1 && vec[length(vec)]!=1){
		vec[(ones[length(ones)]+1):(length(vec))] = two.zero.and.more(vec[(ones[length(ones)]+1):(length(vec))])
		vec = run.two(vec,ones)
		return(vec)
	}
	if(vec[1]!=1 && vec[length(vec)]==1){
		vec[1:(ones[1]-1)] = two.zero.and.more(vec[1:(ones[1]-1)])
		vec = run.two(vec,ones)
		return(vec)
	}
	if(vec[1]==2 && vec[length(vec)]!=1){
		vec[(ones[length(ones)]+1):(length(vec))] = two.zero.and.more(vec[(ones[length(ones)]+1):(length(vec))])
		if(vec[length(vec)]==0){
		vec[1]=0
		vec[1:(ones[1]-1)] = two.zero.and.more(vec[1:(ones[1]-1)])
		vec[length(vec)]=2}
		vec = run.two(vec,ones)
		return(vec)
	}
	else{	#(vec[1]!=1 && vec[length(vec)]!=1)
		vec[(ones[length(ones)]+1):(length(vec))] = two.zero.and.more(vec[(ones[length(ones)]+1):(length(vec))])
		vec[1:(ones[1]-1)] = two.zero.and.more(vec[1:(ones[1]-1)])
		vec = run.two(vec,ones)
		return(vec)
	}
}
