move.up.one1 <-
function(vec){
	one = which(vec==1)
	if(one==1){
		vec[2:length(vec)] = two.zero.and.more(vec[2:length(vec)])
		return(vec)}
	if(one==length(vec)){
		vec[1:(length(vec)-1)] = two.zero.and.more(vec[1:(length(vec)-1)])
		return(vec)}
	if(one!=1 && one!=length(vec)){
		if(vec[1]==2){
			vec[1]=0
			vec[1:(one-1)] = two.zero.and.more(vec[1:(one-1)])
			vec[(one+1):length(vec)] = two.zero.and.more(vec[(one+1):length(vec)])
			if(vec[length(vec)]==0) vec[length(vec)]=2
		return(vec)	
		}
		else{
		vec[1:(one-1)] = two.zero.and.more(vec[1:(one-1)])
		vec[(one+1):length(vec)] = two.zero.and.more(vec[(one+1):length(vec)])
		return(vec)
		}
	}
}
