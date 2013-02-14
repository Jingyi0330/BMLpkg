Drive <-
function(time,hgrid, vgrid, rho,...){
	carPlane = gen.cars(hgrid, vgrid, rho)
	#mat = carPlane
	class(carPlane) = "BML"
	png(file = "Anna0")	
	print(plot(carPlane))
	dev.off()
	
	Annas = rep("Anna",time)
	Annas = paste(Annas, 1:time, sep="")
	
	moved = c()
	unmoved = c()
	for(i in 1:time){
		if(i %% 2 !=0){
			plane = Blue(carPlane)
			moved[i] = length(which((carPlane == plane)==FALSE))
			unmoved[i] = hgrid*vgrid*rho - moved[i]
			carPlane = plane
			class(carPlane) = "BML"
			png(Annas[i])
			print(plot(carPlane))
			dev.off()
		}
		if(i %% 2 ==0){
			plane = Red(carPlane)
			moved[i] = length(which((carPlane == plane)==FALSE))
			unmoved[i] = hgrid*vgrid*rho - moved[i]
			carPlane = plane
			class(carPlane) = "BML"
			png(Annas[i])
			print(plot(carPlane))
			dev.off()
		}
	}
	par(mfrow=c(1,2))
	(plot(seq(time),moved,type="l",xlab = "Time",ylab="Number of Car Moves",main="Number of Car Moves at Each Time Point"))
	(plot(seq(time),unmoved,type="l",xlab = "Time",ylab="Number of Car Unmoves",main="Number of Car Unmoves at Each Time Point"))
}
