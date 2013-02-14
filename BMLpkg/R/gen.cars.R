gen.cars <-
function(hor.grids, ver.grids, rho){
	total.grids = hor.grids*ver.grids
	red <- blue <- rho/2
	gen = sample(0:2, size=total.grids, replace=TRUE, prob=c((1-rho), red, blue))
	plane = matrix(gen, nrow=ver.grids, ncol=hor.grids)
}
