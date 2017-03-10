library("ergm")
library("RDSdevelopment")
library("statnet")




#Function

testRDS <- function(my.code, my.network, my.variable, my.prob.network.recall){

	if(my.code == "RCode") {
		result = create.rds.sampler(my.network, n.seeds=10, target.sample.size=500, n.coupons=2)
		time=system.time(result$do.sampling())[3]
		rds.data = result$get.data()
	} else {
		#decide whether "fast" should be "true" or "false"
		time = system.time(result <- rdssampleC(my.network, my.variable, nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, prob.network.recall=my.prob.network.recall, verbose=TRUE))
	}
	#the next chunk of code is just to transform the result into an rds.data.frame
	#do I need a network attribute for the rds.data.frame object
	#wait-I think I could rewrite this using the function do.simulation
	if(my.code == "RCode"){
		all.variable = my.network %v% my.variable  #seems ok
		all.degrees = degree(my.network, cmode="outdegree") #seems ok
		#I need to install statnet for this function
		temp.variable =   all.variable[rds.data[ ,1]]            
		degree = all.degrees[rds.data[ ,1]]
		rds.data = cbind(rds.data, degree)
		rds.data = cbind(rds.data, temp.variable)
		length = dim(rds.data)[2]
		names(rds.data)[length] = my.variable
		rds.data = as.rds.data.frame(rds.data, id="id", recruiter.id = "recruiter.id", network.size="degree")
	} else {
		rds.data = as.data.frame(result)
		rds.data = as.rds.data.frame(rds.data, id="nsample", recruiter.id = "nominators", network.size = "degsample")
	}
	return(list(rds.data, time))
}



