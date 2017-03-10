
library("ergm")
library("RDSdevelopment")
library("statnet")


load("/Users/klange/Desktop/maggies_masters_thesis_current/Data/florentine.RData")
data(fauxmadrona)
load("/Users/klange/Desktop/maggies_masters_thesis_current/Data/Project90/project90core.RData")

source("/Users/klange/Desktop/maggies_masters_thesis_current/Code/R scripts/testRDS.R")

iterations = 1000

#timing checks-make sure I am getting the results reported in the thesis still
cat("Comparing the R and the C code \n")
test_C_estimates = c(rep(NA, iterations))
test_C_times = c(rep(NA, iterations))
test_R_estimates = c(rep(NA, iterations))
test_R_times = c(rep(NA, iterations))

#first the R code
for(i in 1:iterations){
	temp.result = testRDS("RCode", fauxmadrona.network, "disease")
	test_R_estimates[i] =  RDS.II.estimates(temp.result[[1]], "disease", subset=NULL)$estimate 
	test_R_times[i] = temp.result[[2]]
	cat("done with", i, "\n")
}
#then the C code
for(i in 1:iterations){
	temp.result = testRDS("CCode", fauxmadrona.network, "disease", 1)
	test_C_estimates[i] =  RDS.II.estimates(temp.result[[1]], "attrsample", subset=NULL)$estimate 
	test_C_times[i] = temp.result[[2]][1]
	cat("done with", i, "\n")
}


#did 1000 easily w/o crashing
for(i in 1:iterations){
	result = rdssampleC(flobusiness, "wealth", nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, prob.network.recall = 1, verbose=TRUE, debug=TRUE)
	cat("done with", i, "\n")
}

#did 1000 easily w/o crashing
for(i in 1:iterations){
	result = rdssampleC(flomarriage, "wealth", nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, prob.network.recall = 1, verbose=TRUE, debug=TRUE)
	cat("done with", i, "\n")
}

#fauxmadrona

for(i in 1:iterations){
	result = rdssampleC(fauxmadrona.network, "disease", nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, 1, verbose=TRUE, debug=TRUE)
	cat("done with", i, "\n")
}


#proj90
for(i in 1:iterations){
	result = rdssampleC(proj90, "disabled", nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, 1, verbose=TRUE, debug=TRUE)
	cat("done with", i, "\n")
}

#infinite
j=0
while(TRUE){
	result = rdssampleC(proj90, "disabled", nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, 1, verbose=TRUE, debug=TRUE)
	cat("done with", j, "\n")
	j = j + 1
}
