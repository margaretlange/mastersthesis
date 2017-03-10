#okay, I find that the code is crashing on say 1/100 or 1/500 samples; not sure why
#I want to diagnose this on Proj 90
#change this to code workout

iterations = 1000

library("ergm")
library("RDSdevelopment")
library("statnet")


load("/Users/Margaret/Desktop/masters_thesis/Data/florentine.RData")
data(fauxmadrona)
load("/Users/Margaret/Desktop/masters_thesis/Data/Project90/project90core.RData")
#using my check function on the Proj 90 data
#load check file

source("/Users/Margaret/Desktop/masters_thesis/Code/R scripts/checks.R")
load("/Users/Margaret/Desktop/masters_thesis/results/proj90_sept_2013.RData")


#display flobusiness and flomarriage
set.vertex.attribute(flobusiness, "number_label", c(1:16))
set.vertex.attribute(flomarriage, "number_label", c(1:16))

plot.network(flomarriage, label = get.vertex.attribute(flomarriage, attrname = "number_label"), displaylabels = TRUE)
plot.network(flobusiness, label = get.vertex.attribute(flobusiness, attrname = "number_label" ), displaylabels = TRUE)

#tests
#flobusiness and flomarriage

#small samples
result = rdssampleC(flobusiness, nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, prob.network.recall = 1, verbose=TRUE)

result = rdssampleC(flobusiness, nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, nsims=3, verbose=TRUE)

result = rdssampleC(flomarriage, nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, nsims=3, verbose=TRUE)
result = rdssampleC(flomarriage, nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, nsims=1, verbose=TRUE)


rdssampleC(fauxmadrona.network, nsamp0=2, fixinitial=-1, nsamp =10, coupons=1, nsims = 4, verbose=TRUE)



#larger sample

result = rdssampleC(fauxmadrona.network, nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, verbose=TRUE)

time = system.time(result <- rdssampleC(fauxmadrona.network, nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, verbose=TRUE))

result = rdssampleC(fauxmadrona.network, nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 2, verbose=TRUE)
result = rdssampleC(fauxmadrona.network, nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, prob.network.recall = 0.9, verbose=TRUE)

#testing network recall and rank estimates
#not working, since the returned result is not a data frame


RDS.II.estimates(result, "disease", subset=NULL)


Rank.estimates(result, "disease", subset=NULL)
Rank.estimates(result, "disease", subset=NULL)


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

j=0
while(TRUE){
	result = rdssampleC(proj90, "disabled", nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, 1, verbose=TRUE, debug=TRUE)
	cat("done with", j, "\n")
	j = j + 1
}
#project90
iterations= length(results.100)
test.90 = c(rep(NA, iterations))
test.100 = c(rep(NA, iterations))

# for(i in 1:length(results.100)){
	# result = rdssampleC(fauxmadrona.network, "disease", nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, 1, verbose=TRUE, debug=TRUE)
	# check = checkSample(result[[1]], result[[2]], result[[3]], 10, 2, 500)
	# cat("done with", i, "\n")
	# cat(all(as.logical(check)))
# }

#having trouble getting object

for(i in 1:iterations){
	check = checkSample(as.data.frame(results.100[i])[ ,1], as.data.frame(results.100[i])[ ,2], NULL, 10, 2, 500)
	cat("done with", i, "\n")
	test.100[i] = all(as.logical(check), na.rm=TRUE)
}
all(test.100)

for(i in 1:iterations){
	check = checkSample(as.data.frame(results.90[i])[ ,1], as.data.frame(results.90[i])[ ,2], NULL, 10, 2, 500)
	cat("done with", i, "\n")
	test.90[i] = all(as.logical(check), na.rm=TRUE)
}
all(test.90)

#a series of checks of the sample generated

#function

library("ergm")
library("RDSdevelopment")
library("statnet")


load("/Users/Margaret/Desktop/masters_thesis/Data/florentine.RData")
data(fauxmadrona)
load("/Users/Margaret/Desktop/masters_thesis/Data/Project90/project90core.RData")



checkSample <- function(recruited, recruiters, times, numSeeds, numCoupons, sampSize, reseedInfo = FALSE, timeInfo = FALSE){
	
	# Performs some basic checks on the results of a RDS simulation using Margaret 
	# Lange's C code
	#
	# Args:
	# recruited: vector of individuals recruited
	# recruiters: vector of recruiters for individuals in recruited
	# time: vector of times for vectors recruited and recruiters
	# numSeeds: number of seeds in the RDS simulation
	# numCoupons: number of coupons in the RDS simulation
	# sampSize: size of the sample
	# reseedInfo: if TRUE, function will examine reseed data
	# timeInfo: if TRUE, function will examine time data
	#
	# Returns:
	# A list that represents the results of the check
	# As soon as the function finds a problem, it returns.  Thus if a sample has
	# more than one problem, the function will not immediately identify it.
	# If there are no problems, a full list of indicators showing that all checks
	# are complete will be returned.
	# Elements of the list may include:
	# times.result: TRUE is times are in ascending order, FALSE if they are not,
	#	NA if time check is not required
	# reseed.result = TRUE if reseeding occurred, FALSE if it did not, NA if 
	# reseed check is not required
	# coupons.result = FALSE if any recruiter has exceeded number of coupons,
	# TRUE if everything is okay
	# sample.result = TRUE if the sample is the required size, FALSE otherwise
	# recruited.result = TRUE if no one is recruited more than once, FALSE
	# if someone shows up more than once (a mistake)
	# order.result = TRUE if each recruited has a recruiter who was recruited earlier or is 0.
	# = FALSE if otherwise (a mistake)
	# In general: TRUE if sample is okay, except for reseed.result, which tells whether or
	# not reseeding occurred. 	
	
	#check times are in ascending order
	if(timeInfo == TRUE){
		times.result = times[-1] > times[-length(times)]
		times.result = all(times.result)
		if(times.result == FALSE){
			return(list(time = times.result))
		}
	} else {
		times.result = NA
	}
	
	
	
	#see if reseeding occurred
	if(reseedInfo == TRUE){
		seed.count = sum(recruiters == 0)
		if(seed.count > numSeeds) {
			reseed.result = TRUE	
		} else {
			reseed.result = FALSE
		}
	} else{
		reseed.result = NA	
	}

	#no recruiter should show up more than number of coupons (except 0)
    recruiters.count = table(recruiters)
    recruiters.count = recruiters.count[-1] #gets rid of seeds (there has to be at least one)
    coupons.result = recruiters.count < numCoupons + 1
    coupons.result = all(coupons.result)
    if(coupons.result == FALSE){
    	return(list(time = times.result, coupons = coupons.result))
    }
	
	#sample is the right size
	if(length(recruited) == sampSize){
		sample.result = TRUE
	} else {
		sample.result = FALSE
	}
	if(sample.result == FALSE){
		return(list(time = times.result, coupons = coupons.result, sample.size = sample.result))
	}
	
	#each recruited should only show up once
	recruited.count = table(recruited)
	recruited.result = recruited.count == 1
	recruited.result = all(recruited.result)
	if(recruited.result == FALSE){
		return(list(time = times.result, coupons = coupons.result, recruited = recruited.result, sample.size = sample.result))
	}
	
	
	#each recruited should have recruiter who was recruited earlier or is 0 	
	#for loop for now, if there is another way I'll figure it out later
	order.result = TRUE
	for(i in 1:sampSize)
	{
		temp.recruiter = recruiters[i]
		if(temp.recruiter == 0){
			 order.result = TRUE
		} else if(sum(recruited[-(i:length(recruited))] == temp.recruiter) > 0) {
			 order.result = TRUE
		} else {
			order.result = FALSE
		}
	}
	
	
	return(list(time = times.result, reseed = reseed.result, coupons = coupons.result, recruited = recruited.result, sample.size = sample.result, sample.order = order.result))
	
}

#main code

#randomly generate vectors

#try with flobusiness

#result = rdssampleC(flobusiness, "wealth", nsamp0=2, fixinitial = -1, nsamp = 10, coupons=1, prob.network.recall = 1, verbose=TRUE, debug=TRUE)
#try fauxmadrona
#check = checkSample(result[[1]], result[[2]], result[[3]], 2, 1, 10)
# iterations=10
# for(i in 1:iterations){
	# result = rdssampleC(fauxmadrona.network, "disease", nsamp0=10, fixinitial=-1, nsamp =500, coupons=2, nsims = 1, fast=TRUE, 1, verbose=TRUE, debug=TRUE)
	# check = checkSample(result[[1]], result[[2]], result[[3]], 10, 2, 500)
	# cat("done with", i, "\n")
	# cat(all(as.logical(check), na.rm=TRUE))
# }


# #seeing if code will catch mistakes
# fake.recruited = c(11, 5, 3, 8, 9, 6, 4, 7, 16, 14)
# fake.recruiters = c(0, 11, 0, 5, 3, 9, 8, 4, 0, 0)
# fake.times = c(0.6264617, 0.7424437, 2.2427993, 2.3045142, 2.7503291, 4.0050720, 6.5815342, 8.1244733, 9.2091837, 10.3639587)


# check.try = checkSample(fake.recruited, fake.recruiters, fake.times, 2, 1, 10)

# #code I'm using to test the function
# recruited = result[[1]]
# recruiter = result[[2]]


