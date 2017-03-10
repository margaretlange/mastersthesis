library("ergm")
library("RDSdevelopment")
library("statnet")
library("xtable")


generate_MSE <- function(data.path, data.file.name, network.name, var.names, estimator.names, recall.names){
	
	# Uses data to write a table of mean-squared errors to a file in the same folder as the data.
	#
	# Args:
	#
	# data.path: folder in which file of type .RData is contained
	# data.file.name: name of data file.
	# network.name: name of network under analysis
	# var.names: vector containing names of all variables to be treated
	# estimator.names: vector containing names of all estimators to be treated.
	# recall.names: vector containing names of all recall error values
	#
	# Note: see below for example of how function should be used.
	# Note: right now there is not robust error checking for this function.
	# Rather it is simply assumed that arguments and data objects are correctly named according to 
	# fixed conventions. 
	#
	#
	
	total.path = paste(data.path, data.file.name, sep="")
	load(total.path)

	MSE.results = data.frame()
	short.names = c(rep(NA, length(estimator.names)))
	short.names.alt = c(rep(NA, length(estimator.names)))
	for(i in 1:length(short.names)){
		short.names[i] = paste(estimator.names[i], recall.names[i])
    short.names.alt[i] = paste(estimator.names[i], recall.names[i], sep="_")
	}
	for(j in 1: length(var.names)){
		new.mse = numeric()
		for(k in 1:length(estimator.names)){
			sim.name = paste(estimator.names[k], "results", var.names[j], recall.names[k], sep=".")
			sim = get(sim.name)
			iterations = length(sim)
			sim.mean = mean(sim, na.rm=TRUE)
			true.mean.name = paste("true", var.names[j], sep=".")
			true.mean = get(true.mean.name)
			mse = (sum((sim - true.mean)^2, na.rm=TRUE))/iterations
      mse = sqrt(mse) * 100
			new.mse = c(new.mse, mse)		
		}
		MSE.results = rbind(MSE.results, new.mse)
	}		
		
	names(MSE.results) = short.names
	row.names(MSE.results) = var.names
	display.names = c(rep("f", times= length(short.names))) 
  display.names = c("s", display.names)	
  file.name = paste("MSE", paste(short.names.alt, collapse="_"), sep="_")
  cat(file.name)
  file.name = paste(file.name, "tex", sep=".")
	file.path = paste(data.path, file.name, sep="")
	fileConn <- file(description=file.path, open="w")
	table.text <- print(xtable(MSE.results, display=display.names))
	writeLines(table.text, con=fileConn)
  	close(fileConn)
}


