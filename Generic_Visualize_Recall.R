#load workspace

library("ergm")
library("RDSdevelopment")
library("statnet")



make_short <- function(long.name){
	# Converts string of type "VH.results.disabled.90" to "VH 90"
	# Args:
	# long.name: name of the form [estimator].results.[variable].[percent recall]
	#
	# Returns:
	# Name of the form [estimator][space][percent recall]
	
	name.components = strsplit(long.name, "[.]")
	name.length = length(name.components[[1]])
	first.component=name.components[[1]][1]
  cat(first.component, "\n")
	if(first.component=="VH"){
		return(paste("V-H", name.components[[1]][name.length]))
	} else if (first.component=="SH"){
		return(paste("S-H", name.components[[1]][name.length]))
	} else {
		return(paste(first.component, name.components[[1]][name.length]))
	}
}

latex_friendly <- function(vars){
  num.vars = length(vars)
  new.vars = c(rep(NA, num.vars))
  for(i in 1:num.vars){
    name.components = strsplit(vars[i], "[.]")
    new.vars[i] = paste(name.components[[1]], collapse="_")
  }
  return(new.vars)
}



generate_pdfs <- function(data.path, data.file.name, network.name, var.names, estimator.names, recall.names, num.per.boxplot, boxplots.per.pdf){
	
	# Uses data to write pdfs to a file in the same folder as the data.
	#
	# Args:
	#
	# data.path: folder in which file of type .RData is contained
	# data.file.name: name of data file.
	# network.name: name of network under analysis
	# var.names: vector containing names of all variables to be treated
	# estimator.names: vector containing names of all estimators to be treated.
	# recall.names: vector containing names of all recall error values
	# number of individual distributions displayed per plot.  Right now this number
	# must be the same for all plots
	# number of plots per pdf
	#
	# Note: see below for example of how function should be used.
	# Note: right now there is not robust error checking for this function.
	# Rather it is simply assumed that arguments and data objects are correctly named according to 
	# fixed conventions. 
	#
	#
	
	
	total.path = paste(data.path, data.file.name, sep="")
	load(total.path)
	var.length = length(var.names)
	estimator.length = length(estimator.names)
	num.estimators = var.length * estimator.length
	estimator.list = c(rep(NA, num.estimators))
	for(i in 1:var.length){
		for(j in 1:estimator.length){
			estimator.list[((i - 1) * estimator.length) + j] = paste(estimator.names[j], "results", var.names[i], recall.names[j], sep=".")
		}
	}
	#calculate the population means
	true.vars = c(rep(NA, var.length))
	for(w in 1:var.length){
		true.var = get(paste("true", var.names[w], sep="."))
		true.vars[w] = true.var
	}
	
	
	num.boxplots = length(estimator.list)/num.per.boxplot
	boxplots = vector("list", length=num.boxplots)
	
	
	for(k in 1:num.boxplots){
		boxplot.estimators = vector("list", length = num.per.boxplot)
		boxplot.names = c(rep(NA, num.per.boxplot))
		for(m in 1:num.per.boxplot){
			estimator.name = estimator.list[((k-1)*num.per.boxplot) + m]
			short.name = make_short(estimator.name)
			boxplot.names[m] = short.name 			
			estimator.object = get(estimator.name)
			boxplot.estimators[[m]] = estimator.object
			
		}
	 
		boxplots[[k]] = boxplot(boxplot.estimators, names = boxplot.names, las=1, xlab="EstimatorType", cex.lab = "0.50",	cex.axis="0.50")
	}
	num.pdfs = num.boxplots/boxplots.per.pdf
	#right now 1 variable per pdf
	#num.pdfs = var.length (should hold)
	for(r in 1:num.pdfs){
    alt.var.names = latex_friendly(var.names)
		file.name = paste(alt.var.names[r], "pdf", sep=".")
		file.path = paste(data.path, file.name, sep="")
		pdf(file=file.path)
		par(mfrow=c(1, boxplots.per.pdf))
		ybottom = 0
    #calculate max
    ytop = 0
		for(p in 1:boxplots.per.pdf){
		  index = ((r-1) * boxplots.per.pdf) + p
      newtop = max(boxplots[[index]]$stats[, num.per.boxplot])
      ytop = max(ytop, newtop)
		}
    ytop = max(ytop, true.vars[r])
    for(p in 1:boxplots.per.pdf){
			index = ((r-1) * boxplots.per.pdf) + p
			bxp(boxplots[[index]], outline=FALSE, show.names=TRUE, cex.axis = "1.0", las=2, ylim=c(ybottom, ytop))
			ticks = signif(true.vars[r], 2)
			axis(side = 2, at = ticks, col="red", col.axis= "red", las=1, cex.axis="1.0")
			abline(h=ticks, col = "red", col.axis = "red")
		
		}
		title.string = paste("Comparison of Estimators on", network.name, "Population:", var.names[r])
		mtext(title.string, adj=1)
		dev.off()
	}
}


	
	

