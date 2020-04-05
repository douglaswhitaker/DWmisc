pprint <- function(..., sep="", innersep=" ", outersep1="'", outersep2="'"){
	toPrint <- c();
	for (ii in 1:length(list(...))){
		# print a normal object like text (e.g. "testing ")
		if (length(list(...)[[ii]]) == 1){
			toPrint <- paste(toPrint, list(...)[[ii]], sep=sep);
			}
		# printing a vector (e.g. x <- c(1:10))
		else if (length(list(...)[[ii]]) > 1) {
			tempString <- paste(outersep1, list(...)[[ii]][1], sep="");
			for (jj in 2:length(list(...)[[ii]]))
				tempString <- paste(tempString, list(...)[[ii]][jj], sep=innersep);
			tempString <- paste(tempString, outersep2, sep="");
			toPrint <- paste(toPrint, tempString, sep="");
			}
		}
	print(toPrint);
	}
