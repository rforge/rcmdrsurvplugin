# last modified 8 December 2008 by J. Fox

startStop <- function(time){
	times <- na.omit(eval(parse(text=paste(ActiveDataSet(), '[,c("', time[1], '", "', time[2],'")]', sep=""))))
	if (all(times[[time[1]]] <= times[[time[2]]])){
		return(list(start=time[1], stop=time[2], error=FALSE))
	} else if (all(times[[time[2]]] <= times[[time[1]]])){
		return(list(start=time[2], stop=time[1], error=FALSE))
	}
	else return(list(start="", stop="", error=TRUE))
}

padNA <- function(X, res){
	XX <- matrix(NA, length(res), ncol(X))
	colnames(XX) <- colnames(X)
	XX[!is.na(res), ] <- X
	XX
}