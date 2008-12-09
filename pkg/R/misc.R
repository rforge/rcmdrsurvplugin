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

SurvivalData <- function(){
	if (!activeDataSetP()) return()
	initializeDialog(title=gettextRcmdr("Survival Data Definition"))
	onOK <- function(){
		activeDataSet <- ActiveDataSet()
		time <- getSelection(timeBox)
		if (length(time) == 1){
			time1 <- time
			time2 <- numeric(0)
		}
		else if (length(time) == 2){
			ss <- startStop(time)
			if (ss$error) errorCondition(recall=CoxModel, 
					message=gettextRcmdr("Start and stop times must be ordered."), model=TRUE)
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=CoxModel, message=gettextRcmdr("You must select one or two time variables."), model=TRUE)
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0) {
			errorCondition(recall=CoxModel, message=gettextRcmdr("You must select an event indicator."), model=TRUE)
			return()
		}
		strata <- getSelection(strataBox)
		cluster <- getSelection(clusterBox)
		closeDialog()
		command <- paste("attr(", activeDataSet, ', "time1") <- "', time1, '"', sep="")
		doItAndPrint(command)
		if (length(time2) > 0){
			command <- paste("attr(", activeDataSet, ', "time2") <- "', time2, '"', sep="")
			doItAndPrint(command)
		}
		command <- paste("attr(", activeDataSet, ', "event") <- "', event, '"', sep="")
		doItAndPrint(command)
		if (length(strata) > 0){
			command <- paste("attr(", activeDataSet, ', "strata") <- c(', paste(paste('"', strata, '"', sep=""), collapse=","), ')', sep="")
			doItAndPrint(command)
		}
		if (length(cluster) > 0){
			command <- paste("attr(", activeDataSet, ', "cluster") <- "', cluster, '"', sep="")
			doItAndPrint(command)
		} 
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="SurvivalData")
	survFrame <- tkframe(top)
	timeBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Time or start/end times\n(select one or two)"),
		selectmode="multiple")
	eventBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Event indicator\n(select one)"))
	strataBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Strata\n(select zero or more)"), 
		initialSelection=-1, selectmode="multiple")
	clusterBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Clusters\n(optional)"), initialSelection=-1)
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), labelRcmdr(survFrame, text="  "), getFrame(clusterBox), sticky="sw")
	tkgrid(survFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=12, columns=1)
}
