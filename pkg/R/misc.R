# last modified 16 January 2009 by J. Fox

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
	initializeDialog(title=gettext("Survival Data Definition", domain="R-RcmdrPlugin.survival"))
	onOK <- function(){
		activeDataSet <- ActiveDataSet()
		time <- getSelection(timeBox)
		if (length(time) == 1){
			time1 <- time
			time2 <- numeric(0)
		}
		else if (length(time) == 2){
			ss <- startStop(time)
			if (ss$error) errorCondition(recall=SurvivalData, 
					message=gettext("Start and stop times must be ordered.", 
						domain="R-RcmdrPlugin.survival"), model=TRUE)
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=SurvivalData, message=gettext("You must select one or two time variables.", 
					domain="R-RcmdrPlugin.survival"), model=TRUE)
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0) {
			errorCondition(recall=SurvivalData, message=gettext("You must select an event indicator.", 
					domain="R-RcmdrPlugin.survival"), model=TRUE)
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
	onRefresh <- function(){
		type <- as.character(tclvalue(clusterButtonsVariable))
		vars <- if (type == "all") Variables() else Factors()
		tkdelete(clusterBox$listbox, "0", "end")
		for (var in vars) tkinsert(clusterBox$listbox, "end", var)
		clusterBox$varlist <<- vars
		cmd <- paste('options(clusters="', if (type == "all") "all.variables" else "factors.only", '")', sep="")
		doItAndPrint(cmd)
		tkfocus(top)
	}
	OKCancelHelp(helpSubject="SurvivalData")
	survFrame <- tkframe(top)
	timeBox <- variableListBox(survFrame, Numeric(), title=gettext("Time or start/end times\n(select one or two)", 
			domain="R-RcmdrPlugin.survival"), selectmode="multiple")
	eventBox <- variableListBox(survFrame, Numeric(), title=gettext("Event indicator\n(select one)", 
			domain="R-RcmdrPlugin.survival"))
	strataBox <- variableListBox(survFrame, Factors(), title=gettext("Strata\n(select zero or more)", 
			domain="R-RcmdrPlugin.survival"), initialSelection=-1, selectmode="multiple")
	clusterBox <- variableListBox(survFrame, if (allVarsClusters()) Variables() else Factors(), 
		title=gettext("Clusters\n(optional)", domain="R-RcmdrPlugin.survival"), initialSelection=-1)
	radioButtons(survFrame, name="clusterButtons",
		buttons=c("factors", "all"), initialValue=if (allVarsClusters()) "all" else "factors",
		labels=gettext(c("Factors only", "All variables"), domain="R-RcmdrPlugin.survival"), 
		title=gettext("Candidates for clusters", domain="R-RcmdrPlugin.survival"))
	refresh <- tkbutton(survFrame, text=gettext("Refresh cluster candidates", domain="R-RcmdrPlugin.survival"),
		command=onRefresh)
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="nw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), labelRcmdr(survFrame, text="  "), getFrame(clusterBox), sticky="nw")
	tkgrid(labelRcmdr(survFrame, text=""), labelRcmdr(survFrame, text=""), clusterButtonsFrame, sticky="w")
	tkgrid(labelRcmdr(survFrame, text=""), labelRcmdr(survFrame, text=""), refresh, sticky="w")
	tkgrid(survFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=12, columns=1)
}

allVarsClusters <- function(){
	opt <- match.arg(getOption("clusters"), c("factors.only", "all.variables"))
	opt == "all.variables"
	
}
