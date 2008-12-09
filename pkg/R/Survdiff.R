# last modified 8 December 2008 by J. Fox

# can't allow counting-process data?

Survdiff <-
	function(){
	require(survival)
	if (!activeDataSetP()) return()
	currentModel <- FALSE
	initializeDialog(title=gettextRcmdr("Compare Survival Functions"))
	onOK <- function(){
		time <- getSelection(timeBox)
		if (length(time) == 1){
			time1 <- time
			time2 <- numeric(0)
		}
		else if (length(time) == 2){
			ss <- startStop(time)
			if (ss$error) errorCondition(recall=Survdiff, 
					message=gettextRcmdr("Start and stop times must be ordered."), model=TRUE)
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=Survdiff, message=gettextRcmdr("You must select one or two time variables."))
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0) {
			errorCondition(recall=Survdiff, message=gettextRcmdr("You must select an event indicator."))
			return()
		}
		strata <- getSelection(strataBox) 
		if (length(strata) == 0) {
			errorCondition(recall=Survdiff, message=gettextRcmdr("You must select strata."))
			return()
		}
		rho <- tclvalue(rhoValue)
		closeDialog()
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
		}
		else{
			subset <- paste(", subset=", subset, sep="")
		}
		formula <- paste("Surv(", time1, ",",
			if(length(time2) != 0) paste(time2, ",", sep=""),
			event, ")", sep="")
		formula <- paste(formula, " ~ ", paste(strata, collapse=" + "), sep="")
		command <- paste("survdiff(", formula, ", rho=", rho,
			', data=', ActiveDataSet(), subset, ")", sep="")
		doItAndPrint(command)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="survdiff", model=TRUE)
	survFrame <- tkframe(top)
	timeBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Time or start/end times\n(select one or two)"),
		selectmode="multiple")
	eventBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Event indicator\n(select one)"))
	strataBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Strata\n(select one or more)"), 
		initialSelection=-1, selectmode="multiple")
	rhoFrame <- tkframe(top)
	rhoValue <- tclVar("0")
	rhoSlider <- tkscale(rhoFrame, from=0, to=1, showvalue=TRUE, variable=rhoValue,
		resolution=0.1, orient="horizontal")
	modelFormula(hasLhs=FALSE)
	subsetBox()
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), sticky="nw")
	tkgrid(survFrame, sticky="nw")
	tkgrid(labelRcmdr(rhoFrame, text="rho", foreground="blue"), rhoSlider, sticky="sw")
	tkgrid(rhoFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=9, columns=1)
}

