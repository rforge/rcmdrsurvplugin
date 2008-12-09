# last modified 8 December 2008 by J. Fox

Survfit <-
	function(){
	require(survival)
	if (!activeDataSetP()) return()
	currentModel <- FALSE
	initializeDialog(title=gettextRcmdr("Survival Function"))
	onOK <- function(){
		time <- getSelection(timeBox)
		if (length(time) == 1){
			time1 <- time
			time2 <- numeric(0)
		}
		else if (length(time) == 2){
			ss <- startStop(time)
			if (ss$error) errorCondition(recall=Survfit, 
					message=gettextRcmdr("Start and stop times must be ordered."), model=TRUE)
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=Survfit, message=gettextRcmdr("You must select one or two time variables."))
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0){
			errorCondition(recall=Survfit, message=gettextRcmdr("You must select an event indicator."))
			return()
		}
		strata <- getSelection(strataBox)
		type <- as.character(tclvalue(typeVariable))
		error <- as.character(tclvalue(errorVariable))
		conftype <- as.character(tclvalue(conftypeVariable))
		conf.int <- as.character(tclvalue(plotconfVariable))
		lev <- as.numeric(tclvalue(confidenceLevel))
		quants <- paste("c(", gsub(",+", ",", gsub(" ", ",", tclvalue(quantiles))), ")", sep="")
		closeDialog()
		if ((is.na(lev)) || (lev < 0) || (lev > 1)) {
			Message(gettextRcmdr("Confidence level must be a number between 0 and 1."))
			tkfocus(CommanderWindow())
			return()
		}
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
		if (length(strata) > 0) formula <- paste(formula, " ~ ", paste(strata, collapse=" + "), sep="")
		command <- paste("survfit(", formula, ', conf.type="', conftype, 
			'", conf.int=', lev, ', type="', type, '", error="', error,
			'", data=', ActiveDataSet(), subset, ")", sep="")
		logger(paste(".Survfit <- ", command, sep=""))
		assign(".Survfit", justDoIt(command), envir=.GlobalEnv)
		doItAndPrint("summary(.Survfit)")
		conf.int <- if (conf.int == "default") "" else paste(", conf.int=", conf.int, sep="") 
		if (length(strata) == 0) doItAndPrint(paste("plot(.Survfit", conf.int,  ")", sep=""))
		else{
			allstrata <- eval(parse(text=paste("with(", ActiveDataSet(), 
						", interaction(", paste(strata, collapse=","), "))")))
			levels <- levels(allstrata)
			nlevels <- length(levels)
			doItAndPrint(paste("plot(.Survfit, col=1:", nlevels,", lty=1:", nlevels, 
					', legend.text=c(', paste(paste('"', levels, '"', sep=""), collapse=","),
					')', conf.int, ')', sep=""))
		}
		doItAndPrint(paste("survQuantiles(.Survfit, quantiles=", quants, ")", sep=""))
		logger("remove(.Survfit)")
		remove(.Survfit, envir=.GlobalEnv)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="survfit", model=TRUE)
	survFrame <- tkframe(top)
	timeBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Time or start/end times\n(select one or two)"),
		selectmode="multiple")
	eventBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Event indicator\n(select one)"))
	strataBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Strata\n(select zero or more)"), 
		initialSelection=-1, selectmode="multiple")
	confidenceFrame <- tkframe(top)
	radioButtons(confidenceFrame, name="conftype",
		buttons=c("log", "loglog", "plain", "none"), 
		values=c("log", "log-log", "plain", "none"), initialValue="log",
		labels=gettextRcmdr(c("Log", "Log-log","Plain", "None")), title=gettextRcmdr("Confidence Intervals"))
	confidenceLevel <- tclVar(".95")
	confidenceFieldFrame <- tkframe(confidenceFrame)
	confidenceField <- ttkentry(confidenceFieldFrame, width="6", textvariable=confidenceLevel)
	radioButtons(confidenceFrame, name="plotconf",
		buttons=c("default", "yes", "no"), 
		values=c("default", "TRUE", "FALSE"), initialValue="default",
		labels=gettextRcmdr(c("Default", "Yes", "No")), title=gettextRcmdr("Plot confidence Intervals"))
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name="type",
		buttons=c("kaplanmeier","flemingharrington", "fh2"), 
		values=c("kaplan-meier","fleming-harrington", "fh2"), initialValue="kaplan-meier",
		labels=gettextRcmdr(c("Kaplan-Meier", "Fleming-Harrington", "Fleming-Harrington 2")),
		title=gettextRcmdr("Method"))
	radioButtons(optionsFrame, name="error",
		buttons=c("greenwood", "tsiatis"), initialValue="greenwood",
		labels=gettextRcmdr(c("Greenwood", "Tsiatis")), title=gettextRcmdr("Variance Method"))
	quantilesFrame <- tkframe(optionsFrame)
	quantilesVariable <- tclVar("1")
	quantiles <- tclVar(".25, .5, .75")
	quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)
	modelFormula(hasLhs=FALSE)
	subsetBox()
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), sticky="nw")
	tkgrid(survFrame, sticky="nw")
	tkgrid(labelRcmdr(confidenceFieldFrame, text=gettextRcmdr("Confidence level"), foreground="blue"), sticky="nw")
	tkgrid(confidenceField, sticky="nw")
	tkgrid(conftypeFrame, labelRcmdr(confidenceFrame, text="          "), plotconfFrame, 
		labelRcmdr(confidenceFrame, text="      "), confidenceFieldFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(confidenceFrame, sticky="nw")
	tkgrid(labelRcmdr(quantilesFrame, text=gettextRcmdr("Quantiles to estimate"), foreground="blue"), sticky="nw")
	tkgrid(quantilesEntry)
	tkgrid(typeFrame, labelRcmdr(optionsFrame, text="  "), errorFrame, 
		labelRcmdr(optionsFrame, text="         "), quantilesFrame, sticky="new")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(optionsFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=9, columns=1)
}

