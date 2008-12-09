# last modified 8 December 2008 by J. Fox

CoxModel <-
	function(){
	require(survival)
	if (!activeDataSetP()) return()
	initializeDialog(title=gettextRcmdr("Cox-Regression Model"))
	.activeModel <- ActiveModel()
	currentModel <- if (!is.null(.activeModel))
			class(get(.activeModel, envir=.GlobalEnv))[1] == "coxph"
		else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), hasLhs=TRUE)
		if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
	}
	UpdateModelNumber()
	modelName <- tclVar(paste("CoxModel.", getRcmdr("modelNumber"), sep=""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width="20", textvariable=modelName)
	onOK <- function(){
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
		modelValue <- trim.blanks(tclvalue(modelName))
		ties <- as.character(tclvalue(tiesVariable))
		robust <- as.character(tclvalue(robustVariable))
		closeDialog()
		if (!is.valid.name(modelValue)){
			errorCondition(recall=CoxModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
			return()
		}
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
			putRcmdr("modelWithSubset", FALSE)
		}
		else{
			subset <- paste(", subset=", subset, sep="")
			putRcmdr("modelWithSubset", TRUE)
		}
		check.empty <- gsub(" ", "", tclvalue(rhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=CoxModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
			return()
		}
		if (is.element(modelValue, listCoxModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
				UpdateModelNumber(-1)
				CoxModel()
				return()
			}
		}
		formula <- paste("Surv(", time1, ",",
			if(length(time2) != 0) paste(time2, ",", sep=""),
			event, ") ~ ", tclvalue(rhsVariable), sep="")
		if (length(strata) > 0) formula <- paste(formula, " + strata(", paste(strata, collapse=","), ")", sep="")
		if (length(cluster) > 0) formula <- paste(formula, " + cluster(", cluster, ")", sep="")
		command <- paste("coxph(", formula, ', method="', ties, '"', 
			if (robust != "default") paste(", robust=", robust, sep=""),
			", data=", ActiveDataSet(), subset, ")", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep=""))
		activeModel(modelValue)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="coxph", model=TRUE)
	tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	survFrame <- tkframe(top)
	.activeDataSet <- ActiveDataSet()
	.numeric <- Numeric()
	.factors <- Factors()
	time1 <- eval(parse(text=paste('attr(', .activeDataSet, ', "time1")', sep="")))
	time1 <- if (!is.null(time1)) which(time1 == .numeric) - 1 
	time2 <- eval(parse(text=paste('attr(', .activeDataSet, ', "time2")', sep="")))
	time2 <- if (!is.null(time2)) which(time2 == .numeric) - 1 
	event <- eval(parse(text=paste('attr(', .activeDataSet, ', "event")', sep="")))
	event <- if (!is.null(event)) which(event == .numeric) - 1 
	strata <- eval(parse(text=paste('attr(', .activeDataSet, ', "strata")', sep="")))
	strata <- if (!is.null(strata)) which(is.element(.factors, strata)) - 1 else -1
	cluster <- eval(parse(text=paste('attr(', .activeDataSet, ', "cluster")', sep="")))
	cluster <- if (!is.null(cluster)) which(cluster == .factors) - 1 else -1
	timeBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Time or start/end times\n(select one or two)"),
		selectmode="multiple", initialSelection=if(is.null(time1)) NULL else c(time1, time2))
	eventBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Event indicator\n(select one)"),
		initialSelection=event)
	strataBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Strata\n(select zero or more)"), 
		selectmode="multiple", initialSelection=strata)
	clusterBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Clusters\n(optional)"), initialSelection=cluster)
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name="ties",
		buttons=c("efron", "breslow", "exact"), initialValue="efron",
		labels=gettextRcmdr(c("Efron", "Breslow", "Exact")), title=gettextRcmdr("Method for Ties"))
	radioButtons(optionsFrame, name="robust",
		buttons=c("default", "TRUE", "FALSE"), initialValue="default",
		labels=gettextRcmdr(c("Default", "Yes", "No")), title=gettextRcmdr("Robust Standard Errors"))
	modelFormula(hasLhs=FALSE)
	subsetBox(model=TRUE)
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), labelRcmdr(survFrame, text="  "), getFrame(clusterBox), sticky="sw")
	tkgrid(survFrame, sticky="w")
	tkgrid(tiesFrame, labelRcmdr(optionsFrame, text="                 "), robustFrame, sticky="new")
	tkgrid(optionsFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(getFrame(xBox), sticky="w", columnspan=2)
	tkgrid(labelRcmdr(outerOperatorsFrame, text="         "), operatorsFrame, sticky="w")
	tkgrid(outerOperatorsFrame, sticky="ew")
	tkgrid(formulaFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=12, columns=1, focus=rhsEntry, preventDoubleClick=TRUE)
}

