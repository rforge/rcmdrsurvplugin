# last modified 8 December 2008 by J. Fox

survregModel <-
function(){
	## notes: robust=TRUE causes errors 
	## counting-process form of Surv() doesn't seem to work
  require(survival)
	initializeDialog(title=gettextRcmdr("Survival Regression Model"))
	.activeModel <- ActiveModel()
	currentModel <- if (!is.null(.activeModel))
				class(get(.activeModel, envir=.GlobalEnv))[1] == "survreg"
			else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), hasLhs=TRUE)
		if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
	}
	UpdateModelNumber()
	modelName <- tclVar(paste("SurvregModel.", getRcmdr("modelNumber"), sep=""))
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
			if (ss$error) errorCondition(recall=survregModel, 
					message=gettextRcmdr("Start and stop times must be ordered."), model=TRUE)
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=survregModel, message=gettextRcmdr("You must select one or two time variables."), model=TRUE)
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0) {
			errorCondition(recall=survregModel, message=gettextRcmdr("You must select an event indicator."), model=TRUE)
			return()
		}
		strata <- getSelection(strataBox)
		cluster <- getSelection(clusterBox)
		modelValue <- trim.blanks(tclvalue(modelName))
		robust <- as.character(tclvalue(robustVariable))
		dist <- as.character(tclvalue(distributionVariable))
		closeDialog()
		if (!is.valid.name(modelValue)){
			errorCondition(recall=survregModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
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
			errorCondition(recall=survregModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
			return()
		}
		if (is.element(modelValue, listSurvregModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
				UpdateModelNumber(-1)
				survregModel()
				return()
			}
		}
		formula <- paste("Surv(", time1, ",",
				if(length(time2) != 0) paste(time2, ",", sep=""),
				event, ") ~ ", tclvalue(rhsVariable), sep="")
		if (length(strata) > 0) formula <- paste(formula, " + strata(", paste(strata, collapse=","), ")", sep="")
		if (length(cluster) > 0) formula <- paste(formula, " + cluster(", cluster, ")", sep="")
		command <- paste("survreg(", formula, ', dist="', dist, '"',
				if (robust != "default") paste(", robust=", robust, sep=""),
				", data=", ActiveDataSet(), subset, ")", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep=""))
		activeModel(modelValue)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="survreg", model=TRUE)
	tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	survFrame <- tkframe(top)
	timeBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Time or start/end times\n(select one or two)"),
		selectmode="multiple")
	eventBox <- variableListBox(survFrame, Numeric(), title=gettextRcmdr("Event indicator\n(select one)"))
	strataBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Strata\n(select zero or more)"), 
		initialSelection=-1, selectmode="multiple")
	clusterBox <- variableListBox(survFrame, Factors(), title=gettextRcmdr("Clusters\n(optional)"), initialSelection=-1)
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name="distribution",
		buttons=c("weibull", "exponential", "gaussian", "logistic", "lognormal", "loglogistic"), initialValue="weibull",
		labels=gettextRcmdr(c("Weibull", "Exponential", "Gaussian", "Logistic", "Log-normal", "Log-logistic")),
		title=gettextRcmdr("Distribution"))
	radioButtons(optionsFrame, name="robust",
		buttons=c("default", "TRUE", "FALSE"), initialValue="default",
		labels=gettextRcmdr(c("Default", "Yes", "No")), title=gettextRcmdr("Robust Standard Errors"))
  	modelFormula(hasLhs=FALSE)
	subsetBox(model=TRUE)
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), labelRcmdr(survFrame, text="  "), getFrame(clusterBox), sticky="sw")
	tkgrid(survFrame, sticky="w")
	tkgrid(distributionFrame, labelRcmdr(optionsFrame, text="                    "), robustFrame, sticky="new")
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

