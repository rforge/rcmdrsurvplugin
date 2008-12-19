# last modified 19 December 2009 by J. Fox

plot.coxph <- function(x, newdata, typical=mean, byfactors=FALSE, col=palette(), lty, conf.level=0.95, ...){
	vars <- all.vars(formula(x)[-1])
	XX <- X <- na.omit(expand.model.frame(x, vars)[,vars]) 
	terms <- attr(terms(x), "term.labels")
	tstrata <- grep("strata\\(", terms)
	if (byfactors || !missing(newdata)){
		if (missing(newdata)){
			newdata <- list()
			names <- names(X)
			if (length(tstrata) > 0){
				strata <- gsub("strata\\(|\\)", "", terms[tstrata])
				strata <- strsplit(strata, "[\\ \\,]+")[[1]]
			}
			else strata <- NULL
			names <- setdiff(names, strata)
			X <- X[,names]
			for (i in 1:ncol(X)){
				newdata[[names[i]]] <- if (inherits(X[[i]], "factor")) levels(X[[i]])
					else typical(X[[i]])
			}
			newdata <- expand.grid(newdata)
		}
		S <- survfit(x, conf.int=conf.level, newdata=newdata)
		newdata <-  as.data.frame(lapply(newdata, function(x) if(is.factor(x)) x else signif(x, 4)))
		if (missing(lty)) lty <- 1:((1 + (length(tstrata) > 0))*nrow(newdata))
		plot(S, col=col, lty=lty, 
			main=paste("Survival by", paste(names(newdata), collapse=", ")), 
			xlab="time", ylab="survival", ...)
		if (length(tstrata)> 0){
			levels <- levels(with(XX, eval(parse(text=terms[tstrata]))))
			each <- nrow(newdata)
			new <- newdata
			for (i in 1:(length(levels) - 1)) newdata <- rbind(newdata, new)
			newdata <- cbind(newdata, Strata=rep(levels, each=each))
		}
		legend("bottomleft", legend=apply(newdata, 1, function(x)  paste(x, collapse=", ")),
			col=col, lty=lty, title=paste("          ", paste(names(newdata), collapse=", ")), bty="n")
	}
	else {
		terms <- attr(terms(x), "term.labels")
		S <- survfit(x, conf.int=conf.level)
		tstrata <- grep("strata\\(", terms)
		if (length(tstrata) > 0) {
			levels <- levels(with(XX, eval(parse(text=terms[tstrata]))))
			if (missing(lty)) lty <- 1:length(levels)
			plot(S, col=col, lty=lty, main="Survival by Strata at Average Predictors", xlab="time", ylab="survival", ...)
			legend("bottomleft", legend=levels, col=col, lty=lty, title=terms[tstrata], bty="n")
		}
		else plot(S, main="Survival at Average Predictors", xlab="time", ylab="survival", ...)
	}
	invisible(summary(S))
}

PlotCoxph <- function(){
	setWidth <- function(x){
		if (!is.factor(x)) 5
		else (2 + max(nchar(as.character(x))))
	}
	.activeModel <- ActiveModel()
	col.names <- all.vars(formula(get(.activeModel))[-1])
	terms <- attr(terms(get(.activeModel)), "term.labels")
	strata <- grep("strata\\(", terms)
	if (length(strata) > 0){
		strata <- gsub("strata\\(|\\)", "", terms[strata])
		strata <- strsplit(strata, "[\\ \\,]+")[[1]]
	}
	col.names <- setdiff(col.names, strata)
	X <- na.omit(expand.model.frame(get(.activeModel), col.names)[,col.names]) 
	widths <- sapply(X, setWidth)
	env <- environment()
	initializeDialog(title=gettextRcmdr("Plot Cox-Model Survival Functions"))
	confidenceFrame <- tkframe(top)
	radioButtons(confidenceFrame, name="confint",
		buttons=c("default", "true", "false"), initialValue="",
		labels=gettextRcmdr(c("Default behavior", "Yes", "No")), 
		values=c("", ", conf.int=TRUE", ", conf.int=FALSE"),
		title=gettextRcmdr("Plot Confidence Intervals"))
	confidenceLevel <- tclVar(".95")
	confidenceFieldFrame <- tkframe(confidenceFrame)
	confidenceField <- ttkentry(confidenceFieldFrame, width="6", textvariable=confidenceLevel)
	marginalFrame <- tkframe(top)
	marginalCheckbox <- tkcheckbutton(marginalFrame)
	marginalValue <- tclVar("0")
	tkconfigure(marginalCheckbox, variable=marginalValue)	
	radioButtons(top, name="type",
		buttons=c("standard", "factors", "enter"), initialValue="standard",
		labels=gettextRcmdr(c("Plot at predictor means", 
				"Plot by factor levels at covariate means", "Plot at specified values of predictors")),
		title=gettextRcmdr("Type of Plot"))
	outerTableFrame <- tkframe(top)
	assign(".tableFrame", tkframe(outerTableFrame), envir=env)
	setUpTable <- function(...){
		tkdestroy(get(".tableFrame", envir=env))
		assign(".tableFrame", tkframe(outerTableFrame), envir=env)
		nrows <- as.numeric(tclvalue(rowsValue))
		make.col.names <- "labelRcmdr(.tableFrame, text='')"
		for (j in 1:ncols) {
			make.col.names <- paste(make.col.names, ", ", 
				"labelRcmdr(.tableFrame, text='", col.names[j], "')", sep="")
		}
		eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
		for (i in 1:nrows){   
			varname <- paste(".tab.", i, ".1", sep="") 
			assign(varname, tclVar("") , envir=env)
			make.row <- paste("labelRcmdr(.tableFrame, text=", i, ")")
			make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width=", 
				widths[1], ", textvariable=", 
				varname, ")", sep="")
			for (j in 2:ncols){
				varname <- paste(".tab.", i, ".", j, sep="")
				assign(varname, tclVar(""), envir=env)
				make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width=", 
					widths[j], ",textvariable=", 
					varname, ")", sep="")
			}
			eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
		}
		tkgrid(get(".tableFrame", envir=env), sticky="w")
	}
	ncols <- length(col.names)
	rowsFrame <- tkframe(top)
	rowsValue <- tclVar("1")
	rowsSlider <- tkscale(rowsFrame, from=1, to=10, showvalue=FALSE, variable=rowsValue,
		resolution=1, orient="horizontal", command=setUpTable)
	rowsShow <- labelRcmdr(rowsFrame, textvariable=rowsValue, width=2, justify="right")
	onOK <- function(){
		type <- as.character(tclvalue(typeVariable))
		confint <- as.character(tclvalue(confintVariable))
		lev <- as.numeric(tclvalue(confidenceLevel))
		if ((is.na(lev)) || (lev < 0) || (lev > 1)) {
			errorCondition(recall=PlotCoxph, message=gettextRcmdr("Confidence level must be a number between 0 and 1."))
			return()
		}
		lev.survfit <- if (confint == "") "" else paste(", conf.int=", lev, sep="")
		lev <- if (confint == "") "" else paste(", conf.level=", lev, sep="")
		closeDialog()
		if (type == "standard"){
			command <- paste("plot(", .activeModel, confint, lev, ")", sep="")
			doItAndPrint(command)
		}
		else if (type == "factors"){
			command <- paste("plot(", .activeModel, ", byfactors=TRUE", confint, lev, ")", sep="")
			doItAndPrint(command)
		}
		else {
			nrows <- as.numeric(tclvalue(rowsValue))
			cell <- 0
			values <- rep("", nrows*ncols)
			for (i in 1:nrows){
				for (j in 1:ncols){
					cell <- cell+1
					varname <- paste(".tab.", i, ".", j, sep="")
					values[cell] <- eval(parse(text=paste("tclvalue(", varname,")", sep="")))
				}
			}
			values <- trim.blanks(values)
			values <- values[values != ""]
			if (length(values) != nrows*ncols){
				Message(message=sprintf(gettextRcmdr(
							"Number of valid entries in prediction data (%d)\nnot equal to number of rows (%d) * number of columns (%d)."), 
						length(values), nrows, ncols), type="error")
				PlotCoxph()
				return()
			}
			values <- matrix(values, nrows, ncols, byrow=TRUE)
			colnames(values) <- col.names
			command <- character(0)
			for (i in 1:ncols){
				vals <- values[,i]
				if (inherits(X[[i]], "factor")){
					vals <- paste(paste('"', vals, '"', sep=""), collapse=",")
					command <- c(command, paste(col.names[i], "=factor(c(", vals, "), levels=c(", 
							paste(paste('"', levels(X[[i]]), '"', sep=""), collapse=","), "))",
							sep=""))
				}
				else command <- c(command, paste(col.names[i], "=c(", paste(vals, collapse=","), ")", sep=""))
			}
			command <- paste(".newdata <- data.frame(", paste(command, collapse=","), ")", sep="")
			doItAndPrint(command)
			command <- paste("plot(", .activeModel, ", newdata=.newdata", confint, lev, ")", sep="")
			doItAndPrint(command)
			remove(.newdata, envir=.GlobalEnv)
			logger("remove(.newdata)")
		}
		marginal <- as.character(tclvalue(marginalValue))
		if (marginal == 1){
			doItAndPrint("par(new=TRUE)")
			lhs <- as.character(formula(get(.activeModel)))[[2]]
			if (length(strata) == 0){
				command <- paste("plot(survfit(", lhs, " ~ 1, data=", ActiveDataSet(), lev.survfit,
					")", confint, ", lwd=2, lty=1, col=1, axes=FALSE)", sep="")
				doItAndPrint(command)
				command <- 'legend("topright", legend="Marginal survival", lty=1, lwd=2, col=1, bty="n")'
				doItAndPrint(command)
			}
			else{
				mod <- get(.activeModel)
				XX <- na.omit(expand.model.frame(mod, strata)[,strata, drop=FALSE])
				levels <- levels(with(XX, eval(parse(text=strata))))
				command <- paste("plot(survfit(", lhs, " ~ strata(", paste(strata, collapse=","), 
					"), data=", ActiveDataSet(), lev.survfit, ")", confint, ", lwd=2, lty=1:", length(levels), 
					", col=1:", length(levels), ", axes=FALSE)", sep="")
				doItAndPrint(command)
				command <- paste('legend("topright", legend=c(', 
					paste(paste('"', levels, '"', sep=""), collapse=","),
					"), lty=1:", length(levels), ', lwd=2, col=1:', length(levels), 
					', bty="n", title="Marginal Survival")', sep="")
				doItAndPrint(command)
			} 
		}
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="plot.coxph")
	tkgrid(labelRcmdr(confidenceFieldFrame, text=""))
	tkgrid(labelRcmdr(confidenceFieldFrame, text=gettextRcmdr("Level of confidence: ")), confidenceField, sticky="nw")
	tkgrid(confintFrame, confidenceFieldFrame, sticky="nw")
	tkgrid(confidenceFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(labelRcmdr(marginalFrame, text=gettextRcmdr("Plot marginal survival "), fg="blue"), 
		marginalCheckbox, sticky="w")
	tkgrid(marginalFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(typeFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(labelRcmdr(top, text=gettextRcmdr("Specify Values of Predictors:"), fg="blue"), sticky="w")
	tkgrid(labelRcmdr(rowsFrame, text=gettextRcmdr("Number of rows:")), rowsSlider, rowsShow, sticky="w")
	tkgrid(rowsFrame, sticky="w")
	tkgrid(outerTableFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=8, columns=1)       
} 

TermPlots <- function(){
	.activeModel <- activeModel()
	term.names <- attr(terms(get(.activeModel)), "term.labels")
	strata <- grep("strata\\(", term.names)
	if (length(strata > 0)) term.names <- term.names[-strata]
	nterms <- length(term.names)
	doItAndPrint(paste('.mfrow <- par(mfrow=mfrow(', nterms, '))', sep=""))
	doItAndPrint(paste("termplot(", .activeModel, ", ask=FALSE)", sep=""))
	doItAndPrint("par(.mfrow)")
}
