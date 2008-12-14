# last modified 14 December 2009 by J. Fox

plot.coxph <- function(x, newdata, typical=mean, byfactors=FALSE, col=palette(), lty, ...){
	vars <- all.vars(formula(x)[-1])
	X <- na.omit(expand.model.frame(x, vars)[,vars]) 
	if (byfactors){
		if (missing(newdata)){
			newdata <- list()
			names <- names(X)
			for (i in 1:ncol(X)){
				newdata[[names[i]]] <- if (inherits(X[[i]], "factor")) levels(X[[i]])
					else typical(X[[i]])
			}
			newdata <- expand.grid(newdata)
		}
		S <- survfit(x, newdata=newdata)
		newdata <-  as.data.frame(lapply(newdata, function(x) if(is.factor(x)) x else signif(x, 4)))
		if (missing(lty)) lty <- 1:nrow(newdata)
		col1 <- col
		lty1 <- lty
		if (length(grep("strata\\(", as.character(formula(x))[[3]])) > 0){
			col <- rep(col, each=2)
			lty <- rep(lty, each=2)
		}
		plot(S, col=col, lty=lty, 
			main=paste("Survival by", paste(names(newdata), collapse=", ")), 
			xlab="time", ylab="survival", ...)
		legend("bottomleft", legend=apply(newdata, 1, function(x)  paste(x, collapse=", ")),
			col=col1, lty=lty1, title=paste("          ", paste(names(newdata), collapse=", ")), bty="n")
	}
	else {
		terms <- attr(terms(x), "term.labels")
		S <- survfit(x)
		strata <- grep("strata\\(", terms)
		if (length(strata) > 0) {
			levels <- levels(with(X, eval(parse(text=terms[strata]))))
			if (missing(lty)) lty <- 1:length(levels)
			plot(S, col=col, lty=lty, main="Survival by Strata at Average Covariates", xlab="time", ylab="survival", ...)
			legend("bottomleft", legend=levels, col=col, lty=lty, title=terms[strata], bty="n")
		}
		else plot(S, main="Survival at Average Covariates", xlab="time", ylab="survival", ...)
	}
	invisible(summary(S))
}

PlotCoxph <- function(){
	doItAndPrint(paste("plot(", ActiveModel(), ")", sep=""))
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
