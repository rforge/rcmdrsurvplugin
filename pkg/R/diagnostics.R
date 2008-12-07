# last modified 7 December 2008 by J. Fox

CoxZPH <- function(){
	command <- paste(".CoxZPH <- cox.zph(", ActiveModel(), ")", sep="")
	doItAndPrint(command)
	doItAndPrint(".CoxZPH")
	nvar <- ncol(.CoxZPH$y)
	doItAndPrint(paste(".mfrow <- par(mfrow = mfrow(", nvar, "))", sep=""))
	for (i in 1:nvar){
		doItAndPrint(paste("plot(.CoxZPH[", i, "])", sep=""))
	}
	doItAndPrint("par(mfrow=.mfrow)")
	logger("remove(.CoxZPH, .mfrow)")
	remove(.CoxZPH, .mfrow, envir=.GlobalEnv)
}

CoxDfbetas <- function(){ # works for survreg models as well
	command <- paste(".dfbetas <- residuals(", ActiveModel(), ', type="dfbetas")', sep="")
	doItAndPrint(command)
	command <- if (class(eval(parse(text=ActiveModel()))) == "coxph")
			paste("colnames(.dfbetas) <- names(coef(", ActiveModel(), "))", sep="")
		else paste("colnames(.dfbetas) <- rownames(summary(", ActiveModel(), ")$table)", sep="")
	doItAndPrint(command)
	ncol <- ncol(.dfbetas)
	doItAndPrint(paste(".mfrow <- par(mfrow = mfrow(", ncol, "))", sep=""))
	for (col in colnames(.dfbetas)){
		doItAndPrint(paste('plot(.dfbetas[,"', col, '"])', sep=""))
		doItAndPrint("abline(h=0, lty=2)")
	}
	doItAndPrint("par(mfrow=.mfrow)")
	logger("remove(.dfbetas, .mfrow)")
	remove(.dfbetas, .mfrow, envir=.GlobalEnv)
}

CoxDfbeta <- function(){ # works for survreg models as well
	command <- paste(".dfbeta <- residuals(", ActiveModel(), ', type="dfbeta")', sep="")
	doItAndPrint(command)
	if (class(eval(parse(text=ActiveModel()))) == "coxph"){
		command <- paste("colnames(.dfbeta) <- names(coef(", ActiveModel(), "))", sep="")
		doItAndPrint(command)
	}
	ncol <- ncol(.dfbeta)
	doItAndPrint(paste(".mfrow <- par(mfrow = mfrow(", ncol, "))", sep=""))
	for (col in colnames(.dfbeta)){
		doItAndPrint(paste('plot(.dfbeta[,"', col, '"])', sep=""))
		doItAndPrint("abline(h=0, lty=2)")
	}
	doItAndPrint("par(mfrow=.mfrow)")
	logger("remove(.dfbeta, .mfrow)")
	remove(.dfbeta, .mfrow, envir=.GlobalEnv)
}

MartingalePlots <- function(){
	command <- paste(".residuals <- residuals(", ActiveModel(), ', type="martingale")', sep="")
	doItAndPrint(command)
	coefs <- names(coef(eval(parse(text=ActiveModel()))))
	ncoef <- length(coefs)
	doItAndPrint(paste(".mfrow <- par(mfrow = mfrow(", ncoef, "))", sep=""))
	activeDataSet <- ActiveDataSet()
	for (coef in coefs){
		x <- paste(activeDataSet, "$", coef, sep="")
		command <- if (length(unique(eval(parse(text=x)))) < 10)
				paste("plot(", x, ', .residuals, xlab="', coef,
					'", ylab="Martingale residuals")', sep="")
			else paste("scatter.smooth(", x, ', .residuals, xlab="', coef,
					'", ylab="Martingale residuals", family="gaussian")', sep="")
		doItAndPrint(command)
		doItAndPrint("abline(h=0, lty=2)")
	}
	doItAndPrint("par(mfrow=.mfrow)")
	logger("remove(.residuals, .mfrow)")
	remove(.residuals, .mfrow, envir=.GlobalEnv)	
}

PartialResPlots <- function(){
	command <- paste(".residuals <- residuals(", ActiveModel(), ', type="partial")', sep="")
	doItAndPrint(command)
	coefs <- names(coef(eval(parse(text=ActiveModel()))))
	ncoef <- length(coefs)
	doItAndPrint(paste(".mfrow <- par(mfrow = mfrow(", ncoef, "))", sep=""))
	activeDataSet <- ActiveDataSet()
	for (coef in coefs){
		x <- paste(activeDataSet, "$", coef, sep="")
		command <- if (length(unique(eval(parse(text=x)))) < 10)
				paste("plot(", x, ', .residuals[,"', coef, '"], xlab="', coef,
					'", ylab="partial residuals")', sep="")
			else paste("scatter.smooth(", x, ', .residuals[,"', coef, '"], xlab="',
					coef, '", ylab="partial residuals", family="gaussian")', sep="")
		doItAndPrint(command)
		command <- paste("abline(lm(", '.residuals[,"', coef, '"] ~ ', x, "))", sep="")
		doItAndPrint(command)
	}
	doItAndPrint("par(mfrow=.mfrow)")
	logger("remove(.residuals, .mfrow)")
	remove(.residuals, .mfrow, envir=.GlobalEnv)	
}
