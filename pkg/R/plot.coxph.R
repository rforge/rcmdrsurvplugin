# last modified 13 December 2009 by J. Fox

plot.coxph <- function(x, newdata, typical=mean, col=palette(), lty=1:nrow(newdata), ...){
	X <- model.frame(x)[, -1]
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
	col1 <- col
	lty1 <- lty
	if (length(grep("strata\\(", as.character(formula(x))[[3]])) > 0){
		col <- rep(col, each=2)
		lty <- rep(lty, each=2)
	}
	plot(S, col=col, lty=lty, 
		main=paste("Survival by", paste(names(newdata), collapse=", ")), ...)
	legend("bottomleft", legend=apply(newdata, 1, function(x)  paste(x, collapse=", ")),
		col=col1, lty=lty1, title=paste("          ", paste(names(newdata), collapse=", ")), bty="n")
	invisible(summary(S))
}

PlotCoxph <- function(){
	doItAndPrint(paste("plot(", ActiveModel(), ")", sep=""))
}
