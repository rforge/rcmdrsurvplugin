# last modified 6 December 2008 by J. Fox

survQuantiles.survfit <-
function(object, quantiles=c(.25, .5, .75), ...){
	quants <- function(surv, t){
		invS <- splinefun(surv, t, method="monoH.FC")
		q <- invS(quantiles)
		q[quantiles < min(surv) | quantiles > max(surv)] <- NA
		names(q) <- as.character(round(quantiles, 3))
		q
	}
	summary <- summary(object)
	strata <- summary$strata
	if (is.null(strata)) return(quants(summary$surv, summary$time))
	levels <- levels(strata)
	table <- matrix(0, length(levels), length(quantiles))
	rownames(table) <- levels
	colnames(table) <- as.character(round(quantiles, 3))
	for (s in levels){
		select <- strata == s
		table[s, ] <- quants(summary$surv[select], summary$time[select])
	}
	table
}

