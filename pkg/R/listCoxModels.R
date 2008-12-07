# last modified 7 December 2008 by J. Fox 

listCoxModels <-
function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
						function(.x) "coxph" == (class(get(.x, envir=envir))[1]))]
}

coxphP <- function() activeModelP() && class(get(ActiveModel()))[1] == 'coxph'
