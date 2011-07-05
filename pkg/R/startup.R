# last modified 2011-07-05

#.First.lib <- function(libname, pkgname){
#    if (!interactive()) return()
#    Rcmdr <- options()$Rcmdr
#    plugins <- Rcmdr$plugins
#    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
#        Rcmdr$plugins <- c(plugins, pkgname)
#        options(Rcmdr=Rcmdr)
#        closeCommander(ask=FALSE, ask.save=TRUE)
#        Commander()
#        }
#    }

.onAttach <- function(libname, pkgname){
	if (!interactive()) return()
	Rcmdr <- options()$Rcmdr
	plugins <- Rcmdr$plugins
	if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
		Rcmdr$plugins <- c(plugins, pkgname)
		options(Rcmdr=Rcmdr)
		closeCommander(ask=FALSE, ask.save=TRUE)
		Commander()
	}
}
