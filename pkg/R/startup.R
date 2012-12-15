# last modified 2012-12-14

# .onAttach <- function(libname, pkgname){
# 	if (!interactive()) return()
# 	Rcmdr <- options()$Rcmdr
# 	plugins <- Rcmdr$plugins
# 	if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
# 		Rcmdr$plugins <- c(plugins, pkgname)
# 		options(Rcmdr=Rcmdr)
# 		closeCommander(ask=FALSE, ask.save=TRUE)
# 		Commander()
# 	}
# }

.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if (!pkgname %in% plugins) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        if("package:Rcmdr" %in% search()) {
            if(!getRcmdr("autoRestart")) {
                Rcmdr$ask.on.exit <- FALSE
                options(Rcmdr=Rcmdr)
                closeCommander(ask=FALSE, ask.save=TRUE)
                Commander()
            }
        }
        else {
            Commander()
        }
    }
}
