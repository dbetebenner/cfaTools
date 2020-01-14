`.onLoad` <-
function(libname, pkgname) {
}

`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('cfaTools',paste(paste0(unlist(strsplit(as.character(packageVersion("cfaTools")), "[.]")), c(".", "-", ".", "")), collapse=""),' (1-14-2020). For help: >help("cfaTools") or visit https://centerforassessment.github.io/cfaTools'))
	}
}
