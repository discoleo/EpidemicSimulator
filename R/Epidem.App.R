

### startSimulator
#' @export
startSimulator = function(optUI = NULL, ...) {
	lst = list(...);
	if(length(lst) > 0) {
		warning("Arguments have not been implemented yet!");
	}
	#
	shinyApp(ui=getUI(opt = optUI), server=getServer())
}

