

### Global Options

#' @export
getOptions = function() {
  opt = list(
    ### Global Options - Models
    delay.vacc = 60,
    # delay.2V = 160,
    p.old = 0.2, # 0.2 = 20% of the population (old people)
    p.children = 0.2, # 0.2 = 20% of the population (children)
    death.rate.scale = 24,
    hosp.rate.scale = 12,
    opt.2V.permutation = 0.1, # 10% of V1 will mutate to V2;

    ### Global options - Analysis
    stat.max.cutoff = 0.8, # 80% of maximum value 
    population.size = 1E+6,

    # Sensitivity Analysis
    sensitivity.lty = 4, # lty = line type
    sensitivity.infect.min = 0.75,
    sensitivity.infect.max = 1.25,
	
	### UI
	# Vaccination scale
	vaccine.rate.scale = 100
  )
}

getOptionsDescription = function() {
	descriptions = c(
		"Delay (in days) before vaccination is started", 
		"Proportion of older people in the total population", 
		"Proportion of children in the total population",
		"Used for scaling the daily mortality rate (to make it visible in the plot)",
		"Used for scaling the daily hospitalisation rate (to make it visible in the plot)",
		"Used in the 2 Viruses model. Proportion of persons infected with virus 1 which will suffer a mutation to virus 2.",
		"Analysis: cutoff at 80% of the maximum value of a compartment in a simulation",
		"Analysis: The total population",
		"Sensitivity Analysis: Line type used to plot the results of the Sensitivity Analysis",
		"Sensitivity analysis: Used to compute the lower limit of the infection rate (as the proportion of the current infection rate).",
		"Sensitivity analysis: Used to compute the upper limit of the infection rate (as the proportion of the current infection rate).",
		"UI: Scale for the Vaccination sliders"
	);
  
  return(descriptions);
}

### Colors for compartments
#' @export
getDiagramColors = function() {
	col = list(
		S = "green", V = "light green", E = "light yellow", I = "yellow", 
		H = "orange", D = "gray", R = "green");
	return(col);
}
