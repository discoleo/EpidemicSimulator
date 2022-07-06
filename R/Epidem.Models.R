######################
###
### Epidemic Simulator - Bachelor Thesis
### Student: Anamaria Bica
### West University, Timisoara
### Year 3, Faculty of Computer Science
###
### Coordinator: Daniela Zaharie
### Supervisor: Leonard Mada
### Syonic SRL

### based on:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL

### Load Libraries
# library(deSolve)

# load models
# source("Epidem.Model.Hospital.R");
# source("Epidem.Model.Vaccine.R");
# source("Epidem.Model.Viruses.R");
# source("Epidem.Model.AgeGroups.R");

#####################
#####################

### Plot color
# Xy = Xa (same color)
# T = green, S = green, I = yellow, H = blue, D = gray, R = green/gray, E = ?
# HospRate = red
colors = list(T  = "#32dd97", 
              S  = "#00ff00", Sc = "#64f864", Sy = "#32f832", Sa = "#32f832", So = "#32a832",
              Ey = "#dd96FF", Eo = "#cdA6F0",
              I  = "#ffff22", Ic = "#ffffb2", Iy = "#fed932", Ia = "#fed932", Io = "#feb24c", 
              IV1 = "#ffff22", IV2 = "#feb24c",
              Hcum = "#9d9dF0",
              H  = "#ff5696", Hc = "#ff64a0", Hy = "#fe84d0", Ha = "#fe84d0", Ho = "#fe74c0", 
              HV1 = "#fe84d0", HV2 = "#fe74c0",
              D  = "#696969", Dc = "#696969", Dh = "#a96969", 
              Dy = "#969696", Da = "#969696", Do = "#525252", 
              DV1 = "#969696", DV2 = "#525252", 
              R  = "#41ae76", Rc = "#66c2a4", Ra = "#41ae76", Ro = "#238b45",
              RV1 = "#41ae76", RV2 = "#238b45",
              Vy = "#64ff84", Vo = "#84ff64",
              HospRate = "#ff0000", DeathRate = "#F9A9A9")


### Basic Functions

### Solve SIR
solve.sir = function(sir.f, init, parameters, times) {
  ## Solve using ode (General Solver for Ordinary Differential Equations)
  out = deSolve::ode(y = init, times = times, func = sir.f, parms = parameters)
  ## change to data frame
  out = as.data.frame(out)
  return(out)
}


### Plot SIR
basic.lbl = c("Susceptible", "Infected", "Recovered");

legend.xyf = function(times, x=c(0,0)) {
  c(max(times)*2/3, 0.7) + x;
}

plot.sir = function(y, times = NULL, legend.lbl = basic.lbl, legend.xy, leg.off = c(0,0),
                    ylab = "Susceptible and Recovered", title = "SIR Model",
                    lty = 1, lwd = 2, col = NULL, ncol = 1,
                    add = FALSE, plot.legend=TRUE, ...) {
  if(is.null(times)) {
    times = y$time;
    if(is.null(times)) stop("The times argument is missing!");
  }
  y$time = NULL;
  
  if(is.null(col)){
    col = unlist(colors[names(y)])
  }
  
  if(missing(legend.xy)) legend.xy = legend.xyf(times, leg.off)
  if(add) {
    matplot(x = times, y = y, type = "l", lwd = lwd, lty = lty, bty = "l", col = col, add=TRUE);
  } else {
    matplot(x = times, y = y, type = "l",
            xlab = "Time", ylab = ylab, main = title,
            lwd = lwd, lty = lty, bty = "l", col = col, ...)
  }
  
  ### Add legend
  if(plot.legend) {
    legend(legend.xy[1], legend.xy[2], legend.lbl,
           pch = 19, col = col, bty = "n", ncol = ncol)
  }
}

filter.out = function(x, flt, lbl) {
  # look for filtered names, which will be excluded from the legend
  id = match(flt, names(x)); 
  id = id[ ! is.na(id)];
  if(length(id) > 0) {
    x = x[, - id];
    idTime = match("time", names(x)); # verify if time is a data-column;
    if(! is.na(idTime)) id = id - 1; # correct for x$time
    lbl = lbl[-id];
  }
  return(list(out = x, lbl = lbl));
}

#####################
#####################

### Basic SIR
basic_sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = - infect * S * I;
    dI = - dS - recov * I;
    dR =  recov * I;
    
    return(list(c(dS, dI, dR)))
  })
}

### Function for initializing the model
initSIR_Basic = function(list, end.time){
  times = seq(0, end.time, by=1)
  parameters = c(infect = list[1], recov = list[2])
  print(parameters)
  
  I0 = 1E-6;
  init = c(S = 1 - I0, I = I0, R = 0.0)  ### Solve using ode
  ### Solve using ode
  out = solve.sir(basic_sir, init, parameters, times)
  attr(out, "Model") = "Basic";
  head(out, 10)
  
  leg.xy = leg.xy = c(0, 0.75);
  
  ### Plot
  plot.sir(out, legend.lbl = c("Susceptible", "Infected", "Recovered"), legend.xy = leg.xy)
}
