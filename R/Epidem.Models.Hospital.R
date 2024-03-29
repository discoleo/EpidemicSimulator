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


#######################
#######################

#######################
### Hospitalization ###
#######################

### SIR + Hospital + Young/Old Age
# - different mortalities for Hospital vs Community;

### Sensitivity Analysis
getSensitivity_Hosp = function() {
  c("Basic Model" = "SIR", "Infection rate" = "infect",
    "Hospitalization rate (Old)" = "hosp.o", "Hospitalization rate (Young)" = "hosp.y",
    "Death rate (Hospital)" = "death.h", 
    "Death rate (Community Old)" = "death.o", "Death rate (Community Young)" = "death.y"
  );
}

### Hospitalisation
getDisplayTypes = function() {
  c("All", "Compact", "Young", "Old");
}

### Differential ecuations for Hospitalisation model
sirHosp <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    ITot = Iy + Io; # all infected (young & old) can infect!
    # Susceptible
    dSy = -infect * Sy * ITot - infect * Sy * H; # both I & H infect!
    dSo = -infect * So * ITot - infect * So * H;
    # Total
    dT  =  dSy + dSo; # not really needed;
    # Infected
    dIy = -dSy - recov.c * Iy - death.y * Iy - hosp.y * Iy;
    dIo = -dSo - recov.c * Io - death.o * Io - hosp.o * Io;
    # Hospitalized
    dHcum = hosp.y * Iy + hosp.o * Io; # used to extract daily rates;
    dHy =  hosp.y * Iy - recov.h * Hy - death.h * Hy;
    dHo =  hosp.o * Io - recov.h * Ho - death.h * Ho;
    dH  =  dHcum - recov.h * H - death.h * H; # Hy + Ho
    # Death 
    dDc =  death.y * Iy + death.o * Io; # check if death.old useful?
    dDh =  death.h * H;
    # Recovered
    dR  =  recov.c * Iy + recov.c * Io + recov.h * H;
    return(list(c(dT, dSy, dSo, dIy, dIo, dR, dHcum, dH, dHy, dHo, dDc, dDh)));
  })
}

initSIR_Hosp = function(opt, end.time, options) {
  times = seq(0, end.time, by = 1) # flt = filter
  # - S = susceptible young people;
  # - O = susceptible old people;
  # - Hcum = cumulative hospitalization;
  # - Dc = Deceased (community); Dh = Deceased hospital;
  parameters = c(infect = opt$infect,
                 recov.c = opt$recov.c, 
                 recov.h = opt$recov.h,
                 # Death rate: scaled by opt$recov: remove scaling?
                 death.y = opt$recov.c * opt$death.y, 
                 death.o = opt$recov.c * opt$death.o,
                 death.h = opt$recov.h * opt$death.h,
                 hosp.y = opt$hosp.y, 
                 hosp.o = opt$hosp.o)
  I0 = 1E-6; 
  init = c(T = 1 - I0, Sy = (1 - I0) * (1 - options$p.old), So = (1 - I0) * options$p.old,
           Iy = I0, Io = 0.0, R = 0.0,
           Hcum = 0.0, H = 0.0, Hy = 0.0, Ho = 0.0, Dc = 0.0, Dh = 0.0); # init = state
  
  ### Solve using ode
  out = solve.sir(sirHosp, init, parameters, times)
  attr(out, "Model") = "Hospitalisation";
  return(out);
}

### Function for plotting 
plotSIR_Hosp = function (out, options, flt="Old", add = FALSE, plot.legend = TRUE, ...)
{ 
  # legend labels
  lbl = c("Total", "Young", "Old", "Infected: Young (in community)", "Infected: Old (in community)",
          "Recovered", "Hosp (cumulative)", "Hosp: All", "Hosp: Young", "Hosp: Old",
          "Death: Community", "Death: Hospital");
  # controller for filtering
  type = match(flt, getDisplayTypes()); # verify if the first parameter is in the list generated by getDisplay
  # legend position
  leg.xy = c(0.7 * max(out$time), 0.8);
  # number of colums for legend
  ncol = 2
  
  # filter results
  # type1 = All; type2 = Compact
  # type3 = Young; type4 = Old
  if(type > 1) {
    out$D = out$Dc + out$Dh;
    out$HospRate = c(out$Hcum[1], diff(out$Hcum)) * options$hosp.rate.scale; # modify hospitalisation rate 
    lbl = c(lbl, "Death: All", paste0("Hosp (rate)[scale = x", options$hosp.rate.scale, "]"));
    ncol = 1
    if(type == 2) {
      r = filter.out(out, c("T", "Hy", "Ho", "Dc", "Dh"), lbl);
      leg.xy = c(0, 0.79);
    } else if(type == 3) {
      r = filter.out(out, c("T", "So", "Io", "Ho", "Dc", "Dh", "R"), lbl); 
      leg.xy = c(0.0, max(1-options$p.old, out$Io) * 0.8);
      col = 2
    } else if(type == 4) {
      r = filter.out(out, c("T", "Sy", "Iy", "Hy", "Dc", "Dh", "R"), lbl);
      leg.xy = c(0.0, max(options$p.old, max(out$Io)) * 0.8) ;
    }
    out = r$out; lbl = r$lbl;
  }
  # plotting the grafic
  plot.sir(out, legend.lbl = lbl, legend.xy = leg.xy, add = add, ncol = ncol,
           plot.legend = plot.legend,  title = "SIR Hospitalisation Model", ...);
}

### Sensitivity Analysis

Sensitivity_Hosp = function(paramName, parameters, end.time, min=0, max=1, options, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    parameters[[paramName]] = p;
    print(parameters)
    
    out = initSIR_Hosp(parameters, end.time, options); 
    
    plotSIR_Hosp(out, options, flt = flt, add = if(p == min) FALSE else TRUE,
                 plot.legend = FALSE, lty = options$sensitivity.lty);
  }
  
  parameters[[paramName]] = min;
  
  out = initSIR_Hosp(parameters, end.time, options);
  
  plotSIR_Hosp(out, options, flt = flt,
               add = TRUE, plot.legend = TRUE,
               lty = 1);
}



################################
### Extended Hospitalisation ###
################################

### Options for sensitivity analysis controller
getSensitivity_EH = function() {
  c("Basic Model" = "SEIR", "Infection rate" = "infect",
    "Exposed rate (Young)" = "exposed.y", "Exposed rate (Old)" = "exposed.o",
    "Hospitalization rate (Old)" = "hosp.o", "Hospitalization rate (Young)" = "hosp.y",
    "Death rate (Hospital)" = "death.h", 
    "Death rate (Community Old)" = "death.o", "Death rate (Community Young)" = "death.y"
  );
}

### Options for filter controller
getDisplayTypesEH = function() {
  c("All", "Compact", "Young", "Old");
}

### Differential ecuations for Exposed model
sirEH <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    ITot = Iy + Io; 
    # Susceptible
    dSy = -infect * Sy * ITot - infect * Sy * H; 
    dSo = -infect * So * ITot - infect * So * H;
    # Total
    dT  =  dSy + dSo; 
    # Exposed
    dEy = - dSy - Ey * exposed.y;
    dEo = - dSo - Eo * exposed.o;
    # Infected
    dIy = Ey * exposed.y - recov.c * Iy - death.y * Iy - hosp.y * Iy;
    dIo = Eo * exposed.o - recov.c * Io - death.o * Io - hosp.o * Io;
    # Hospitalized
    dHcum = hosp.y * Iy + hosp.o * Io; 
    dHy =  hosp.y * Iy - recov.h * Hy - death.h * Hy;
    dHo =  hosp.o * Io - recov.h * Ho - death.h * Ho;
    dH  =  dHcum - recov.h * H - death.h * H; 
    # Death
    dDc =  death.y * Iy + death.o * Io; 
    dDh =  death.h * H;
    # Recovered
    dR  =  recov.c * Iy + recov.c * Io + recov.h * H;
    return(list(c(dT, dSy, dSo, dEy, dEo, dIy, dIo, dR, dHcum, dH, dHy, dHo, dDc, dDh)));
  })
}

### Function for initializing the model
initSIR_EH = function(opt, end.time, options) {
  times = seq(0, end.time, by = 1) 
  
  parameters = c(infect = opt$infect,
                 exposed.y = opt$exposed.y,
                 exposed.o = opt$exposed.o,
                 recov.c = opt$recov.c, 
                 recov.h = opt$recov.h,
                 death.y = opt$recov.c * opt$death.y, 
                 death.o = opt$recov.c * opt$death.o,
                 death.h = opt$recov.h * opt$death.h,
                 hosp.y = opt$hosp.y, 
                 hosp.o = opt$hosp.o)
  I0 = 1E-6; 
  init = c(T = 1 - I0, Sy = (1 - I0) * (1 - options$p.old), So = (1 - I0) * options$p.old,
           Ey = 0.0, Eo = 0.0,
           Iy = I0, Io = 0.0, R = 0.0,
           Hcum = 0.0, H = 0.0, Hy = 0.0, Ho = 0.0, Dc = 0.0, Dh = 0.0);
  
  ### Solve using ode
  out = solve.sir(sirEH, init, parameters, times)
  attr(out, "Model") = "Extended Hospitalisation";
  return(out);
}

### Function for plotting
plotSIR_EH = function (out, options, flt="Old", add = FALSE, plot.legend = TRUE, ...)
{ 
  # legend labels
  lbl = c("Total", "Young", "Old", "Exposed (Young)", "Exposed (Old)",
          "Infected: Young (in community)", "Infected: Old (in community)",
          "Recovered", 
          "Hosp (cumulative)", "Hosp (All)", "Hosp (Young)", "Hosp (Old)",
          "Death (Community)", "Death (Hospital)");
  # legend position
  leg.xy = c(0, 0.77);
  # number of colums for legend
  ncol = 2
  
  # controller for filtering
  type = match(flt, getDisplayTypesEH())
  
  # filter results
  # type1 = All; type2 = Compact
  # type3 = Young; type4 = Old
  if(type > 1) {
    out$D = out$Dc + out$Dh;
    out$HospRate = c(out$Hcum[1], diff(out$Hcum)) * options$hosp.rate.scale; 
    ncol = 1
    lbl = c(lbl, "Death: All", paste0("Hosp (rate)[scale = x", options$hosp.rate.scale, "]"));
    if(type == 2) {
      r = filter.out(out, c("T", "Hy", "Ho", "Dc", "Dh"), lbl);
      ncol = 2
      leg.xy = c(0, 0.75);
    } else if(type == 3) {
      r = filter.out(out, c("T", "So", "Io", "Eo", "Ho", "Dc", "Dh", "R"), lbl); 
      leg.xy = c(0, max(1-options$p.old, out$Io, max(out$Hcum)) * 0.8);
    } else if(type == 4) {
      r = filter.out(out, c("T", "Sy", "Iy", "Ey", "Hy", "Dc", "Dh", "R"), lbl);
      leg.xy = c(0, max(options$p.old, out$Io) * 0.9);
    } else r = filter.out(out, c("Hy", "Ho"), lbl=lbl);
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, legend.xy = leg.xy, add = add, ncol = ncol,
           plot.legend = plot.legend, title = "SIR Exposed Hospitalization Model", ...);
}


### Sensitivity Analysis
# paramName = controller name, 
# parameters = list of parameters
# options = global options
Sensitivity_EH = function(paramName, parameters, end.time, min=0, max=1, options, flt = "Old") {
  by = (max - min)/20;
  # for this interval, run the simulation and plot the result
  for(p in seq(min, max, by = by)) {
    parameters[[paramName]] = p;
    print(parameters)
    
    out = initSIR_EH(parameters, end.time, options); 
    
    plotSIR_EH(out, options, flt = flt, add = if(p == min) FALSE else TRUE,
               plot.legend = FALSE, lty = options$sensitivity.lty);
  }
  
  parameters[[paramName]] = min;
  
  out = initSIR_EH(parameters, end.time, options);
  
  plotSIR_EH(out, options, flt = flt,
             add = TRUE, plot.legend = TRUE,
             lty = 1);
}