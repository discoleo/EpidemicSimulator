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


### Generic Functions

# Header for Analysis
hrAnalysis = function() {
	HTML("<h2>Analysis</h2>");
}
hrStatistics = function() {
	HTML("<h2>Summary Statistics</h2>
		<p>Total Death, Total Hospitalized:</p>");
}

# Analysis
summaryAnalysis = function(x){
  results = rbind(summarySIR_Infected(x), 
                  summarySIR_Death(x),
                  summarySIR_Hosp(x));
  # Debug
  print(results)
  return(results)
}

### Helper Functions
# - used by function computeSummary();

calculate_parameters <- function(dX, category = NULL, type = "Infected", options = getOptions()) {
  # maximum number of infected persons/day
  maxCutoff = max(dX) * options$stat.max.cutoff;
  # days with number of infections >= cutoff
  isHigher = (dX >= maxCutoff);
  daysHigh = rle(isHigher);
  daysHigh = daysHigh$lengths[daysHigh$values > 0];
  if(length(daysHigh) == 0) daysHigh = 0;
  dXMax = round(max(dX) * options$population.size);
  dXCutoff = round(maxCutoff * options$population.size);
  if(is.null(category)){
    return(data.frame(daysHigh, dXMax, dXCutoff));
  } else {
    return(data.frame(type, category, daysHigh, dXMax, dXCutoff));
  }
}

computeSummary = function(x, y, ctgs, isCummulative = FALSE) {
  if(isCummulative) y = c(0, diff(y));
  Dx_cum = - x + x[1] - y;
  dDx = diff(Dx_cum);
  param_Dx = calculate_parameters(dDx, ctgs, "Infected");
  
  return(param_Dx)
}

computeSummary0 = function(x, ctgs,  type = "Death", isCumulative = TRUE) {
  
  Dx_cum = x - x[1];
  dDx = if(isCumulative) diff(Dx_cum) else Dx_cum;
  param_Dx = calculate_parameters(dDx, ctgs, type);
  
  return(param_Dx)
  
}

### Generic Functions

# Basic Summary Statistics
# - Total Deaths;
# - Total Hospitalized; 
summarySIR = function(x) {
  type = attr(x, "Model");
  last = nrow(x);
  
  if(type == "Vaccination" ) {
    D = x$D[last];
    results = data.frame(
      Comp = c("D", "Hcum"), 
      Val = c(D, x$Hcum[last]) )
  } else if(type == "Vaccination Stratified") {
    Dy = x$Dy[last];
    Do = x$Do[last];
    Dt = Dy + Do
    results = data.frame(
      Comp = c("Dy", "Do", "Dt", "Hcum"), 
      Val = c(Dy, Do, Dt, x$Hcum[last]) )
  } else if(type == "2 Viruses") {
    DV1 = x$DV1[last];
    DV2 = x$DV2[last];
    Dt = DV1 + DV2 
    results = data.frame(
      Comp = c("DV1", "DV2", "Dt", "Hcum"), 
      Val = c(DV1, DV2, Dt, x$Hcum[last]) )
  } else if(type == "AG3") {
    Dc = x$Dc[last];
    Da = x$Da[last];
    Do = x$Do[last];
    Dt = Dc + Da + Do
    results = data.frame(
      Comp = c("Dc", "Da", "Do", "Dt", "Hcum"), 
      Val = c(Dc, Da, Do, Dt, x$Hcum[last]) )
  } else {
    print("Default Model"); # TODO: check valid model
    Dc = x$Dc[last];
    Dh = x$Dh[last];
    Dt = Dc + Dh
    results = data.frame(
      Comp = c("Dc", "Dh", "Dt", "Hcum"), 
      Val = c(Dc, Dh, Dt, x$Hcum[last]) )
  }
  return(results)
}


summarySIR_Infected = function(x)
{
  type = attr(x, "Model");
  
  if(type == "Hospitalisation" ){
    param_Iy = computeSummary(x$Sy, 0, "Young: ")
    param_Io = computeSummary(x$So, 0,  "Old: ")
    param_IAll = computeSummary(x$T, 0,  "Total: ")
    
    results = rbind(param_IAll, param_Iy, param_Io);
  } else if(type == "Extended Hospitalisation"){
    param_Iy = computeSummary(x$Sy, 0, "Young: ")
    param_Io = computeSummary(x$So, 0,  "Old: ")
    param_IAll = computeSummary(x$T, 0,  "Total: ")
    
    results = rbind(param_IAll, param_Iy, param_Io);
  } else if( type == "Vaccination" ){
    param_Iy = computeSummary(x$Sy, x$Vy, "Young: ")
    param_Io = computeSummary(x$So, x$Vo,  "Old: ")
    param_IAll = computeSummary(x$T, x$Vy + x$Vo,  "Total: ")
    
    results = rbind(param_IAll, param_Iy, param_Io);
  } else if(type == "Vaccination Stratified"){
    param_Iy = computeSummary(x$Sy, x$Vy, "Young: ")
    param_Io = computeSummary(x$So, x$Vo,  "Old: ")
    param_IAll = computeSummary(x$T, x$Vy + x$Vo,  "Total: ")
    
    results = rbind(param_IAll, param_Iy, param_Io);
  } else if( type == "2 Viruses" ){
    param_I = computeSummary(x$S, 0,  "Total infected: ")
    
    results = param_I;
  } else if( type == "AG3" ){
    param_Ic = computeSummary(x$Sc, 0, "Young: ")
    param_Ia = computeSummary(x$Sa, 0, "Adults: ")
    param_Io = computeSummary(x$So, 0,  "Old: ")
    param_IAll = computeSummary(x$T, 0,  "Total: ")
    
    results = rbind(param_IAll, param_Ic, param_Ia, param_Io);
  }
  
  names(results) = c("Type", "Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";

  return(results)
}



summarySIR_Death = function(x){
  type = attr(x, "Model");
 
  if(type == "Hospitalisation"){
    param_Dc = computeSummary0(x$Dc,  "Community: ")
    param_Dh = computeSummary0(x$Dh,  "Hospital: ")
    param_DT = computeSummary0(x$Dc + x$Dh , "Total: ");
    
    results = rbind(param_DT, param_Dc, param_Dh);
  } else if(type == "Extended Hospitalisation"){
    param_Dc = computeSummary0(x$Dc,  "Community: ")
    param_Dh = computeSummary0(x$Dh,  "Hospital: ")
    param_DT = computeSummary0(x$Dc + x$Dh , "Total: ");
    
    results = rbind(param_DT, param_Dc, param_Dh);
  } else if(type == "Vaccination Stratified"){
    param_Dy = computeSummary0(x$Dy,  "Young: ")
    param_Do = computeSummary0(x$Do,  "Old: ")
    param_DT = computeSummary0(x$Dy + x$Do , "Total: ");
    
    results = rbind(param_DT, param_Dy, param_Do);
  } else if(type == "2 Viruses"){
    param_DV1 = computeSummary0(x$DV1,  "Virus 1: ")
    param_DV2 = computeSummary0(x$DV2,  "Virus 2: ")
    param_DT = computeSummary0(x$DV1 + x$DV2 , "Total: ");
    
    results = rbind(param_DT, param_DV1, param_DV2);
  } else if(type == "Vaccination"){
    param_D = computeSummary0(x$D,  "Death: ")
    
    results = rbind(param_D);
  } else if(type == "AG3"){
    param_Dc = computeSummary0(x$Dc,  "Children: ")
    param_Da = computeSummary0(x$Da,  "Adults: ")
    param_Do = computeSummary0(x$Do,  "Old: ")
    param_DT = computeSummary0(x$Dc + x$Da + x$Do , "Total: ");
    
    results = rbind(param_DT, param_Dc, param_Da, param_Do);
  }
  
  names(results) = c("Type", "Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";
  
  return(results)
}

summarySIR_Hosp = function(x){
  type = attr(x, "Model");
  typeInHosp = "In Hosp";
  
  if(type == "Hospitalisation" ){
    param_H = computeSummary0(x$H,  "Total: ", type = typeInHosp, isCumulative = FALSE)
    param_Hy = computeSummary0(x$Hy,  "Young: ", type = typeInHosp, isCumulative = FALSE)
    param_Ho = computeSummary0(x$Ho, "Old: ", type = typeInHosp, isCumulative = FALSE);
    param_Hcum = computeSummary0(x$Hcum, "Total: ", type = "Hosp (daily)");
    
    results = rbind(param_H, param_Hy, param_Ho, param_Hcum);
  }
  else if(type == "Extended Hospitalisation"){
    param_H = computeSummary0(x$H,  "Total: ", type = typeInHosp, isCumulative = FALSE)
    param_Hy = computeSummary0(x$Hy,  "Young: ", type = typeInHosp, isCumulative = FALSE)
    param_Ho = computeSummary0(x$Ho, "Old: ", type = typeInHosp, isCumulative = FALSE);
    param_Hcum = computeSummary0(x$Hcum, "Total: ", type = "Hosp (daily)");
    
    results = rbind(param_H, param_Hy, param_Ho, param_Hcum);
  }
  else if(type == "Vaccination Stratified"){
    param_T = computeSummary0(x$Hy + x$Ho,  "Total: ", type = typeInHosp, isCumulative = FALSE)
    param_Hy = computeSummary0(x$Hy,  "Young: ", type = typeInHosp, isCumulative = FALSE)
    param_Ho = computeSummary0(x$Ho, "Old: ", type = typeInHosp, isCumulative = FALSE);
    param_Hcum = computeSummary0(x$Hcum, "Total: ", type = "Hosp (daily)");
    
    results = rbind(param_T, param_Hy, param_Ho, param_Hcum);
  }
  else if(type == "2 Viruses"){
    param_T = computeSummary0(x$HV1  + x$HV2,  "Total: ", type = typeInHosp, isCumulative = FALSE)
    param_HV1 = computeSummary0(x$HV1,  "Virus 1: ", type = typeInHosp, isCumulative = FALSE)
    param_HV2 = computeSummary0(x$HV2,  "Virus 2: ", type = typeInHosp, isCumulative = FALSE)
    param_Hcum = computeSummary0(x$Hcum, "Total: ", type = "Hosp (daily)");
    
    results = rbind(param_T, param_HV1, param_HV2, param_Hcum);
  }
  else if(type == "Vaccination"){
    param_H = computeSummary0(x$H,  "Hosp: ", type = typeInHosp, isCumulative = FALSE)
    param_Hcum = computeSummary0(x$Hcum, "Total: ", type = "Hosp (daily)");
    
    results = rbind(param_H, param_Hcum);
  }
  else if(type == "AG3"){
    param_T = computeSummary0(x$Hc + x$Ha + x$Hc,  "Total: ", type = typeInHosp, isCumulative = FALSE)
    param_Hc = computeSummary0(x$Hc,  "Children: ", type = typeInHosp, isCumulative = FALSE)
    param_Ha = computeSummary0(x$Ha,  "Adults: ", type = typeInHosp, isCumulative = FALSE)
    param_Ho = computeSummary0(x$Ho,  "Old: ", type = typeInHosp, isCumulative = FALSE)
    param_Hcum = computeSummary0(x$Hcum, "Total: ", type = "Hosp (daily)");
    
    results = rbind(param_T, param_Hc, param_Ha, param_Ho, param_Hcum);
  }
  
  names(results) = c("Type", "Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";
  
  return(results)
}


