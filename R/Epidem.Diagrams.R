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

# Parts of the work are based on:
# Team Project 2021
# Students:
#   Dora Calea, Ioana Obreja,
#   Liviu Sopon and Dragos Ursan
#   West University, Timisoara
#
# Supervisor: Leonard Mada
# Syonic SRL


### Colors for compartments
# - moved to file: Epidem.Options.R;


### Helper Functions
plotMatrix = function(m, pos, name, main, col,
		box.size=0.0175, box.cex=1.75, arr.pos=0.75, arr.width=0.25,
		lwd=2, arr.lwd = lwd) {
	diagram::plotmat(A = m, pos=pos, name=name, main=main,
		lwd = lwd, box.col = col,
		box.size = box.size, box.cex = box.cex,
		arr.type = "simple", arr.lwd = arr.lwd, 
		arr.width = arr.width, arr.pos = arr.pos, curve = 0);
}

##################
### Schema SIR ###
##################

diagramBasicSIR = function(lwd = 2, col = getDiagramColors(),
		file = "Model_SIR_Basic.png", save.png = FALSE,
		scaleX = 3/4, scaleY = 3/4) {
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res = 100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 3
  DiffMat = matrix(data = character(0), nrow = nComp, ncol = nComp);
  m = as.data.frame(DiffMat);
  
  # names and colors of boxes
  name <- c('S', 'I', 'R');
  color <- unlist(col[name]);
  
  ### Arrows 
  m[[2,1]] = "";
  m[[3,2]] = "";
  
  ### Plot diagram
  plotMatrix(m, pos = 3, name=name, col = color,
	main = "SIR model",
	box.size = 0.05, box.cex = 3, arr.lwd = lwd);
  
  # Curved arrow
  # - coordinates are hard coded;
  arr.pos = 0.93; delta = 0.015;
  diagram::curvedarrow(from = c(0.45, 0.58), to = c(0.3, 0.5 + delta), lwd=lwd,
              arr.width = 0.4, curve = 0.5, arr.type = "triangle", 
              arr.pos = arr.pos, lcol = col$I, arr.col = col$I)
}



##############################
### Schema Hospitalisation ###
##############################

diagram.H = function(lwd = 2, col = getDiagramColors(),
		file = "Model_SIHRD.png", save.png = FALSE,
		scaleX = 3/4, scaleY = 3/4) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res = 100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 8
  DiffMat = matrix(data = character(0), nrow = nComp, ncol = nComp);
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(
    expression(S[Y]), #1
    expression(S[O]), #2
    expression(I[Y]), #3
    expression(I[O]), #4
    expression(H[Y]), #5
    expression(H[O]), #6
    "D", #7
    "R") #8
  
  color <-  c(col$S, col$S, col$I, col$I, 
              col$H, col$H, col$D, col$R)
  
  # arrows 
  m[[3,1]] = ""
  m[[4,2]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[8,3]] = ""
  m[[6,4]] = ""
  m[[7,4]] = ""
  m[[8,4]] = ""
  m[[7,5]] = ""
  m[[8,5]] = ""
  m[[7,6]] = ""
  m[[8,6]] = ""
  
  ### Positions of boxes
  coord = matrix(nrow = nComp, ncol = 2)
  
  # Sy
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.2 * scaleY
  # So
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 - 0.2 * scaleY
  
  # Iy
  coord[3,1] = 0.5 - 0.2 * scaleX
  coord[3,2] = 0.5 + 0.2 * scaleY
  # Io
  coord[4,1] = 0.5 - 0.2 * scaleX
  coord[4,2] = 0.5 - 0.2 * scaleY
  
  # Hy
  coord[5,1] = 0.5 # + 0.1 * scaleX
  coord[5,2] = 0.5 + 0.2 * scaleY
  # Ho
  coord[6,1] = 0.5 # + 0.1 * scaleX
  coord[6,2] = 0.5 - 0.2 * scaleY
  
  # D
  coord[7,1] = 0.5 + 0.2 * scaleX
  coord[7,2] = 0.5 + 0.4 * scaleY
  
  # R
  coord[8,1] = 0.5 + 0.2 * scaleY
  coord[8,2] = 0.5 - 0.4 * scaleY
  
  ### Plot diagram
  plotMatrix(m, pos = coord, name = name, col = color,
	main = "SIR + Hospitalization Model",
	box.size = 0.0175, box.cex = 1.75, arr.lwd = lwd);
  
  ### Curved Arrows
  # - coordinates are hard coded;
  
  # from Iy to Sy & Io to So
  curvedArrows2(0.5, 0.5, dx = c(-0.215, -0.3), dy = c(0.295, 0.2),
                curve=0.7, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Hy to Sy & Ho to So
  curvedArrows2(0.5, 0.5, dx = c(-0.01, -0.3), dy = c(0.30, 0.2),
                curve=0.7, lcol = col$H,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Iy to So & Io to Sy
  curvedArrows2(0.5, 0.5, dx = c(-0.215, -0.3), dy = c(0.12, -0.2),
                curve=0.2, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
}

########################################
### Schema Exposed + Hospitalisation ###
########################################

diagram.EH  = function(lwd = 2, col = getDiagramColors(),
		file = "Model_SEIHRD.png", save.png = FALSE,
		scaleX = 3/4, scaleY = 3/4) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res = 100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 10
  DiffMat <- matrix(data = 0, nrow = nComp, ncol = nComp)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(
    expression(S[Y]), #1
    expression(S[O]), #2
    expression(I[Y]), #3
    expression(I[O]), #4
    expression(H[Y]), #5
    expression(H[O]), #6
    "D", #7
    "R", #8
    expression(E[Y]), # 9
    expression(E[O])  # 10
    ) 
  
  color <-  c(col$S, col$S, col$I, col$I, 
              col$H, col$H, col$D, col$R, 
              col$E, col$E)
  
  # arrows 
  m[[9,1]]  = ""
  m[[10,2]] = ""
  m[[3,9]]  = ""
  m[[4,10]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[8,3]] = ""
  m[[6,4]] = ""
  m[[7,4]] = ""
  m[[8,4]] = ""
  m[[7,5]] = ""
  m[[8,5]] = ""
  m[[7,6]] = ""
  m[[8,6]] = ""
  
  ### Positions of boxes
  coord = matrix(nrow = nComp, ncol = 2)
  
  # Sy
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.2 * scaleY
  # So
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 - 0.2 * scaleY
  
  # Ey
  coord[9,1] = 0.5 - 0.2 * scaleX
  coord[9,2] = 0.5 + 0.2 * scaleY
  # Eo
  coord[10,1] = 0.5 - 0.2 * scaleX
  coord[10,2] = 0.5 - 0.2 * scaleY
  
  # Iy
  coord[3,1] = 0.5 
  coord[3,2] = 0.5 + 0.2 * scaleY
  # Io
  coord[4,1] = 0.5 
  coord[4,2] = 0.5 - 0.2 * scaleY
  
  # Hy
  coord[5,1] = 0.5 + 0.2 * scaleX
  coord[5,2] = 0.5 + 0.2 * scaleY
  # Ho
  coord[6,1] = 0.5 + 0.2 * scaleX
  coord[6,2] = 0.5 - 0.2 * scaleY
  
  # D
  coord[7,1] = 0.5 + 0.4 * scaleX
  coord[7,2] = 0.5 + 0.4 * scaleY
  
  # R
  coord[8,1] = 0.5 + 0.4 * scaleY
  coord[8,2] = 0.5 - 0.4 * scaleY

  ### Plot diagram
  plotMatrix(m, pos = coord, name = name, col = color,
	main = "SIR + Exposed Hospitalization Model",
	box.size = 0.0175, box.cex = 1.75, arr.lwd = lwd);
  
  ### Curved Arrows
  # - coordinates are hard coded;
  
  # from Iy to Sy & Io to So
  curvedArrows2(0.5, 0.5, dx = c(-0.015, -0.3), dy = c(0.295, 0.2),
                curve=0.7, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Hy to Sy & Ho to So
  curvedArrows2(0.5, 0.5, dx = c(+0.21, -0.3), dy = c(0.30, 0.2),
                curve=0.7, lcol = col$H,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Iy to So & Io to Sy
  curvedArrows2(0.5, 0.5, dx = c(-0.015, -0.3), dy = c(0.12, -0.2),
                curve=0.2, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
}

##########################
### Schema Vaccination ###
##########################

diagramV = function(lwd = 2, col = getDiagramColors(),
		file = "Model_SIR_Vaccination.png", save.png = FALSE,
		scaleX = 1/2, scaleY = 1/2) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units="in", res = 100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 9
  DiffMat <- matrix(data = 0, nrow = nComp, ncol = nComp)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(expression(V[Y]), #1
            expression(S[Y]), #2
            "H", #3
            expression(I[Y]), #4
            expression(I[O]), #5
            "D", #6
            expression(S[O]), #7
            "R", #8
            expression(V[O])) #9
  
  color <-  c(col$V, col$S, col$H, col$I, col$I, col$D, 
              col$S, col$R, col$V)
  
  # arrows 
  m[[1,2]] = ""
  m[[4,2]] = ""
  m[[5,7]] = ""
  m[[9,7]] = ""
  m[[3,4]] = ""
  m[[3,5]] = ""
  m[[6,4]] = ""
  m[[6,5]] = ""
  m[[8,4]] = ""
  m[[8,5]] = ""
  m[[6,3]] = ""
  m[[8,3]] = ""
  
  ### Positions of boxes
  coord = matrix(nrow = nComp, ncol = 2)
  
  # Vy
  coord[1,1] = 0.5 - 0.2 * scaleX
  coord[1,2] = 0.5 + 0.4 * scaleY
  
  # Sy
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 + 0.2 * scaleY
  
  # H
  coord[3,1] = 0.5 + 0.2 * scaleX
  coord[3,2] = 0.5 #+ 0.4 * scaleY
  
  # Iy
  coord[4,1] = 0.5
  coord[4,2] = 0.5 + 0.2 * scaleY
  
  # Io
  coord[5,1] = 0.5
  coord[5,2] = 0.5 -0.2 * scaleY
  
  # D
  coord[6,1] = 0.5 + 0.3 * scaleX
  coord[6,2] = 0.5 + 0.4 * scaleY
  
  # So
  coord[7,1] = 0.5 - 0.4 * scaleX
  coord[7,2] = 0.5 - 0.2 * scaleY
  
  # R
  coord[8,1] = 0.5 + 0.3 * scaleX
  coord[8,2] = 0.5 - 0.4 * scaleY
  
  # Vo
  coord[9,1] = 0.5 - 0.2 * scaleX
  coord[9,2] = 0.5 - 0.4 * scaleY
  
  ### Plot diagram
  plotMatrix(m, pos = coord, name = name, col = color,
	main = "SIR + Vaccination Model",
	box.size = 0.0175, box.cex = 1.75, arr.pos = 0.8, arr.lwd = lwd);
  
  ### Curved Arrows 
  # - coordinates are hard coded;

  # from I[Young] to S[Young]
  # & from I[Old] to S[Old]
  curvedArrows2(0.5, 0.5, dx = c(0, -0.15), dy = c(0.25, 0.22), 
                curve=0.7, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
  
  # from H to Sy & So
  curvedArrows2(0.5, 0.5, dx = c(0.2, -0.14), dy = c(0.10, 0.22), 
                curve=0.7, lcol = col$H,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Iy to So & Io to Sy
  curvedArrows2(0.5, 0.5, dx = c(0, -0.15), dy = c(0.15, -0.19), 
                curve=0.2, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
}

#####################################
### Schema Vaccination Stratified ###
#####################################

diagramVS  = function(lwd = 2, col = getDiagramColors(),
		file = "Model_SIR_Vaccination_AgeStratified.png", save.png = FALSE,
		scaleX = 3/4, scaleY = 3/4) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units="in", res = 100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 11
  DiffMat <- matrix(data = 0, nrow = nComp, ncol = nComp)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(expression(V[Y]), #1
            expression(S[Y]), #2
            expression(H[Y]), #3
            expression(H[O]), #4
            expression(I[Y]), #5
            expression(I[O]), #6
            expression(D[Y]), #7
            expression(D[O]), #8
            expression(S[O]), #9
            "R", #10
            expression(V[O])) #11
  
  color <-  c(col$V, col$S, col$H, col$H, col$I, col$I, 
              col$D, col$D, col$S, col$R, col$V)
  
  # arrows 
  m[[1,2]] = ""
  m[[5,2]] = ""
  m[[6,9]] = ""
  m[[11,9]] = ""
  m[[3,5]] = ""
  m[[4,6]] = ""
  m[[7,3]] = ""
  m[[7,5]] = ""
  m[[8,4]] = ""
  m[[8,6]] = ""
  m[[10,5]] = ""
  m[[10,6]] = ""
  m[[10,3]] = ""
  m[[10,4]] = ""
  
  ### Positions of boxes
  coord = matrix(nrow = nComp, ncol = 2)
  
  # V[Y]
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.9 * scaleY
  
  # V[O]
  coord[11,1] = 0.5 - 0.4 * scaleX
  coord[11,2] = 0.5 - 0.9 * scaleY
  
  # S[Y]
  coord[2,1] = 0.5 - 0.55 * scaleX
  coord[2,2] = 0.5 + 0.5 * scaleY
  
  # S[O]
  coord[9,1] = 0.5 - 0.55 * scaleX
  coord[9,2] = 0.5 - 0.5 * scaleY
  
  # H[Y]
  coord[3,1] = 0.5 + 0.2 * scaleX
  coord[3,2] = 0.5 + 0.5 * scaleY
  
  # H[O]
  coord[4,1] = 0.5 + 0.2 * scaleX#0.5 + 0.3 * scaleX
  coord[4,2] = 0.5 - 0.5 * scaleY#0.5 - 0.1 * scaleY
  
  # I[Y]
  coord[5,1] = 0.5 - 0.2 * scaleX
  coord[5,2] = 0.5 + 0.5 * scaleY
  
  # I[O]
  coord[6,1] = 0.5 - 0.2 * scaleX
  coord[6,2] = 0.5 - 0.5 * scaleY
  
  # D[Y]
  coord[7,1] = 0.5 + 0.45 * scaleX
  coord[7,2] = 0.5 + 0.9 * scaleY
  
  # D[O]
  coord[8,1] = 0.5 + 0.45 * scaleX
  coord[8,2] = 0.5 - 0.9 * scaleY
  
  # R
  coord[10,1] = 0.5 + 0.45 * scaleX#0.5 + 0.25 * scaleX
  coord[10,2] = 0.5 - 0.1 * scaleY#0.5 - 1 * scaleY
  
  
  ### Plot diagram
  plotMatrix(m, pos = coord, name = name, col = color,
	main = "SIR + Vaccination Stratified Model",
	box.size = 0.0175, box.cex = 1.75, arr.pos = 0.625, arr.lwd = lwd);
  
  ### Curved arrows 
  # - coordinates are hard coded;
  
  # from Iy to Sy & Io to So
  curvedArrows2(0.5, 0.5, dx = c(-0.2, -0.4), dy = c(0.6, 0.53), 
                curve=0.7, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Hy to Sy & Ho to So
  curvedArrows2(0.5, 0.5, dx = c(0.2, -0.3), dy = c(0.6, 0.53), 
                curve=0.7, lcol = col$H,
                scaleX=scaleX, scaleY=scaleY);
  
  # from Iy to So & Io to Sy
  curvedArrows2(0.5, 0.5, dx = c(-0.2, -0.4), dy = c(0.4, -0.48), 
                curve=0.1, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
}

########################
### Schema 2 Viruses ###
########################

diagram.2V = function(lwd = 2, col = getDiagramColors(),
		file = "2 Virusess.png", save.png = FALSE,
		scaleX = 1/3, scaleY = 1/3){
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res=100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 9;
  Diffmatrix = matrix(data = 0, nrow = nComp, ncol = nComp)
  
  m <- as.data.frame(Diffmatrix)
  
  # names and colors of boxes
  name <- c("S", #1
            expression(IV1), #2
            expression(IV2), #3 
            expression(HV1), #4 
            expression(HV2), #5
            expression(DV1), #6 
            expression(DV2), #7
            expression(RV1), #8
            expression(RV2)  #9
            )
  
  color <-  c(col$S, col$I, col$I, 
              col$H, col$H, col$D, 
              col$D, col$R, col$R)
  
  # arrows
  m[[2,1]] = ""
  m[[3,1]] = ""
  m[[4,2]] = ""
  m[[6,2]] = ""
  m[[8,2]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[9,3]] = ""
  m[[6,4]] = ""
  m[[8,4]] = ""
  m[[7,5]] = ""
  m[[9,5]] = ""
  
  ### Positions of boxes
  coord = matrix(nrow = nComp, ncol = 2)
  
  # S
  coord[1,1] = 0.5 - 0.8 * scaleX
  coord[1,2] = 0.5 # - 0.2 * scaleY
  
  # IV1[1]
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 + 0.4 * scaleY
  # IV2[2]
  coord[3,1] = 0.5 - 0.4 * scaleX
  coord[3,2] = 0.5 - 0.4 * scaleY
  
  # HV1[1]
  coord[4,1] = 0.5 - 0.1 * scaleX
  coord[4,2] = 0.5 + 1.2 * scaleY
  # HV2[2]
  coord[5,1] = 0.5 - 0.1 * scaleX 
  coord[5,2] = 0.5 - 1.2 * scaleY
  
  # DV1[1]
  coord[6,1] = 0.5 + 0.3 * scaleX
  coord[6,2] = 0.5 + 1.2 * scaleY
  # DV2[2]
  coord[7,1] = 0.5 + 0.3 * scaleX
  coord[7,2] = 0.5 - 1.2 * scaleY
  
  # RV1[1]
  coord[9,1] = 0.5 + 0.3 * scaleX
  coord[9,2] = 0.5 - 0.4 * scaleY
  # RV2[2]
  coord[8,1] = 0.5 + 0.3 * scaleX
  coord[8,2] = 0.5 + 0.4 * scaleY
  
  ### Plot diagram
  plotMatrix(m, pos = coord, name = name, col = color,
	main = "SIR: 2 Viruses",
	box.size = 0.0175, box.cex = 1.625, arr.pos = 0.625, arr.lwd = lwd);
  
  ### Curved arrows 
  # - coordinates are hard coded;
  
  # from I to S
  curvedArrows2(0.5, 0.5, dx = c(-0.45, -0.6), dy = c(0.6, 0.27), 
                curve = 0.2, lcol = col$I,
                scaleX=scaleX, scaleY=scaleY);
  
  # from H to S
  curvedArrows2(0.5, 0.5, dx = c(-0.16, -0.65), dy = c(1.22, 0.27), 
                curve = 0.15, lcol = col$H,
                scaleX=scaleX, scaleY=scaleY, arr.pos = 0.975);
}

##########################
### Diagram Age Groups ###
##########################

diagram.AG3 = function(lwd = 2, col = getDiagramColors(),
		file = "Age Groups Model.png", save.png = FALSE,
		scaleX = 1/3, scaleY = 1/3) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res=100)
  } else {
    # dev.new(width = 11.7, height = 8.3)
  }
  
  # Number of compartments in the SIR model
  nComp = 12;
  Diffmatrix = matrix(data = 0, nrow = nComp, ncol = nComp)
  
  m <- as.data.frame(Diffmatrix)
  
  # names and colors of boxes
  name <- c(expression(Sc), #1
            expression(Sa), #2
            expression(So), #3 
            expression(Ic), #4 
            expression(Ia), #5
            expression(Io), #6 
            expression(Hc), #7
            expression(Ha), #8
            expression(Ho), #9
            expression(Dc/Rc), #10
            expression(Da/Ra), #11
            expression(Do/Ro) #12
            # expression(Rc), #13
            # expression(Ra), #14
            # expression(Ro)  #15
            
  )
  
  color <-  c(col$S, col$S, col$S, 
              col$I, col$I, col$I, 
              col$H, col$H, col$H, 
              col$R, col$R, col$R)
  
  # arrows
  m[[4,1]] = ""
  m[[5,2]] = ""
  m[[6,3]] = ""
  m[[7,4]] = ""
  m[[8,5]] = ""
  m[[9,6]] = ""
  m[[10,4]] = ""
  m[[11,5]] = ""
  m[[12,6]] = ""
  m[[10,7]] = ""
  m[[11,8]] = ""
  m[[12,9]] = ""
  
  ### Positions of boxes
  coord = matrix(nrow = nComp, ncol = 2)
  
  # Sc
  coord[1,1] = 0.5 - 0.9 * scaleX
  coord[1,2] = 0.5 + 1.1 * scaleY
  # Sa
  coord[2,1] = 0.5 - 0.9 * scaleX
  coord[2,2] = 0.5 + 0.1 * scaleY
  # So
  coord[3,1] = 0.5 - 0.9 * scaleX
  coord[3,2] = 0.5 - 0.9 * scaleY
  
  # Ic
  coord[4,1] = 0.5 - 0.3 * scaleX
  coord[4,2] = 0.5 + 1.1 * scaleY
  # Ia
  coord[5,1] = 0.5 - 0.3 * scaleX
  coord[5,2] = 0.5 + 0.1 * scaleY
  # Io
  coord[6,1] = 0.5 - 0.3 * scaleX
  coord[6,2] = 0.5 - 0.9 * scaleY
  
  # Hc
  coord[7,1] = 0.5 + 0.3 * scaleX
  coord[7,2] = 0.5 + 1.1 * scaleY
  # Ha
  coord[8,1] = 0.5 + 0.3 * scaleX
  coord[8,2] = 0.5 + 0.1 * scaleY
  # Ho
  coord[9,1] = 0.5 + 0.3 * scaleX
  coord[9,2] = 0.5 - 0.9 * scaleY
  
  # Dc
  coord[10,1] = 0.5 + 0.9 * scaleX
  coord[10,2] = 0.5 + 2.1 * scaleY
  # Da
  coord[11,1] = 0.5 + 0.9 * scaleX
  coord[11,2] = 0.5 + 1 * scaleY
  # Do
  coord[12,1] = 0.5 + 0.9 * scaleX
  coord[12,2] = 0.5 # - 0.3 * scaleY
  
  ### Plot diagram
  plotMatrix(m, pos = coord, name = name, col = color,
	main = "SIR Model: 3 Age Groups",
	box.size = 0.0175, box.cex = 1.5, arr.pos = 0.625, arr.lwd = lwd);
}
