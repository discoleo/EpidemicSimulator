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

### based of:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL

# install.packages(diagram)

# library(diagram)

### Functions

# Function for generating two complementary arrows
curvedArrows2 = function(x, y, dx, dy, curve, lcol, scaleX, scaleY, arr.pos=0.95) {
  if(length(curve) == 1) curve = c(curve, -curve);
  # Arrow 1:
  diagram::curvedarrow(
    from = c(x + dx[1] * scaleX, y + dy[1] * scaleY),
    to   = c(x + dx[2] * scaleX, y + dy[2] * scaleY),
    curve = curve[1], arr.pos = arr.pos, lcol=lcol)
  
  # Complementary Arrow:
  diagram::curvedarrow(
    from = c(x + dx[1] * scaleX, y - dy[1] * scaleY),
    to   = c(x + dx[2] * scaleX, y - dy[2] * scaleY),
    curve = curve[2], arr.pos = arr.pos, lcol=lcol)
}

