######################
###
### Epidemic Simulator
### - Bachelor Thesis 2022 -
### Student: Anamaria Bica
### West University, Timisoara
### Year 3, Faculty of Computer Science
###
### Coordinator: Prof. Dr. Daniela Zaharie
### Supervisor: Dr. med. Leonard Mada
###   Syonic SRL
###


### Parts of the work are based on:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL
###
### ========================
### L. Mada: modified/improved;


### draft v.0.2a
# - split into modules;


#######################

# library(shiny)
# library(shinyjs)

# library(rsconnect)


### Options
#' @export
getOptionsUI = function() {
	list(
		H = list(hosp.young = 0.01, hosp.old = 0.1,
			death.young = 0.001, death.old = 0.05, death.h = 0.07),
		EH = list(hosp.young = 0.01, hosp.old = 0.1,
			death.young = 0.001, death.old = 0.05, death.h = 0.07,
            exposed.y = 0.2, exposed.o = 0.2),
		V = list(hosp.young = 0.01, hosp.old = 0.1,
			death.young = 0.001, death.old = 0.05, death.h = 0.07),
		# n = non-children (others);
		AG3 = list(inf.cc = 0.35, inf.cn = 0.25, inf.nc = 0.25, inf.nn = 0.25,
		hosp.c = 0.01, hosp.a = 0.01, hosp.o = 0.1)
	);
}

getVaccinationScale = function() {
	opt = getOptions();
	return(opt$vaccine.rate.scale);
}


### Helper Functions
sliderTime = function(id, label = "Number of days", val = 200, max = 720, step = 1) {
  sliderInput(inputId = id, label = label,
              value = val, min = 1, max = max, step = step)
}
sliderBase = function(id, label, val = 0.05, min = 0, max = 0.8, step = 0.001) {
  sliderInput(inputId = id, label=label,
              value = val, min = min, max = max, step = step)
}


sliderVaccine = function(id, label, val = 0.001, min = 0, max = 0.01, step = 0.0002,
		scale = getVaccinationScale()) {
  sliderInput(inputId = id, label=label,
              value = val * scale, min = min * scale, max = max * scale, step = step * scale);
}

headerRow = function(idCntrl, lblFilter, selFilter, lblSensitivity, selSens) {
	fluidRow(
		column(4,
			textOutput(idCntrl[[1]]),
			checkboxInput(idCntrl[[2]], "Toggle between plot and diagram") ), 
		column(4, selectInput(idCntrl[[3]], "Display", lblFilter, selected=selFilter) ),
		column(4, selectInput(idCntrl[[4]], "Sensitivity Analysis", 
			lblSensitivity, selected=selSens) ),
		tipSensitivity(idCntrl[[4]])
	)
}
tipSensitivity = function(idCntrl) {
	shinyBS::bsTooltip(id = idCntrl, title = helpTipSensitivity(TRUE), 
		placement = "left", trigger = "hover");
}


### UI
getUI = function(opt = NULL) {
	if(is.null(opt)) opt = getOptionsUI();
	fluidPage("Epidemic Simulation", shinyjs::useShinyjs(),
		tabsetPanel(
			tabPanel("Hospitalisation SIR Model",
				headerRow(
					c("txtHosp", "toggleH", "optTypeH", "optSensitivityH"),
					getDisplayTypes(), selFilter="Old",
					getSensitivity_Hosp(), selSens="SIR"),
				plotOutput("Hosp"),
				
				fluidRow(
					column(4, sliderTime("timeH"),
						sliderBase("recov.cH", label="Recovery rate"),
						sliderBase("recov.hH", label="Recovery rate (Hosp)") ),
					column(4,
						sliderBase("infectH", label="Infection rate", 0.25),
						sliderBase("hosp.oH", label="Hospitalization rate (Old)", opt$H$hosp.old),
						sliderBase("hosp.yH", label="Hospitalization rate (Young)", opt$H$hosp.young)
					),
					column(4,
						sliderBase("death.oH", label="Death rate (Old)", opt$H$death.old),
						sliderBase("death.yH", label="Death rate (Young)", opt$H$death.young),
						sliderBase("death.hH", label="Death rate (Hosp)", opt$H$death.h) )
			)),
			
			tabPanel("Exposed Hospitalisation Model",
				headerRow(
					c("txtEH", "toggleEH", "optTypeEH", "optSensitivityEH"),
					getDisplayTypesEH(), selFilter="Old",
					getSensitivity_EH(), selSens="SEIR"),
				plotOutput("EH"),
				
				fluidRow(
					column(4, sliderTime("timeEH"),
						sliderBase("exposed.yEH", label="Exposed rate (Young)", opt$EH$exposed.y),
						sliderBase("exposed.oEH", label="Exposed rate (Old)", opt$EH$exposed.y),
						sliderBase("recov.hEH", label="Recovery rate (Hosp)") ),
					column(4,
						sliderBase("infectEH", label="Infection rate", 0.25),
						sliderBase("hosp.oEH", label="Hospitalization rate (Old)", opt$EH$hosp.old),
						sliderBase("hosp.yEH", label="Hospitalization rate (Young)", opt$EH$hosp.young),
						sliderBase("recov.cEH", label="Recovery rate") ),
					column(4,
						sliderBase("death.oEH", label="Death rate (Old)", opt$EH$death.old),
						sliderBase("death.yEH", label="Death rate (Young)", opt$EH$death.young),
						sliderBase("death.hEH", label="Death rate (Hosp)", opt$EH$death.h) )
			)),
			
			tabPanel("Basic SIR Model",
				textOutput("txtBasic"),
				checkboxInput("toggleB", "Toggle between diagram and plot", value=FALSE), 
				plotOutput("BasicPl"),
				hr(),
                           
				fluidRow(
					column(4, sliderTime("timeB")),
					column(4, sliderBase("infectB", label="Infection rate", 0.25)),
					column(4, sliderBase("recovB", label="Recovery rate"))
			)),
			
			tabPanel("Vaccination SIR Model",
				headerRow(
					c("txtVacc", "toggleV", "optTypeV", "optSensitivityVacc"),
					getDisplayTypesVaccine(), selFilter="Old",
					getSensitivityVaccine(), selSens="Vacc"),
				plotOutput("Vacc"),
                           
				fluidRow(
					column(3, sliderTime("timeV"),
						sliderBase("recovV", label="Recovery rate"),
						sliderBase("recov.hV", label="Recovery rate (Hosp)") ),
					column(3,
						sliderBase("infectV", label="Infection rate", 0.25),
						sliderBase("hosp.vV", label="Hospitalization rate (Old)", opt$V$hosp.old),
						sliderBase("hosp.yV", label="Hospitalization rate (Young)", opt$V$hosp.young)
					),
					column(3,
						sliderBase("death.oV", label="Death rate (Old)", opt$V$death.old),
						sliderBase("deathV", label="Death rate (Young)", opt$V$death.young),
						sliderBase("death.hV", label="Death rate (Hosp)", opt$V$death.h)),
					column(3,
						sliderVaccine("vacc.oV", label="Vaccination rate (Old%) "),
						sliderVaccine("vacc.yV", label="Vaccination rate (Young%) ") )
			)),
			
			tabPanel("Vaccination Stratified Model",
				headerRow(
					c("txtVaccStratified", "toggleVS", "optTypeVS", "optSensitivityVaccStrat"),
					getDisplayTypesVaccineStrat(), selFilter="Old",
					getSensitivityVaccineStrat(), selSens="VaccStrat"),
				plotOutput("VaccVS"),
                           
				fluidRow(
                             column(3, sliderTime("timeVS"),
                                    sliderBase("infectVS", label="Infection rate", 0.25),
                                    sliderBase("hosp.oVS", label="Hospitalization rate (Old)"),
                                    sliderBase("hosp.yVS", label="Hospitalization rate (Young)")
                             ),
                             column(3,
                                    sliderBase("recov.oVS", label="Recovery rate (Old)"),
                                    sliderBase("recov.yVS", label="Recovery rate (Young)"),
                                    sliderBase("recov.hVS", label="Recovery rate (Hosp)")
                             ),
                             column(3,
                                    sliderBase("death.oVS", label="Death rate (Old)"),
                                    sliderBase("death.yVS", label="Death rate (Young)"),
                                    sliderBase("death.hyVS", label="Death rate in hospital (Young)")
                             ),
                             
                             column(3,
                                    sliderBase("death.hoVS", label="Death rate in hospital (Old)"),
                                    sliderVaccine("vacc.oVS", label="Vaccination rate (Old%)"), #, max = 0.01, val = 0.001,  step = 2E-4)
                                    sliderVaccine("vacc.yVS", label="Vaccination rate (Young%)" ) #, max = 0.01, val = 0.001,  step = 2E-4),
                             )
			)),
			
			tabPanel("Two Viruses Model",
				headerRow(
					c("txtTwoVirus", "toggle2V", "optType2V", "optSensitivity2V"),
					getDisplayTypes2Viruses(), selFilter="Compact",
					getSensitivity2Viruses(), selSens="2V"),
				plotOutput("Virus"),
				
				column(3, sliderTime("time2V", label = "Time", 400),
					sliderTime("delayV2", label = "Time delay", 110, max = 200),
					sliderBase("recovV1", label="Recovery rate (Virus 1)"),
					sliderBase("recovV2", label="Recovery rate (Virus 2)") ),
				column(3,
					sliderBase("infectV1", label="Infection rate (Virus 1)", 0.15),
					sliderBase("infectV2", label="Infection rate (Virus 2)", 0.25),
					sliderBase("recovV1.h", label="Recovery rate (Hosp, Virus 1)", 0.1),
					sliderBase("recovV2.h", label="Recovery rate (Hosp, Virus 2)", 0.1) ),
				column(3,
					sliderBase("hospV1", label="Hospitalization rate (Virus 1)"),
					sliderBase("hospV2", label="Hospitalization rate (Virus 2)") ),
				column(3,
					sliderBase("deathV1", label="Death rate (Virus 1)", 0.1),
					sliderBase("deathV2", label="Death rate (Virus 2)", 0.15),
					sliderBase("deathV1.h", label="Death rate (Hosp, Virus 1)"),
					sliderBase("deathV2.h", label="Death rate (Hosp, Virus 2)") )
			),
			
			tabPanel("Age Groups Stratified Model",
				headerRow(
					c("txtAG3", "toggleAG3", "optTypeAG3", "optSensitivityAG3"),
					getDisplayTypesAG3(), selFilter="Old",
					getSensitivityAG3(), selSens="AG3"),
				plotOutput("AgeGroupsModel"),
				
				fluidRow(
					column(3,
						sliderTime("timeAG3"),
						sliderBase("recovAG3.c", label="Recovery rate children"),
						sliderBase("recovAG3.a", label="Recovery rate adults"),
						sliderBase("recovAG3.o", label="Recovery rate elders"),
						sliderBase("recovAG3.hc", label="Recovery rate children (Hosp)")
					),
					column(3,
						sliderBase("infectAG3.cc", label="Infection rate btw children", opt$AG3$inf.cc),
						sliderBase("infectAG3.cn", label="Infection rate children -> others", opt$AG3$inf.cn),
						sliderBase("infectAG3.nn", label="Infection rate others -> others", opt$AG3$inf.nn),
						sliderBase("infectAG3.nc", label="Infection rate others -> children", opt$AG3$inf.nc),
						sliderBase("recovAG3.ha", label="Recovery rate adults (Hosp)") ),
					column(3,
						sliderBase("hospAG3.c", label="Hospitalization rate children", opt$AG3$hosp.c),
						sliderBase("hospAG3.a", label="Hospitalization rate adults", opt$AG3$hosp.a),
						sliderBase("hospAG3.o", label="Hospitalization rate elders", opt$AG3$hosp.o),
						sliderBase("deathAG3.ho", label="Death rate elders (Hosp)"),
						sliderBase("recovAG3.ho", label="Recovery rate elders (Hosp)") ),
					column(3,
						sliderBase("deathAG3.o", label="Death rate elders"),
						sliderBase("deathAG3.a", label="Death rate adults"),
						sliderBase("deathAG3.c", label="Death rate children"),
						sliderBase("deathAG3.ha", label="Death rate adults (Hosp)"),
						sliderBase("deathAG3.hc", label="Death rate children (Hosp)") )
				)
			), 
			
			### Analysis
			tabPanel("Analysis",
				uiOutput("hrAnalysis"),
				tableOutput("doAnalysis"),
				uiOutput("hrStatistics"),
				tableOutput("doBasicStatistics"),
				downloadButton("downloadData", "Download")
			),
			
			### Other
			tabPanel("Model Options",
				tableOutput("printOptions")
			),
			
			tabPanel("Help",
				uiOutput("HelpUI")
			),
			
			tabPanel("About",
				uiOutput("AboutUI")
			)
		)
	)
}

##############

# to implement
# tabsets (with their own models and parameters)
# fancy(-er) display (plot up, params down)