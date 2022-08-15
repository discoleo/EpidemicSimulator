% File src/library/EpidemicSimulator/man/getDiagramColors.Rd
\name{getDiagramColors}
\alias{getDiagramColors}
\title{Provides default colors for the model diagrams}
\description{
  Provides default colors for the diagrams corresponding to the various compartmental models. These values are set as default values when starting the web app.
  
  Details:
  The function is called internally when the shiny server is started. The values are stored as a reactive value in the server.
}
\usage{
	getDiagramColors()
}
\arguments{
}
\seealso{
}
\examples{
# print the default colors for all diagrams
getDiagramColors()
}
\keyword{options}