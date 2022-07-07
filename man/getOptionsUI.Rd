% File src/library/EpidemicSimulator/man/getOptionsUI.Rd
\name{getOptionsUI}
\alias{getOptionsUI}
\title{Provides various default values for the UI of the web app}
\description{
  Provides specific default values for the parameters of the various compartmental models. These values are set as default values in the UI of the web app.
  
  Note: not yet fully implemented. Only some parameters in some of the models are set through these options.
}
\usage{
getOptionsUI()
}
\arguments{
}
\seealso{
}
\examples{
# print the default values
getOptionsUI()

# start simulator with modified default values
opt = getOptionsUI()
opt$H$hosp.old = 0.2;
startSimulator(opt)
}
\keyword{file}