% File src/library/EpidemicSimulator/man/getOptions.Rd
\name{getOptions}
\alias{}
\title{Provides various default options for the web app}
\description{
  Provides specific default values for various global options: special settings of the compartmental models, parameter limits in the sensitivity analysis, line type in plots, etc.
  
  These values are set as default values in the web app. The values of these options can be viewed in the web page Options.
  
  Details:
  The function is called internally when the shiny server is started.
}
\usage{
	getOptions()
}
\arguments{
}
\seealso{
	\code{\link{getOptionsUI}}.
}
\examples{
# print the default values
getOptions()

\keyword{options}