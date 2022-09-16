% File src/library/EpidemicSimulator/man/getOptions.Rd
\name{getOptions}
\alias{getOptions}
\title{Provides various default options for the web app}
\description{
  Provides specific default values for various global options: e.g. special settings of the compartmental models, parameter limits in the sensitivity analysis, line type in plots, etc.
  
  These values are set as default values in the web app. The values of these options can be viewed in the web page Options.
  
  Details:
  The function is called internally when the shiny server is started. Functionality is not yet available to change dynamically these values.
  
  The global options are:
  
  \bold{Models}
  \itemize{
  \item p.old = proportion of old persons;
  \item p.children = proportion of children in models with a compartment for children;
  \item delay.vacc = time delay until vaccination starts;
  \item opt.2V.permutation = proportion of virus 1 that mutates to virus 2; these mutations affect currently only the I compartment and are a one-time event implemented using a stop-and-go approach;
  }
  
  \bold{Plot}
  \itemize{
  \item death.rate.scale = scale death rate;
  \item hosp.rate.scale = scale hospitalisation rate;
  }
  
  \bold{UI}
  \itemize{
  \item vaccine.rate.scale = scale values on the rate of vaccination slider;
  }
  
  \bold{Sensitivity Analysis}
  \itemize{
  \item sensitivity.lty = line type used to plot results of the sensitivity analysis;
  \item sensitivity.infect.min = factor used to compute the minimum value of the infection rate (based on the current value);
  \item sensitivity.infect.max = factor used to compute the maximum value of the infection rate (based on the current value);
  }
  
  \bold{Analysis}
  \itemize{
  \item stat.max.cutoff = cutoff used to extract the days with more events that this cutoff (relative value);
  \item population.size = results are reported as absolute number of persons in a population of this size;
  }
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
}

\concept{options}