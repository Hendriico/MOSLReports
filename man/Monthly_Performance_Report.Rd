\name{monthly_performance_report}
\alias{monthly_performance_report}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
monthly_performance_report
}
\description{
Creates the monthly performance report
}
\usage{
monthly_performance_report(dir = choose.dir(), output_dir, period = Sys.Date() m- months(1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
Path to the Analytics folder
}
  \item{output_dir}{
Path to the folder you want the output file to be in. By default it is the Analytics/Monthly folder
  }
  \item{period}{
Which month's data to run the report for. By default it is the month before the current month
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
monthly_performance_report(dir = "C:\\Documents\\Analytics",
                           output_dir = dir,
                           period = as.Date("2019-10-20"))
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
