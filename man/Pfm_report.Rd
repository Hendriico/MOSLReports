\name{Pfm_reports}
\alias{Pfm_reports}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pfm_reports
}
\description{
Creates the Pfm reports for all the Trading Parties
}
\usage{
Pfm_reports(dir = choose.dir(), output = dir, render_list = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
Path to the Analytics folder
}
  \item{output_dir}{
Path to the folder you want the folder for the output files to be created in. By default it is the Analytics folder.
  }
  \item{render_list}{
  List of Trading Parties to produce the reports for. If it's left empty, reports will be produced for all Trading Parties.
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
Pfm_reports(dir = "C:\\Documents\\Analytics")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
