\name{prettymapr-package}
\alias{prettymapr-package}
\alias{prettymapr}
\docType{package}
\title{
Scale Bar, North Arrow, And Pretty Margins In R
}
\description{
Contains functions to automatically plot north arrows and
scalebars with minimal effort. Bounding box tools query online sources
for lat/lon bounding box of human-readable names.
}
\details{
\tabular{ll}{
Package: \tab prettymapr\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2015-10-10\cr
License: \tab GPL-2\cr
}
This package automates the process of creating a \link{scalebar} and \link{northarrow} in
any package that uses base graphics to plot in R. Bounding box tools \link{searchbbox} and
\link{zoombbox} help find and manipulate extents. Finally \link{prettymap} automates the
process of setting margins, plotting the map, scalebar, and north arrow, and resetting
graphic parameters upon completion.
}
\author{
Dewey Dunnington <dewey@fishandwhistle.net>
}
\keyword{ package }
\examples{

\donttest{
#bbox functions make it easy to manipulate bounding boxes
wolfville <- searchbbox("wolfville ns")
wolfvillezoomedout <- zoombbox(wolfville, 0.5)

#scalebar() makes it easy to add a scalebar to any map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-66.86, -59.75), ylim=c(43, 47.3)) #Nova Scotia
scalebar()

#also works in non-lat/lon coordinate systems
scalebar(plotepsg=3395) #specify plot is in mercator projection
scalebar(plotepsg=26920) #specify plot is in UTM Zone 20N

#northarrow() adds a north arrow
northarrow()

#prettymap() sets margins and plots scalebar and north arrow
library(maptools)
data(wrld_simpl)
prettymap(plot(wrld_simpl, xlim=c(-66.86, -59.75), ylim=c(43, 47.3)),
          arrow.scale=1.1)
}

}
