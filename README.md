# Prettymapr: Tools for rapid, publication-grade mapping in R

[![](http://cranlogs.r-pkg.org/badges/prettymapr)](https://cran.r-project.org/package=prettymapr)

Prettymapr automates the process of creating a scale bar and north arrow in any package that uses base graphics to plot in R. Bounding box tools help find and manipulate extents. Finally, there is a function to automate the process of setting margins, plotting the map, scale bar, and north arrow, and resetting graphic parameters upon completion.

The gist of it:


```R
install.packages("rosm") 
install.packages("prettymapr") #if these are not installed already

library(rosm)
library(prettymapr)
osm.plot(searchbbox("wolfville, ns"))
bmaps.plot(searchbbox("wolfville, ns"))

#or use prettymapr to remove margins and add scale bar
prettymap(bmaps.plot(searchbbox("wolfville, ns")))
```

Find more information on the [CRAN package page](https://cran.r-project.org/package=prettymapr) or [view the manual](https://cran.r-project.org/web/packages/prettymapr/prettymapr.pdf).
