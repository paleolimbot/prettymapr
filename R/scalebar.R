#scalebar


# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf) from: http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
torad <- function(deg) {
  deg*pi/180.0
}

geodist <- function(long1, lat1, long2, lat2) {
  long1 <- torad(long1)
  lat1 <- torad(lat1)
  long2 <- torad(long2)
  lat2 <- torad(lat2)
  R <- 6371009 # Earth mean radius [m]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in m
}

fromsi <- function(sivalue, unit) {
  if(unit == "km") {
    sivalue / 1000.0
  } else if(unit == "m") {
    sivalue
  } else if(unit =="ft") {
    sivalue * 3.28084
  } else if(unit == "mi") {
    sivalue / 1609.344051499
  } else if(unit == "in") {
    sivalue * 39.370079999999809672
  } else if(unit == "cm") {
    sivalue * 100.0
  }
}

tosi <- function(unitvalue, unit) {
  if(unit == "km") {
    unitvalue * 1000.0
  } else if(unit == "m") {
    unitvalue
  } else if(unit =="ft") {
    unitvalue / 3.28084
  } else if(unit == "mi") {
    unitvalue * 1609.344051499
  } else if(unit == "in") {
    unitvalue / 39.370079999999809672
  } else if(unit == "cm") {
    unitvalue / 100.0
  }
}

scalebarparams <- function(widthhint=0.25, unitcategory="metric", plotunit="m") {
  #params check
  if(!(plotunit %in% c("latlon", "m"))) stop("Unrecognized plotunit: ", plotunit)
  if(!(unitcategory %in% c("metric", "imperial"))) stop("Unrecognized unitcategory: ", unitcategory)

  extents <- par('usr')
  if(plotunit == "latlon") {
    heightm <- geodist(extents[1], extents[3], extents[1], extents[4])
    widthbottom <- geodist(extents[1], extents[3], extents[2], extents[3])
    widthtop <- geodist(extents[1], extents[4], extents[2], extents[4])
    cat("width bottom:", widthbottom, "; width top: ", widthtop, "\n")
    widthm <- mean(widthbottom, widthtop)
    mperplotunit <- widthm/(extents[2]-extents[1])
  } else {
    heightm <- tosi(extents[4] - extents[3], plotunit)
    widthm <- tosi(extents[2] - extents[1], plotunit)
    mperplotunit <- tosi(1.0, plotunit)
  }

  geowidthm <- widthm * widthhint

  if(geowidthm < 1) {
    scaleunits <- c("cm", "in")
  } else if(geowidthm < 1600) {
    scaleunits <- c("m", "ft")
  } else {
    scaleunits <- c("km", "mi")
  }

#   String unit = units[unitCategory] ;
  if(unitcategory == "metric") {
    unit <- scaleunits[1]
  } else {
    unit <- scaleunits[2]
  }
#   double widthHintU = Units.fromSI(geoWidthM, unit) ;
  widthhintu <- fromsi(geowidthm, unit)
#   double tenFactor = Math.floor(Math.log10(widthHintU)) ;
  tenfactor <- floor(log10(widthhintu))
#   double widthInTens = Math.floor(widthHintU / Math.pow(10, tenFactor)) ;
  widthintens <- floor(widthhintu / (10^tenfactor))
  if(widthintens == 1) {
    widthintens <- 10
    tenfactor = tenfactor - 1 ;
  } else if(widthintens == 7) {
    widthintens <- 6
  } else if(widthintens == 9) {
    widthintens <- 8
  }

  if(widthintens < 6) {
    majdivtens <- 1
  } else {
    majdivtens <- 2
  }

#   double widthU = widthInTens * Math.pow(10, tenFactor) ;
  widthu <- widthintens * 10^tenfactor
#   double majorDiv = majDivTens * Math.pow(10, tenFactor) ;
  majordiv <- majdivtens * 10^tenfactor
#   long majorDivs = Math.round(widthU / majorDiv) ;
  majordivs <- round(widthu / majordiv)
#   double widthPx = Units.toSI(widthU, unit) / mPerPixel ;
  widthplotunit <- tosi(widthu, unit) / mperplotunit
#   double majorDivPx = widthPx / majorDivs ;
  majordivplotunit <- widthplotunit / majordivs
#   this.scaleParameters = new double[] {widthU, majorDiv, widthPx, majorDivPx} ;
  params = list()
  params$widthu <- widthu
  params$majordivu <- majordiv
  params$majordivs <- majordivs
  params$widthplotunit <- widthplotunit
  params$majordivplotunit <- majordivplotunit
  params$labeltext <- paste(as.integer(widthu), unit)
#   this.labelText = String.valueOf(Math.round(widthU)) + " " + unit ;
  params

}

drawscalebar <- function(x, y, ht, params, style="bar", adj=c(0,0)) {
  if(style=="bar") {
    wd <- params$widthplotunit
    cols <- rep(c("black", "white"), params$majordivs/2+1)
    for(i in 1:params$majordivs) rect(x-adj[1]*wd+(i-1)*params$majordivplotunit, y-adj[2]*ht+ht,
                                      x-adj[1]*wd+i*params$majordivplotunit, y-adj[2]*ht, col=cols[i])
  } else {
    stop("Invalid style specified to drawscalebar: ", style)
  }

}

scalebar <- function(plotunit, widthhint=0.25, unitcategory="metric", htin=0.1, padin=c(0.1, 0.1),
                     labelpadin=0.08, label.cex=0.8, pos="bottomleft") {
  extents <- par('usr')
  params <- scalebarparams(plotunit=plotunit, widthhint = widthhint, unitcategory=unitcategory)

  bottomin <- grconvertY(extents[3], from="user", to="inches")
  leftin <- grconvertX(extents[1], from="user", to="inches")
  topin <- grconvertY(extents[4], from="user", to="inches")
  rightin <- grconvertX(extents[2], from="user", to="inches")

  ht <- grconvertY(bottomin+htin, from="inches", to="user") - extents[3]
  paduser <- grconvertX(leftin+labelpadin, from="inches", to="user") - extents[1]

  if(pos=="bottomleft") {
    x <- grconvertX(leftin+padin[1], from="inches", to="user")
    y <- grconvertY(bottomin+padin[2], from="inches", to="user")
    adj <- c(0,0)
    textadj <- c(0,0)
    textx <- x+params$widthplotunit+paduser
    texty <- y
  } else if(pos=="topleft") {
    x <- grconvertX(leftin+padin[1], from="inches", to="user")
    y <- grconvertY(topin-padin[2], from="inches", to="user")
    adj <- c(0,1)
    textadj <- c(0, 0)
    textx <- x+params$widthplotunit+paduser
    texty <- y-ht
  } else if(pos=="topright") {
    x <- grconvertX(rightin-padin[1], from="inches", to="user")
    y <- grconvertY(topin-padin[2], from="inches", to="user")
    adj <- c(1,1)
    textadj <- c(1, 0)
    textx <- x-params$widthplotunit-paduser
    texty <- y-ht
  } else if(pos=="bottomright") {
    x <- grconvertX(rightin-padin[1], from="inches", to="user")
    y <- grconvertY(bottomin+padin[2], from="inches", to="user")
    adj <- c(1,0)
    textadj <- adj
    textx <- x-params$widthplotunit-paduser
    texty <- y
  }

  drawscalebar(x, y, ht, params, adj=adj)
  text(textx, texty, params$labeltext, adj=textadj, cex=label.cex)
}

