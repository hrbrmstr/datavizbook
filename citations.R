pkg <- c("aplpack", "binom", "car", "colorspace",
         "devtools", "effects", "gdata", "ggdendro",
         "ggmap", "ggplot2", "ggthemes", "grid",
         "gridExtra", "lattice", "maps", "maptools",
         "plyr", "portfolio", "RColorBrewer", "rgdal",
         "rjson", "RJSONIO", "scales", "splines",
         "vcd", "verisr", "zoo")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
      install.packages(new.pkg)
}
for (i in pkg) {
      x <- citation(i)
  for(j in x$textVersion) {
          cat(paste0(i,":"), j, "\n")
    }
}
