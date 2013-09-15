library("rjson")

json_file <- "~/Documents/github/VCDB/incidents/osint999.json"
foo <- readLines(json_file)
infile <- paste(scan(json_file, what=" "), collapse=" ")
read.table(json_file)
json_data <- RJSONIO::fromJSON(paste(, collapse=""))

scan(json_file, what="character")
library("RJSONIO")

document <- fromJSON(file=json_file, method='C')

readjson <- function(dir=".") {
  jfiles <- list.files(path = dir, pattern = "json$", full.names=T)
  jread <- function(jfile) {
    doc <- fromJSON(file=jfile, method='C')
    out <- list()  
    out[[doc$incident_id]] <- doc
  }
  lapply(jfiles, jread)
}

dir <- "~/Documents/github/VCDB/incidents"
dir <- "~/Documents/json/newfinal/uscert"
vcdb <- readjson(dir)

