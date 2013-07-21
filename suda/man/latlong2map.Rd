\name{latlong2map}
\alias{latlong2map}
\title{Convert latitude/longitude pairs to areas from a map}
\usage{
  latlong2map(pointsDF, mapping)
}
\arguments{
  \item{pointsDF}{a data frame with long, lat in the
  columns}

  \item{mapping}{the map to load ("world", "states",
  "county")}
}
\description{
  Given a data frame of lat and long pairs and a map type
  to load, this will convert the location to the region of
  the map. code is adapted from a gist on github
  https://gist.github.com/rweald/4720788
}
\examples{
geo <- data.frame(long=runif(10, min=-120, max=-80), lat=runif(10, min=-45, max=-32))
  states <- latlong2map(geo, "state")
}
\author{
  Jay Jacobs
}
\keyword{map}

