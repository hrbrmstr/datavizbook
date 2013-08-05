#
# ip2long(ip)
#
# take an IP address string in dotted octets (e.g. "192.168.0.1")
# and convert it to a 32-bit long integer (e.g. 3232235521)
#

library(bitops)

ip2long <- function(ip) {
  # Reduce applys a funcution cumulatively on the arguments
  # from left to right. bit-shift, then "OR" the octets
  Reduce(function(x, y) { 
    bitOr(bitShiftL(x,8),y)
  }, sapply(strsplit(ip,".",fixed=TRUE),function(o) {
    as.integer(o)
  }))
}

#
# long2ip(int)
#
# take an 32-bit integer IP address (e.g. 3232235521)
# and convert it to a (e.g. "192.168.0.1")
#
long2ip <- function(longip) {
  # paste converts arguments to character andconcatenates them
  # Map applys a function to each element of the arguent
  paste(Map(function(nbits) { 
    bitAnd(bitShiftR(longip,nbits),0xFF)
  }, c(24,16,8,0)), sep="",collapse=".")
}

#
# ip.is.in.cidr(ip,cidr)
#
# take an IP address (string) and a CIDR (string) and
# return whether the given IP address is in the CIDR range
#
ip.is.in.cidr <- function(ip,cidr) {
  long.ip = ip2long(ip)
  cidr.parts = unlist(strsplit(cidr,"/"))
  cidr.range = ip2long(cidr.parts[1])
  cidr.mask = bitShiftL(bitFlip(0),(32-as.integer(cidr.parts[2])))
  return(bitAnd(long.ip,cidr.mask) == bitAnd(cidr.range,cidr.mask))
}


is.ip <- function(ip) {
  return(grepl("^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$",ip))
}

ip.test <- function() {
  long2ip(ip2long("192.168.0.0"))
  long2ip(ip2long("192.168.0.2"))
  long2ip(ip2long("192.168.0.4"))
  long2ip(ip2long("192.168.100.6"))
  long2ip(ip2long("192.168.33.64"))
  long2ip(ip2long("192.168.33.68"))
  long2ip(ip2long("10.0.1.99"))
  long2ip(ip2long("24.62.253.107"))
  
  ip.is.in.cidr("10.0.1.15","192.0.1.0/24")
  ip.is.in.cidr("10.0.1.15","10.0.1.255/24")
  ip.is.in.cidr("192.168.33.72","192.168.33.64/26")
  ip.is.in.cidr("192.168.33.127","192.168.33.64/26")
  ip.is.in.cidr("192.168.33.128","192.168.33.64/26")
}