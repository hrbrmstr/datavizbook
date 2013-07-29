#
# ip2long(ip)
#
# take an IP address string in dotted octets (e.g. "192.168.0.1")
# and convert it to a 32-bit long integer (e.g. 3232235521)
#

library(bitops)

ip2long <- function(ip) {
  # separate out the octets into a list
  octets = sapply(strsplit(ip,".",fixed=TRUE),function(o) {
    as.integer(o)
  })
  
  # do some bitwise operations to combine each octet's decimal
  # value into a
  bitOr(bitOr(bitOr(bitShiftL(octets[1],24),bitShiftL(octets[2],16)),
              bitShiftL(octets[3],8)),octets[4])
}

#
# long2ip(int)
#
# take an 32-bit integer IP address (e.g. 3232235521)
# and convert it to a (e.g. "192.168.0.1")
#

long2ip <- function(longip) {
  return(sprintf("%d.%d.%d.%d", 
          bitShiftR(bitAnd(longip,bitShiftL(255,24)),24),
          bitShiftR(bitAnd(longip,bitShiftL(255,16)),16),
          bitShiftR(bitAnd(longip,bitShiftL(255,8)),8),
          bitAnd(longip,255)
  ))
}

long2ip(ip2long("192.168.0.0"))
long2ip(ip2long("192.168.0.2"))
long2ip(ip2long("192.168.0.4"))
long2ip(ip2long("192.168.100.6"))
long2ip(ip2long("192.168.33.64"))
long2ip(ip2long("192.168.33.68"))
long2ip(ip2long("10.0.1.99"))
long2ip(ip2long("24.62.253.107"))
