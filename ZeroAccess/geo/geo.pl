# geo.pl
# 
# quick hack to convert lat/lon to zip code for zeroaccess stuff
#
# need an account at geonames to use the api
# start here: http://www.geonames.org/export/web-services.html#findNearbyPostalCodes
#
# there are a bunch of xXX files. I "split" the main file into them
# to adhere to geonames ToS
#
# I also did "sleep(3)" as well to avoid getting banned
# 
# it took a day per file to avoid being banned
#
# they do have a data dump, but that would have meant writing more code #lazy
#

use Geo::GeoNames;

my $geo = new Geo::GeoNames( username => "hrbrmstr") ;

open F, "xaa" ;
open O, ">>latlonzip.csv" ;

$header = <F> ;

while(<F>) {

    chomp $_ ;
    chop $_ ;

    ($country, $latitude, $long) = split(/\,/, $_, 3) ;

    if ($country eq "US") {

        $result = $geo->find_nearby_postalcodes( lat => $latitude, lng => $long, maxRows => 1) ;

        $zip = $result->[0]->{postalcode} ;

        print "$latitude,$long,$zip\n" ;
        print O "$latitude,$long,$zip\n" ;

        sleep(3) ;

    };

}

close(O) ;
close(F) ;
