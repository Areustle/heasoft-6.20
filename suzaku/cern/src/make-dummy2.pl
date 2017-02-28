#/usr/local/bin/perl

print "C cernlib-dummy2.f from hbook.h, higz.h, hplot.h, minut.h\n";

while (<>) {
	if ( /^PROTOCCALLSFSUB/ ) {
		split "[(,)]";
		$name = $_[1];
		next if ( $name eq "HLIMAP" );
		print <<EOF

      Subroutine $name
      Return
      End
EOF
;
	}
}
