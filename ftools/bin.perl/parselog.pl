#!/usr/local/bin/perl 
# This pushes the directory where the executible is stored into the 
# search path for the require statement.
# On some systems, $0 contains the full path, 
if ( $0 =~ /\// ) {
    $0 =~ m#^(.*)/([^/]+)$#;
    $execdir = $1;
}
# Otherwise, use which to find it...
else {
    ($execdir = `which $0`) =~ m#^(.*)/([^/]+)$#;
    $execdir = $1;
}

push(@INC,$execdir);

require "utils.pl";
require "interface.pl";

$LOGFILE = "";
while ( $ARGV[0] =~ / *-(.*)/ ) {
    if ( $1 =~ /^l(.*)/ ) {
	$LOGFILE = $1;
	die "Can't find log file\n" if ( ! -e $LOGFILE );
    }
    shift;
}
if ( $LOGFILE eq "" ) {
    $LOGFILE = '/ftools/heasarc.log';
}

@columns = ("task","user","system","date","command");

($primary) = &getOneFromList('l',"Choose a primary column:\n",
			     $#columns+1,@columns); 

for ($iprimary=0;$iprimary<=$#columns;$iprimary++) {
    last if ( $primary eq $columns[$iprimary]);
}

print "Enter reg. expr for primary column: ";
$primexpr = <STDIN>;
chop $primexpr;

$cross = &yesOrNo("Do you want to cross correlate this ",'y');

if ( $cross ) {
    @logselect = ();
    print "\n";
    print "Enter regular expressions for selecting from log file.\n";
    print "   Hit a return to do no filtering on the column.\n";
    for ( $i = 0; $i <= $#columns; $i++ ) {
	if ( $i == $iprimary ) {
	    $logselect[$i] = $primexpr;
 	}			
	else {
	    printf "%20s: ",$columns[$i];
	    $logselect[$i] = <STDIN>;
	    chop $logselect[$i];
	    if ( $logselect[$i] =~ /^ *$/ ) {
		$logselect[$i] = '.*';
                $logmode[$i] = 0;
	    }
	    elsif ( $logselect[$i] =~ /^ *@(.*)/ ) {
		$INDIRECT = $1;
		$logselect[$i] = "";
		print "Looking in file $INDIRECT.\n";
		open(INDIRECT) || die "Could not open $INDIRECT\n";
		while ($line = <INDIRECT>) {
		    chop $line;
		    $logselect[$i] .= "$line"."|";
		}
		$logselect[$i] =~ s/\|[ ]*$//;
		print "Got $logselect[$i]\n";
		close(INDIRECT);
	    }
	}
    }
    print "\n";
    for ( $i =0; $i <= $#columns; $i++ ) {
	if ( $logselect[$i] =~ /^ *!(.*)/ ) {
	    $logmode[$i] = 1;
	    $logselect[$i] = $1;
	}
	elsif ( $logselect[$i] =~ /^( *)\\(!.*)/ ) {
	    $logmode[$i] = 0;
	    $logselect[$i] = $1.$2;
	}
	else {
	    $logmode[$i] = 0;
	}
    }
}
else {
    for ( $i = 0; $i <= $#columns; $i++ ) {
	if ( $i == $iprimary ) {
	    $logselect[$i] = $primexpr;
	    if ( $primexpr =~ /^ *!(.*)/ ) {
		$logmode[$i] = 1;
		$logselect[$i] = $1;
	    }
	    else {
		$logmode[$i] = 0;
	    }
	}
	else {
	    $logselect[$i] = '.*';
	    
	    $logmode[$i] = 0;
	}
    }
}

print "Enter output file name (STDOUT for screen): ";
$OUTFILE = <STDIN>;
chop $OUTFILE;
if ( $OUTFILE =~ /^ *$/ ) {
    $OUTFILE = 'STDOUT';
}
elsif ( $OUTFILE ne 'STDOUT' ) {
    if ( $OUTFILE =~ /^ *!(.*)$/ ) {
	$OUTFILE = $1;
	if ( -e $OUTFILE ) {
	    unlink $OUTFILE;
	    print " * $OUTFILE removed.\n\n";
	}
    }
    elsif (-e $OUTFILE ) {
	print "  Output file $OUTFILE exists, overwrite it (Y/n)? ";
	$answer = &yes_or_no('y');
	if ( $answer ) {		# 
	    unlink $OUTFILE;
	    print "   $OUTFILE removed.\n";
	}
	else {
	    print "Be seeing you\n";
	    exit;
	}
    }

    open(OUTFILE,">$OUTFILE")|| die "Could not open output file\n";
}

open(LOGFILE)||die "Could not open the log file\n";


while ( $line = <LOGFILE> ) {
    chop $line;
    @line = split('\|',$line);
#    if ( $#line != $#columns ) {
#	print "Ill formed line: $line\n";
#    }
    if( $primary eq 'task' ) {
	$position = rindex($line[0],'/');
	if ( $position > 0 ) {
	    $line[0] = substr($line[0],$position+1);
	}
    }
    $match = 0;
    for ( $i = 0; $i <= $#logselect; $i++) {
	if ( $logmode[$i] ) {
	    $match = 1, last if $line[$i] =~ m%$logselect[$i]%;
	}
	else{
	    $match = 1, last if $line[$i] !~ m%$logselect[$i]%;
	}
    }
    if( $match == 0 ) {
	$outarray{$line[$iprimary]}++;
    }
}
close(LOGFILE);				
if( $OUTFILE ne 'STDOUT' ) {
    select(OUTFILE);
}
print "Column Name     Selection Expression\n",
      "------------------------------------\n";
for ( $i = 0;$i<=$#columns;$i++) {
    if ( $logmode[$i] ) {
	printf "  %8s       !%s\n",$columns[$i],$logselect[$i];
    }
    else {
	printf "  %8s       %s\n",$columns[$i],$logselect[$i];
    }
}
print "\n\n";
@keyvals = keys(%outarray);
@keyvals = sort(@keyvals);
	
$total = 0;
printf "%20s   # of occurances\n",$primary;
print "        --------------------------------\n";
for ( $i=0;$i<=$#keyvals;$i++) {
    printf "%20s   %d\n",$keyvals[$i],$outarray{$keyvals[$i]};
    $total += $outarray{$keyvals[$i]};
}
printf "\n-----------------------------------------\nTotal:         %d      %d\n\n"
    ,$#keyvals,$total;
if( $OUTFILE ne 'STDOUT' ) {
    close(OUTFILE);
}

