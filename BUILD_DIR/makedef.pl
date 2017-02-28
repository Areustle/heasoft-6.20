$def = $ARGV[0];
shift @ARGV;
$lib = $ARGV[0];
shift @ARGV;
$srcdir = $ARGV[0];
shift @ARGV;
$dumpbin = "dumpbin /symbols @ARGV";

chdir($srcdir);

open(DUMP, "$dumpbin |")
    || die "Can't run `$dumpbin': $!.\n";

open(DEF, "> $def")
    || die "Can't open `$def': $!.\n";

print DEF "LIBRARY $lib\n";
print DEF "EXPORTS\n";

while(<DUMP>)
{
#    if(!/\bUNDEF\b/ && /\bExternal\b/)
    if(/\bExternal\b/)
    {
	s/^.*\|\s+//;
	split;
	$_ = $_[0];


	if(!/^\?\?_G/ && !/^\?\?_E/  && !/^\?\?_C/ && !/^__/ && !/DllMain/ && !/\@/)
        {
            # Stupid windows linker needs to have
            # preceding underscore for ANSI C programs
            s/^_//;

            
            print DEF "    $_\n";
	}
    }
}
