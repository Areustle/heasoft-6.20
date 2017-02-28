# $Source: /headas/headas/attitude/tasks/tristarid/StarID/HierTriMesh.pm,v $
# $Revision: 1.2 $
# $Date: 2005/10/31 13:56:10 $
#
# $Log: HierTriMesh.pm,v $
# Revision 1.2  2005/10/31 13:56:10  rwiegand
# Renamed the catalog partition paramater to catspec.  This removes the
# need for a dummy directory when the actual data is not even local.
#
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#
# Revision 1.2  2005/05/18 21:17:10  rwiegand
# Avoid a message on stderr.
#
# Revision 1.1  2004/06/17 21:23:28  rwiegand
# The code for loading a partitioned star catalog was modularized from the
# uvotstarid since several tasks need it.  The Partition and SquareRADec
# modules were relocated here with some refactoring and a new Partition
# subclass was added for dealing with the hierarchical triangular mesh
# partitioning used by the Guide Star Catalog folks.
#

package StarID::HierTriMesh;
use base qw(StarID::Partition);

use FileHandle;

use Math;


sub getPlate
{
	my ($self, $spec) = @_;

	my $task = $self->{task};

	my $domain = $task->temporary('domain');
	my $fh = FileHandle->new($domain, 'w');
	if (not $fh) {
		$task->error(BAD_OUTPUT, "unable to create $domain [$!]");
		return;
	}
	$fh->print("#DOMAIN\n");
	$fh->print("1\n");
	$fh->print("#CONVEX_RADEC\n");
	$fh->print("1\n");
	# why in the world?!
	my $raDeg = Math::toDegrees($spec->ra);
	my $decDeg = Math::toDegrees($spec->dec);
	my $radiusDeg = Math::toDegrees($spec->{radius});
	my $cosrad = cos($spec->{radius});
	$fh->print("$raDeg $decDeg $cosrad\n");
	undef($fh);

	my $result = $task->shell("htmintersect -symbolic -expand 6 $domain 2>&1");

	my $root = $self->{directory} || '.';
	my @fits;

	foreach my $line (@{ $result->{lines} }) {
		if ($line =~ /^[NS]\d+$/) {
			my @twos = grep { length } split(/(..)/, $line);
			pop(@twos);
			my $dir = join('/', $root, @twos);
			my $path = "$dir/$line.FIT";
			if (not -f $path) {
				$task->error(BAD_INPUT, "$path is not a file");
			}
			else {
				push(@fits, "$path+2");
			}
		}
		elsif ($line =~ /^Depth =/) {
		}
		else {
			$task->warning("ignoring $line");
		}
	}

	if (not $self->{years2000}) {
		my @t = gmtime;
		$self->{years2000} = (1900 + $t[5]) + $t[7] / 365;
	}

	my $plate = undef;

	if ($task->isValid) {
		my $catalog = $task->temporary('catalog');

		$task->shell("listgsc"
				. " -ra=$raDeg"
				. " -dec=$decDeg"
				. " -radius=$radiusDeg"
				. " -epoch=$self->{years2000}"
				. " @fits >> $catalog",
				);

		$plate = StarID::Partition::Plate->new(
				id => 'dynamic',
				bounds => $spec,
				path => $catalog,
				);
	}

	return $plate;
}


sub platePath
{
	my ($self, $plate) = @_;
	return $plate->{path};
}




1;

