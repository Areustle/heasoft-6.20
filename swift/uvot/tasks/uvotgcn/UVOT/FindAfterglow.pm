# $Source: /headas/headas/swift/uvot/tasks/uvotgcn/UVOT/FindAfterglow.pm,v $
# $Revision: 1.2 $
# $Date: 2006/01/25 21:53:05 $
#
# mod 2005-10-06 to print more info about nearby bright stars
# mod 2005-11-09 to write results to archive file
#
# $Log: FindAfterglow.pm,v $
# Revision 1.2  2006/01/25 21:53:05  rwiegand
# Use StarID::Catalog radius filtering.
#
# Revision 1.1  2005/12/16 16:35:54  rwiegand
# Made library of Frank Marshall's afterglow candidate codes.
#
# Revision 1.2  2005/12/16 14:48:38  wiegand
# Implemented composeParagraph.  Reworked direct match to use StarID::Catalog.
#
# Revision 1.1  2005/12/07 21:33:43  wiegand
# Initial revision
#

use strict;

package UVOT::FindAfterglow;


use File::Basename;
use Text::Wrap;

use Task qw(:codes);
use Math;
use SimpleFITS;
use StarID::Source;
use StarID::CatalogLoader;
use UVOT::GCN;


use constant UNKNOWN_VALUE => '???';


my %DEFAULT = (
	UVOT_CATALOG_FILE => undef,

	CATSPEC => 'USNOB1=/ssdc/usnob1/usnob1.spec',

	# reference catalog search radius for each packet type
	SEARCH_SRCLIST_arcmin => 5.7,
	SEARCH_IMAGE_arcmin => 2.0,

	MAX_XRT_mag => 18.0,		# candidate constraints for XRT position
	MIN_XRT_mag => 9.0,
	MAX_XRT_arcsec => 8.0,

	MAX_BAT_mag => 17.0,		# candidate constraints for BAT position
	MIN_BAT_mag => 9.0,
	MAX_BAT_arcsec => 250.0,

	PACKET_FILTER => '',

	REF_BRIGHT_mag => 13,		# list all reference catalog entries brighter than this
	REF_CLOSE_arcsec => 5.0,	# list all reference catalog entries closer than this

	REF_MATCH_arcsec => 2.0,	# reference objects within this radius are considered a match
	REF_SEARCH_arcmin => 0.5,	# reference catalog search radius

	BRIGHT_SOURCE_mag => 12.0,

	USE_ENTRY => -1,

	PAIR_REPORT_arcsec => 5.0,	# candidate pairs closer than this are reported
	PAIR_MERGE_arcsec => 2.0,	# candidate pairs closer than this are merged
);



sub execute
{
	my ($task, %args) = @_;

	my $info = \%args;

	initializeSearch($task, $info);

	loadUvotCatalog($task, $info)
		if $task->isValid;

	loadReferenceCatalogs($task, $info)
		if $task->isValid;

	matchReferenceToDetections($task, $info)
		if $task->isValid;

	collectBrightSources($task, $info)
		if $task->isValid;

	determineCandidates($task, $info)
		if $task->isValid;

	matchCandidatesToReferences($task, $info)
		if $task->isValid;

	counterCandidates($task, $info)
		if $task->isValid;

	mergeCandidatePairs($task, $info)
		if $task->isValid;

	reportResults($task, $info);

	collectOutputStatements($task, $info);

	saveResults($task, $info);

	return $info;
}



sub initializeSearch
{
	my ($task, $info) = @_;

	$task->report('initializing search');

	while (my ($key, $value) = each(%DEFAULT)) {

		if (ref($value) eq 'HASH') {
			if (exists($value->{init})) {
				$info->{$key} = $value->{init};
			}
		}
		elsif (not exists($info->{$key})) {
			if (defined($value)) {
				$info->{$key} = $value;
			}
			else {
				$task->error(BAD_INPUT,
						"initializeSearch: missing required arg $key");
			}
		}
		else {
			# user provided value
		}
	}

	if (not -f $info->{UVOT_CATALOG_FILE}) {
		$task->error(BAD_INPUT, "invalid file '$info->{UVOT_CATALOG_FILE}'");
	}

	$info->{REF_SEARCH_arcsec} = 60 * $info->{REF_SEARCH_arcmin};
}



sub loadUvotCatalog
{
	my ($task, $info) = @_;

	$task->report('loading UVOT catalog');

	my @table;

	my $header;
	my $status = SimpleFITS->readonly($info->{UVOT_CATALOG_FILE})
			->move('SOURCES')
			->readheader($header, clean => 1)
			->loadtable(\@table)
			->close
			->status;
	if ($status) {
		$task->error(BAD_INPUT, "unable to load catalog file [$status]");
		return;
	}

	my $i = 0;
	foreach my $entry (@table) {
		$entry->{ROW} = ++$i;
		foreach my $key (qw(RA DEC)) {
			my $degkey = $key . '_deg';
			$entry->{$degkey} = $entry->{$key};
			$entry->{$key} = Math::toRadians($entry->{$degkey});
		}
		$entry->{UNIT} = Math::rd2unit($entry->{RA}, $entry->{DEC});
		bless($entry, 'StarID::Source');
	}

	$task->report("loaded $i rows");

	$info->{UVOT_LIST} = \@table;

	if (my $message = $header->{MESGNAME}) {
		if ($message =~ /GENI/) {
			$info->{PACKET_TYPE} = 'IMAGE';
		}
		elsif ($message =~ /FINDINGCHART/) {
			$info->{PACKET_TYPE} = 'SRCLIST';
		}
		else {
			$task->error(BAD_INPUT,
					"unexpected MESGNAME '$header->{MESGNAME}'");
		}
	}
	else {
		$task->error(BAD_INPUT, 'unable to determine packet type');
	}

	$info->{header} = $header;
	foreach my $key (qw(TSTART TSTOP TARG_ID DATE-OBS RA_PNT DEC_PNT)) {
		if (not defined($header->{$key})) {
			$task->error(BAD_INPUT, "missing required key $key");
		}
		else {
			$info->{$key} = $header->{$key};
		}
	}

	$info->{TMIDDLE} = ($info->{TSTART} + $info->{TSTOP}) / 2;
	$info->{TDURATION} = $info->{TSTOP} - $info->{TSTART};

	$task->report("packet type $info->{PACKET_TYPE}");
	$task->report("target ID $info->{TARG_ID}");
	$task->report("DATE-OBS $info->{'DATE-OBS'}");
	$task->report(sprintf('TSTART %.1f', $info->{TSTART}));
	$task->report(sprintf('TSTOP  %.1f', $info->{TSTOP}));
	$task->report(sprintf('target RA, DEC: %.5f, %.5f',
				$info->{RA_PNT}, $info->{DEC_PNT}));

	if (not $header->{ASPDELTA}) {
		$task->warning("did not find keyword ASPDELTA -- aspect solution failed");
	}
	else {
		$info->{ASPDELTA} = $header->{ASPDELTA};
		$task->report("Found aspect solution ASPDELTA = $info->{ASPDELTA} [arcsec]");
	}

	my $rows = scalar(@{ $info->{UVOT_LIST} });
	if ($rows == 0) {
		$task->warning('no rows in UVOT catalog table');
	}
	else {
		my %hasColumn;

		my $testObj = $info->{UVOT_LIST}[0];
		foreach my $colname (qw(RA DEC RA_deg DEC_deg MAG REFID STARID BAT_DELTA)) {
			if (not exists($testObj->{$colname})) {
				$task->error(BAD_INPUT, "catalog objects missing $colname");
			}
			else {
				$hasColumn{$colname} = 1;
			}
		}

		foreach my $colname (qw(PACKET TYPE CAT_DELTA MAG_DELTA XRT_DELTA)) {
			if (not exists($testObj->{$colname})) {
				$task->warning("catalog objects missing $colname");
			}
			else {
				$hasColumn{$colname} = 1;
			}
		}

		my $which = $hasColumn{XRT_DELTA} ? 'XRT' : 'BAT';
		foreach my $inst (qw(XRT BAT)) {
			$info->{"has$inst"} = $hasColumn{$inst . '_DELTA'};
		}
		$info->{useINST} = $which;

		$info->{MIN_CANDIDATE_mag} = $info->{"MIN_${which}_mag"};
		$info->{MAX_CANDIDATE_mag} = $info->{"MAX_${which}_mag"};
		$info->{MAX_CANDIDATE_arcsec} = $info->{"MAX_${which}_arcsec"};
	}
}



sub loadReferenceCatalogs
{
	my ($task, $info) = @_;

	my @spec = split(',', $info->{CATSPEC});
	my $plural = @spec > 1 ? 's' : '';
	$task->report("loading reference catalog$plural");

	my $bounds = StarID::Source->new(
			RA => $info->{RA_PNT},
			DEC => $info->{DEC_PNT},
			radius => $info->{"SEARCH_$info->{PACKET_TYPE}_arcmin"} / 60,
			degrees => 1,
			);

	my @tag;
	foreach my $spec (@spec) {
		my ($tag, $path);
		if ($spec =~ /(\w+)=(.+)/) {
			$tag = $1;
			$path = $2;
		}
		else {
			my ($name) = File::Basename::fileparse($spec, '.spec');
			$tag = $name;
			$path = $spec;
		}

		push(@tag, $tag);

		my $loader = StarID::CatalogLoader->new(
				task => $task,
				bounds => $bounds,
				catspec => $path,
				);

		$loader->execute;

		if (not $loader->isValid) {
			$task->warning("unable to load $spec");
			next;
		}

		my $catalog = $loader->getCatalog;

		$catalog->apply(sub {
					my ($s) = @_;
					foreach my $key (qw(RA DEC)) {
						my $degkey = $key . '_deg';
						$s->{$degkey} = Math::toDegrees($s->{$key});
					}
				});

		my $count = $catalog->size;
		$task->report("loaded $count objects using $spec");

		$info->{'REF_CATALOG_' . $tag} = $catalog;
		if (not $info->{REF_CATALOG}) {
			$info->{REF_CATALOG} = $catalog;
		}
	}

	$info->{TAG_CATSPEC} = join(',', @tag);
}


sub matchReferenceToDetections
{
	my ($task, $info) = @_;

	$task->report('matching references to detections');

	my $refCatalog = $info->{REF_CATALOG};
	my $matchrad = Math::toRadians($info->{REF_MATCH_arcsec} / 3600);

	# by iterating over the detections first, we can use an indexed search on
	# the presumably much larger reference catalog

	foreach my $uvotEntry (@{ $info->{UVOT_LIST} }) {

		# skip UVOT source table entries that are not detections
		next if $uvotEntry->{REFID} < 0;

		my $sublist = $refCatalog->contents($uvotEntry, $matchrad);

		foreach my $refObject (@$sublist) {

			$refObject->{MATCHED_DETECTION} = $uvotEntry;

			$task->report("matched reference object $refObject->{ID}"
					. " with detected object $uvotEntry->{REFID}")
				if $task->chatter(4);
		}
	}

	my $refTotal = $refCatalog->size;
	my $refMatches = 0;
	$refCatalog->apply(sub {
				my ($o) = @_;
				if ($o->{MATCHED_DETECTION}) {
					++$refMatches;
				}
			});

	$task->report("matched $refMatches of $refTotal reference objects to detections");
}


sub collectBrightSources
{
	my ($task, $info) = @_;

	$task->report('finding bright sources');

	my @brightSources;
	$info->{BRIGHTEST_mag} = 999;

	my $refCatalog = $info->{REF_CATALOG};
	foreach my $refObject (@{ $refCatalog->contents }) {
		my $mag = $refObject->mag;
		if ($mag < $info->{BRIGHT_SOURCE_mag}) {
			if (not @brightSources) {
				$task->report("bright object:  RA      DEC   Mag   radius[arcsec]");
			}
			$refObject->{BRIGHT_arcsec} = 80 - 10 * ($mag - 6);

			push(@brightSources, $refObject);

			$task->report(sprintf('bright %2d: %9.5f %9.5f %5.2f %6.2f',
					scalar(@brightSources),
					$refObject->{RA_deg}, $refObject->{DEC_deg},
					$refObject->{MAG}, $refObject->{BRIGHT_arcsec}));
		}
		if ($mag < $info->{BRIGHTEST_mag}) {
			$info->{BRIGHTEST_mag} = $mag;
		}
	}

	$info->{BRIGHT_LIST} = \@brightSources;
}



sub determineCandidates
{
	my ($task, $info) = @_;

	$task->report('determining candidates');

	$task->report('matching detections to reference objects');
	$task->report("using rows with PACKET matching $info->{PACKET_FILTER}")
		if $info->{PACKET_FILTER};

	my @matched;
	my @candidate;
	my %refid;
	my $bestUnidentified = undef;

	$info->{REFIDS} = \%refid;
	$info->{N_NO_STARID} = 0;

	my $distkey = $info->{useDELTA} = $info->{useINST} . '_DELTA';

	foreach my $uvotEntry (@{ $info->{UVOT_LIST} }) {

		# don't think about entries in UVOT source table that were not detected
		next if $uvotEntry->{REFID} < 0;

		next if $info->{PACKET_FILTER}
				and $uvotEntry->{PACKET} !~ $info->{PACKET_FILTER};

		$refid{$uvotEntry->{REFID}} = 1;

		push(@matched, $uvotEntry);

		testBrightSources($task, $info, $uvotEntry);

		if (not $uvotEntry->{STARID}) {

			++$info->{N_NO_STARID};

			if (not $bestUnidentified) {
				$bestUnidentified = $uvotEntry;
			}
			elsif ($uvotEntry->{$distkey} < $bestUnidentified->{$distkey}) {
				$bestUnidentified = $uvotEntry;
			}

			my $mag = $uvotEntry->{MAG};
			if ($mag > $info->{MIN_CANDIDATE_mag}
					and $mag < $info->{MAX_CANDIDATE_mag}
					and $uvotEntry->{$distkey} < $info->{MAX_CANDIDATE_arcsec}
					and not $uvotEntry->{NEARBY_BRIGHT_SOURCE}) {
				push(@candidate, $uvotEntry);
			}
		}
	}

	$info->{BEST_UNIDENTIFIED} = $bestUnidentified;
	$info->{MATCHED_LIST} = \@matched;
	$info->{CANDIDATE_LIST} = \@candidate;

	# look at catalog objects which were not detected
	my @uncat;
	$info->{BRIGHTEST_UNCAT} = 999;
	foreach my $uvotEntry (@{ $info->{UVOT_LIST} }) {

		# operate on entries not detected by UVOT
		next if $uvotEntry->{REFID} > 0;

		# note that TYPE is not available for USNO B1
		# if ($uvotEntry->{TYPE} and $uvotEntry->{TYPE} eq 'star') {
			push(@uncat, $uvotEntry);
			if ($uvotEntry->{MAG} < $info->{BRIGHTEST_UNCAT}) {
				$info->{BRIGHTEST_UNCAT} = $uvotEntry->{MAG};
			}
		# }
	}

	$info->{UNMATCH_LIST} = \@uncat;

	# examine MAG_DELTA and CAT_DELTA
	my ($cat_mean, $cat_std) = std_dev([ map { $_->{CAT_DELTA} } @matched ]);
	my ($mag_mean, $mag_std) = std_dev([ map { $_->{MAG_DELTA} } @matched ]);
	$info->{CAT_DELTA_MEAN} = $cat_mean;
	$info->{CAT_DELTA_STD} = $cat_std;
	$info->{MAG_DELTA_MEAN} = $mag_mean;
	$info->{MAG_DELTA_STD} = $mag_std;

}


sub matchCandidatesToReferences
{
	my ($task, $info) = @_;

	$task->report('matching candidates to reference objects');

	my $refCatalog = $info->{REF_CATALOG};
	my $matchrad = Math::toRadians($info->{REF_SEARCH_arcsec} / 3600);

	foreach my $candidate (@{ $info->{CANDIDATE_LIST} }) {

		$candidate->{SEARCH_LIST} = $refCatalog->contents($candidate, $matchrad);
	}
}



sub counterCandidates
{
	my ($task, $info) = @_;

	$task->report('finding problems with candidates');

	foreach my $candidate (@{ $info->{CANDIDATE_LIST} }) {

		my @found;

		foreach my $refObject (@{ $candidate->{SEARCH_LIST} }) {

			my $separcsec = separation_arcsec($candidate, $refObject);

			if (not $candidate->{REF_CLOSEST} or
					$separcsec < $candidate->{REF_CLOSEST_arcsec}) {
				$candidate->{REF_CLOSEST} = $refObject;
				$candidate->{REF_CLOSEST_arcsec} = $separcsec;
			}

			if ($separcsec < $info->{REF_CLOSE_arcsec}
					or $refObject->{MAG} < $info->{REF_BRIGHT_mag}) {
				push(@found, $refObject);
			}
		}

		$candidate->{COUNTER_LIST} = \@found;
	}
}


sub formatCandidate
{
	my ($source) = @_;
	my $str = sprintf('candidate %s ra=%.4f, dec=%.4f, mag=%.1f',
			$source->{REFID}, $source->{RA_deg}, $source->{DEC_deg}, $source->{MAG});
	return $str;
}

sub reportCloseCandidates
{
	my ($task, $info) = @_;

	$task->report('reporting close candidates');

	foreach my $alpha (@{ $info->{UNMERGED_LIST} }) {

		foreach my $beta (@{ $info->{UNMERGED_LIST} }) {

			last if $alpha == $beta;

			my $separcsec = separation_arcsec($alpha, $beta);

			if ($separcsec < $info->{PAIR_REPORT_arcsec}) {
				$task->report('pair: %s and %s separation %.1"',
						formatCandidate($alpha), formatCandidate($beta),
						$separcsec);
			}
		}
	}
}


sub mergeCandidatePairs
{
	my ($task, $info) = @_;

	$task->report('merging close candidates');

	foreach my $alpha (@{ $info->{CANDIDATE_LIST} }) {

		foreach my $beta (@{ $info->{CANDIDATE_LIST} }) {

			last if $alpha == $beta;

			my $separcsec = separation_arcsec($alpha, $beta);

			if ($separcsec < $info->{PAIR_MERGE_arcsec}) {
				my $dominant = $beta->{MAG} < $alpha->{MAG} ? $beta : $alpha;
				my $subject = $dominant == $alpha ? $beta : $alpha;
				$subject->{DOMINATOR} = $dominant;
				$task->report('merge: %s dominates %s',
						formatCandidate($dominant), formatCandidate($subject));
			}
		}
	}

	my @merged = grep { not $_->{DOMINATOR} } @{ $info->{CANDIDATE_LIST} };
	$info->{UNMERGED_LIST} = $info->{CANDIDATES};
	$info->{CANDIDATE_LIST} = \@merged;
}



sub testBrightSources
{
	my ($task, $info, $uvotEntry) = @_;

	foreach my $brightSource (@{ $info->{BRIGHT_LIST} }) {

		my $separcsec = separation_arcsec($uvotEntry, $brightSource);

		if ($separcsec < $brightSource->{BRIGHT_arcsec}) {

			$uvotEntry->{NEARBY_BRIGHT_SOURCE} = $brightSource;
			$uvotEntry->{BRIGHT_INFO} = sprintf('is %4.1f from bright star %s',
					$separcsec, $brightSource->id);
			last;
		}
	}
}



sub reportResults
{
	my ($task, $info) = @_;

	reportMatchedCatalog($task, $info)
		if $task->isValid;

	reportUnmatchedCatalog($task, $info)
		if $task->isValid;

	reportCloseCandidates($task, $info)
		if $task->isValid;
}


sub reportMatchedCatalog
{
	my ($task, $info) = @_;

	if ($task->chatter(1)) {
		my $str = "      No. ID REFID   RA      DEC     mag d_bat";
		if ($info->{hasXRT}) {
			$str .= " d_xrt";
		}
		$task->report($str);
	}

	if ($task->chatter(3)) {

		foreach my $uvotEntry (@{ $info->{MATCHED_LIST} || [ ] }) {
			my $str = sprintf('entry %3d  %1s %3d %8.4f %8.4f %5.2f %5.1f',
					$uvotEntry->{ROW},
					$uvotEntry->{STARID} ? 'Y' : 'N',
					$uvotEntry->{REFID},
					$uvotEntry->{RA_deg},
					$uvotEntry->{DEC_deg},
					$uvotEntry->{MAG},
					$uvotEntry->{BAT_DELTA});
			if (my $xrtDelta = $uvotEntry->{XRT_DELTA}) {
				$str .= sprintf(' %5.1f', $xrtDelta);
			}
			if (my $info = $uvotEntry->{BRIGHT_INFO}) {
				$str .= ' ' . $info;
			}
			$task->report($str);
		}
	}
}


sub reportUnmatchedCatalog
{
	my ($task, $info) = @_;

	my $uncat = $info->{UNMATCH_LIST};

	my $nunmatch = @$uncat;
	$task->report("UVOT catalog contains $nunmatch unmatched catalog stars.");
	if ($nunmatch > 0) {
		my ($unmag_mean, $unmag_std) = std_dev([ map { $_->{MAG} } @$uncat ]);
		$task->report(sprintf(' Brightest such star is mag %.2f, mean mag is %.2f, std dev is %.2f',
				$info->{BRIGHTEST_UNCAT}, $unmag_mean, $unmag_std));
	}

	my $percent = 100;
	my $nunique = $info->{N_UNIQUE} = scalar(keys(%{ $info->{REFIDS} }));
	if ($nunique > 0) {
		$percent = int(100 * $info->{N_NO_STARID} / $nunique);
	}
	$info->{PERCENT_NO_STARID} = $percent;
}



sub collectOutputStatements
{
	my ($task, $info) = @_;

	my @statements;

	my ($base) = File::Basename::fileparse($info->{UVOT_CATALOG_FILE});

	push(@statements, "$info->{TARG_ID} UVOT afterglow report for $base");
	push(@statements, "Observation start time is $info->{'DATE-OBS'}");
	push(@statements, "Examining file $info->{UVOT_CATALOG_FILE} with $info->{N_UNIQUE} unique entries");

	if ($info->{USE_ENTRY} >= 0) {
		if ($info->{USE_ENTRY} < @{ $info->{UVOT_LIST} }) {
			my $useEntry = $info->{UVOT_LIST}[$info->{USE_ENTRY}];
			$info->{CANDIDATE_LIST} = [ $useEntry ];
			push(@statements, "*** WARNING -- matching candidate set to be entry $info->{USE_ENTRY}");
		}
		else {
			$task->error(BAD_INPUT, "USE_ENTRY $info->{USE_ENTRY} out of range");
		}
	}

	my $nOk = @{ $info->{CANDIDATE_LIST} || [ ] };

	if (not defined($info->{ASPDELTA})) {
		push(@statements, "*** WARNING -- Aspect solution failed.");
	}

	my $refCatalog = $info->{REF_CATALOG};
	if (not $refCatalog) {
		push(@statements, "*** WARNING -- no catalog loaded.",
                  "  -- cannot check for bright stars or nearby sources.");
	}

	my $brightCount = @{ $info->{BRIGHT_LIST} || [ ] };
	if ($brightCount > 0) {
		my $word = $brightCount == 1 ? 'entry' : 'entries';
		push(@statements, "*** WARNING -- bright star catalog has $brightCount $word brighter than $info->{BRIGHT_SOURCE_mag} mag",
        		"  The brightest has mag $info->{BRIGHTEST_mag}");

	}

	if (not $info->{hasXRT}) {
		push(@statements, "*** WARNING -- no XRT distances are available.");
    }

	push(@statements,
			sprintf('Mean distance difference (CAT_DELTA) for matched entries is %.3f arcsec',
			$info->{CAT_DELTA_MEAN}));
	push(@statements,
			sprintf('Mean and std. dev of mag. difference (MAG_DELTA) are %.3f and %.3f',
			$info->{MAG_DELTA_MEAN}, $info->{MAG_DELTA_STD}));

	push(@statements, "Found $info->{N_NO_STARID} entries without IDs and $nOk entries that are acceptable candidates.");
	push(@statements, "Acceptable entries have no ID, mag between $info->{MIN_CANDIDATE_mag} and $info->{MAX_CANDIDATE_mag}",
			" $info->{useINST} distances < $info->{MAX_CANDIDATE_arcsec}\"",
			" and are > (80 - 10*(mag-6))\" from any bright star");


	my $nGood = 0;

	if ($info->{N_NO_STARID} > 0) {
		if ($info->{PERCENT_NO_STARID} > 25) {
			push(@statements, "*** WARNING $info->{PERCENT_NO_STARID} per cent of entries have no IDs.");
		}
		if ($nOk == 0) {
			my $best = $info->{BEST_UNIDENTIFIED};
			push(@statements, "Found NO acceptable entries.",
					"Unidentified entry closest to $info->{useINST} position is in row $best->{ROW}",
					sprintf('  ra=%.4f dec=%.4f mag=%.1f dist=%.1f REFID=%s',
							$best->{RA_deg}, $best->{DEC_deg}, $best->{MAG},
							$best->{$info->{useDELTA}}, $best->{REFID}),
					);
		}
		else {
			my $first = 1;

			foreach my $candidate (@{ $info->{CANDIDATE_LIST} }) {
				my ($rahms, $decdms) = deg2sex($candidate->{RA_deg}, $candidate->{DEC_deg});
				push(@statements, "UVOT $info->{PACKET_TYPE} catalog row $candidate->{ROW} is a candidate afterglow for $info->{TARG_ID}",
						sprintf(' ra=%6.4f (%s) dec=%6.4f (%s) mag=%4.1f dist=%3.1f REFID=%s',
								$candidate->{RA_deg}, $rahms, $candidate->{DEC_deg}, $decdms,
								$candidate->{MAG}, $candidate->{$info->{useDELTA}}, $candidate->{REFID}),
						$candidate->{PACKET} ? "with $candidate->{PACKET}" : '',
						);
				if (my $nearby = $candidate->{SEARCH_LIST}) {
					my $count = @$nearby;
					push(@statements, "Found $count reference catalog entries within $info->{REF_SEARCH_arcsec}\" of the candidate");

					if ($first) {
						$first = undef;
						push(@statements, "  printing closest, all within $info->{REF_CLOSE_arcsec}\", all with mag < $info->{REF_BRIGHT_mag}",
								"  (Entries within $info->{REF_MATCH_arcsec} arc sec of a srclist entry are considered a match");
					}

					push(@statements, "    RA        Dec    Sep.  MAG   Match");

					my $counterList = $candidate->{COUNTER_LIST};
					my $showClosest = $candidate->{REF_CLOSEST};
					foreach my $entry (@$counterList) {
						if ($entry == $showClosest) {
							$showClosest = undef;
						}
						push(@statements, formatCloseEntry($candidate, $entry) . ' *');
					}
					if ($showClosest) {
						push(@statements, formatCloseEntry($candidate, $showClosest));
					}
					if (@$counterList > 0) {
						push(@statements, "*** Due to reference entries marked with *, this is probably NOT an afterglow");
					}
					else {
						++$nGood;
						push(@statements, "This source is still a GOOD candidate");
					}
				}
			}
		}
	}

	my $plural = $nGood == 1 ? '' : 's';
	push(@statements, "SUMMARY: $nGood good candidate$plural found");

	$task->report(join("\n", 'processing report', @statements));

	$info->{STATEMENTS} = \@statements;
}



sub saveResults
{
	my ($task, $info) = @_;

	# store the important information about candidates
	my @candidates;
	my @basicFields = qw(REFID RA_deg DEC_deg MAG XRT_DELTA BAT_DELTA CAT_DELTA);
	foreach my $candidate (@{ $info->{CANDIDATE_LIST} || [ ] }) {
		my %basic = map { ($_ => $candidate->{$_}) } @basicFields;
		push(@candidates, \%basic);
	}
	my %results = (
		(map { $_ => $info->{$_} } qw(CAT_DELTA_MEAN CAT_DELTA_STD MAG_DELTA_MEAN)),
		CANDIDATES => \@candidates,
	);

	foreach my $tag (split(',', $info->{TAG_CATSPEC})) {
		if (my $catalog = $info->{'REF_CATALOG_' . $tag}) {
			$results{'N_REF_' . $tag} = $catalog->size;
		}
	}

	$info->{FIND_AFTERGLOW} = \%results;
}



sub separation_arcsec
{
	my ($s, $t) = @_;
	my $radians = $s->angle($t);
	my $arcsec = 3600 * Math::toDegrees($radians);
	return $arcsec;
}


sub formatCloseEntry
{
	my ($candidate, $close) = @_;

	my $separcsec = separation_arcsec($candidate, $close);

	my $str = sprintf('  %8.4f %8.4f %5.1f %5.2f %s',
			$close->{RA_deg}, $close->{DEC_deg}, $separcsec,
			$close->{MAG}, $close->{ID});

	return $str;
}



sub std_dev
{   
	my ($aref) = @_;

    # input is:
    #   a pointer to an array of values
    # outputs are:
    #   (mean, std. dev) of values
    #   std. dev < 0 indicates problem (too few entries)

    my $sum  = 0;
    my $sum2 = 0;
    my $n    = 0;

	my ($mean, $sigma);

	foreach my $x (@$aref) {
		if ($x ne 'nan') {
			$sum += $x;
			$sum2 += $x * $x;
			++$n;
		}
	}

	if ($n > 0) {
		$mean = $sum / ($n);    # unweighted mean
		$sigma = sqrt($sum2 / ($n) - $mean * $mean);
    }
    else {
		$mean  = 0;
		$sigma = -1.;
	}

	return ($mean, $sigma);
}



sub deg2sex
{
	my ($ra, $dc) = @_;

	my ($dg, $dmn, $dsec, $hr, $mn);
	my ($sec, $sign, $tmp);

	# start with RA

	if ($ra < 0.) { $ra += 360. }
	$tmp = $ra / 15.;
	$hr  = int($tmp);
	$tmp = ($tmp - $hr) * 60.;
	$mn  = int($tmp);
	$sec = ($tmp - $mn) * 60.;

	# now do dec
	$sign = "+";
	if ($dc < 0.)
	{
		$sign = "-";
		$dc   = -$dc;
	}
	$tmp  = $dc;
	$dg   = int($tmp);
	$tmp  = ($tmp - $dg) * 60.;
	$dmn  = int($tmp);
	$dsec = ($tmp - $dmn) * 60.;

	#  printf "%9.4f%10.4f",$ra,$dc ;
	my $rahms = sprintf("%02dh%02dm%05.2fs", $hr, $mn, $sec);
	my $dcdms = sprintf("%-s%02do%02d\'%04.1f\"", $sign, $dg, $dmn, $dsec);

	return ($rahms, $dcdms);
}



sub composeParagraph
{
	my ($task, %args) = @_;

# produce message and notes

# basic approach for the message is:
# report exposure duration and delay from trigger
# report candidate afterglow if there is one
#   if one in image, report it
# otherwise report one from srclist if there is one
# if no candidates report overlaps and upper limits
#     report only XRT position if there is one
# if there is image data, report overlap and nominal UL
# if not, report lack of data
# if there is srclist data, report overlap and nominal UL
# if not, report lack of data
# finally report extinction

# notes are intended for team. they provide additional
# information and may help explain what is in the message

	my $findInfo = $args{FIND_INFO};
	my $jobInfo = $args{JOB_INFO};
	# my $triggerInfo = $args{TRIGGER_INFO};

	my @paragraph;
	my @problems;
	my @notes;

	my $packet = $jobInfo->{PACKET};

	my $targID = $jobInfo->{TARG_ID} || UNKNOWN_VALUE;

	my $expform = UNKNOWN_VALUE;
	if (my $exposure = $packet->{EXPOSURE}) {
		if ($packet->{EXPGUESS}) {
			$expform = sprintf('nominally %d', $exposure);
		}
		else {
			$expform = sprintf('%4.1f', $exposure);
		}
	}
	else {
		push(@problems, '*** Warning -- exposure duration not available.');
	}

	my $filter = $packet->{FILTER};
	if (not $filter) {
		$filter = UNKNOWN_VALUE;
		push(@problems, '*** Warning -- filter not available.');
	}

	my $trigtime = $packet->{BAT_TRIG_TIME};
	my $tstart = $findInfo->{TSTART};
	my $trigform = UNKNOWN_VALUE;
	if (defined($trigtime)) {
		$trigform = sprintf('%.1f', $trigtime);
	}
	my $delayform = UNKNOWN_VALUE;
	if (defined($trigtime) and defined($tstart)) {
		$delayform = sprintf('%.1f', $tstart - $trigtime);
	}
	else {
		push(@problems, '*** Warning -- delay time not available.');
	}

	push(@paragraph,
			"In response to the Swift/BAT trigger $targID at $trigform,",
			"UVOT took a finding chart exposure of $expform seconds",
			"with the $filter filter",
			"starting $delayform seconds after the BAT trigger.",
			);

	my $instInfo;
	foreach my $tag (qw(XRT BAT)) {
		if (my $notice = $jobInfo->{$tag . '_NOTICE'}) {
			$notice->{INST} = $tag;
			if (not $instInfo) {
				$instInfo = $notice;
			}
			push(@notes, "$tag position found");
		}
		else {
			push(@notes, "No $tag position found");
		}
	}

	my $nCatalogs = 0; # number of reference catalogs from which objects were loaded
	foreach my $tag (split(',', $findInfo->{TAG_CATSPEC} || '')) {
		my $refcat = $findInfo->{"REF_CATALOG_$tag"};
		if (not defined($refcat)) {
			push(@notes, "Loading the $tag star catalog FAILED.");
		}
		else {
			my $loaded = $refcat->size;
			if ($loaded < 1) {
				push(@notes, "There was likely a PROBLEM loading the $tag catalog because no stars were found.");
			}
			else {
				++$nCatalogs;
			}
		}
	}

	my $nGood = scalar(@{ $findInfo->{CANDIDATE_LIST} || [ ] });
	my $type = $findInfo->{PACKET_TYPE};
	push(@notes, "There are $nGood candidate(s) in the UVOT $type catalog.");
	if (not $nCatalogs) {
		push(@problems, 'Failure of star catalog access makes the candidate(s) UNRELIABLE.');
	}
	if ($nGood > 1) {
		push(@problems, 'More than 1 candidate indicates that the candidates are UNRELIABLE.');
	}

	my $source;
	if ($type eq UVOT::GCN::PACKET_IMAGE) {
		$source = "in the rapidly available 2.7'x2.7' sub-image";
	}
	elsif ($type eq UVOT::GCN::PACKET_SRCLIST) {
		$source = 'in the list of sources generated on-board';
	}
	else {
		$source = 'in unknown packet';
	}

	if (not defined($instInfo)) { # should not happen
		push(@notes,  'No instrument position available');
	}
	elsif ($nGood == 1 and $nCatalogs > 0) {
		my $candidate = $findInfo->{CANDIDATE_LIST}[0];

		my $ra = $candidate->{RA_deg};
		my $dec = $candidate->{DEC_deg};
		my $raform = sprintf('%.5f', $ra);
		my $decform = sprintf('%.5f', $dec);
		my ($rahms, $decdms) = deg2sex($ra, $dec);

		my $magform = sprintf('%.1f', $candidate->{MAG});

		my $poserrform;
		if (my $poserr = $findInfo->{CAT_DELTA_MEAN}) {
			if ($poserr < 0.5) { $poserr = 0.5; } # set lower limit
			$poserrform = sprintf('%.1f', $poserr);
		}
		else {
			$poserrform = 1;	# use default value
		}

		my @distform;
		my $distkey = $findInfo->{useDELTA};
		if (my $dist = $candidate->{$distkey}) {
			my $distform = sprintf('%.1f', $dist);
			push(@distform,
					"This position is $distform arcsec from",
						"the center of the $instInfo->{INST} error circle.",
					);
		}

		push(@paragraph,
				'There is a candidate afterglow',
				$source,
				"at (RA,DEC) (J2000) of ($raform, $decform) or ($rahms, $decdms)",
					"with a 1-sigma error radius of about $poserrform arcsec.",
				@distform,
				"The estimated V magnitude is $magform",
					"with a 1-sigma error of about 0.5 mag.",
				);
	}
	else {
		my @trouble;
		if ($nGood > 0) {
			if ($nGood > 1) {
				@trouble = ('Multiple (hence UNRELIABLE) afterglow candidates were found',
						$source);
			}
			else {
				@trouble = ('A problem accessing the star catalog',
						'makes the afterglow candidate', $source, 'UNRELIABLE.');
			}
		}
		else {
			@trouble = ('No afterglow candidate has been found',
					'in the initial data products.');
		}

		push(@paragraph, @trouble);
	}

	if ($nGood > 0) {
		# calculate overlap, report upper limit
	}

	my $uniqueAfterglow = ($nGood == 1 and $nCatalogs > 0);
	if (not $uniqueAfterglow) {
		push(@notes, 'No unique and reliable afterglow was found');
	}

	$findInfo->{GCN_LIST} = \@paragraph;
	$findInfo->{PROBLEM_LIST} = \@problems;
	$findInfo->{NOTE_LIST} = \@notes;

	local($Text::Wrap::columns) = $args{TEXT_COLUMNS} || 72;
	my $paragraph = Text::Wrap::wrap('', '', join(' ', @paragraph));
	my $notes = join("\n", @problems, @notes);

	return ($paragraph, $notes);
}



1;

