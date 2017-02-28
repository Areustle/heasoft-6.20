# $Source: /headas/headas/swift/uvot/lib/perl/WebQuery.pm,v $
# $Revision: 1.2 $
# $Date: 2004/10/17 15:55:22 $
#
#	This module is based on the webquery module provided on
#	the SkyView batch page http://skyview/docs/batchpage.html
#
#
# $Log: WebQuery.pm,v $
# Revision 1.2  2004/10/17 15:55:22  rwiegand
# Display response when HTTP POST fails.
#
# Revision 1.1  2003/11/26 20:00:19  rwiegand
# Modules for sharing among UVOT tasks.
#
# Revision 1.1  2003/11/18 14:43:19  wiegand
# Initial revision
#
# Here is the original webquery documentation:
# This program gets the output of a specified URL.  The user
# specifies the host and URL as well as any parameters that might
# be used by the URL.  The output of URL is returned either
# to standard output or to a specified file.  Five special keyword
# parameters may be specified: url, host, method, port and file.
# All other parameters are passed to the URL.
#
# Webquery does no reformatting or error checking.  Typically
# the returned data will be an HTML formatted document.  If an
# error is encountered, the returned data may be an HTML formatted
# error message.
#
# Usage:
# webquery url=URL host=HOST method=METHOD port=PORT file=FILE 
#            [key=VALUE key=VALUE]
#
# The default URL is the null string. The default host is the local
# host.  The method and port keywords may be used
# to override the defaults of POST and 80 respectively.
# The file keyword specifies an output file to which the response is
# to be sent.  If not specified, output is sent to standard output.
#
#
# Additional keywords are appended to the URL request as
# keyword values for a forms request.  Webquery will appropriately
# escape special characters.
#

use strict;

package WebQuery;

use base qw(Task);
use Task qw(:codes);

use FileHandle;
use IO::Socket;


sub _initialize
{
	my ($self) = @_;

	$self->{toolversion} = 'WebQuery 1.0pm';

	$self->{port} ||= 80;
	$self->{method} ||= 'POST';
	$self->{input} ||= [ ];

	foreach my $arg (qw(url host method port file input)) {
		if (not $self->{$arg}) {
			$self->error(BAD_INPUT, "missing required argument '$arg'");
		}
	}
}


sub execute
{
	my ($self) = @_;

	$self->initialize;

	$self->sendQuery
		if $self->isValid;

	$self->saveResults
		if $self->isValid;

	$self->finalize;
}


sub initialize
{
	my ($self) = @_;

	my $out = FileHandle->new($self->{file}, 'w');
	if (not $out) {
		$self->error(BAD_OUTPUT,
			"unable to create output file $self->{file}: $!");
		return;
	}
	else {
		$self->{out} = $out;
	}

	my $s = IO::Socket::INET->new(
		PeerAddr => $self->{host},
		PeerPort => $self->{port},
		);

	if (not $s) {
		$self->error(BAD_EXECUTE,
			"unable to open connection to $self->{host} @ $self->{port}: $!");
	}
	else {
		$s->timeout(30);
		$self->{sock} = $s;
	}
}


sub webcode ($)
{
	my ($string) = @_;

# First convert special characters to to %xx notation.
	$string =~ s/[^ a-zA-Z0-9]/sprintf("%%%02x", ord($&))/eg;

# Now convert the spaces to +'s.
# We do this last since otherwise +'s would be translated by above.
	$string =~ tr/ /+/;

# Perl doesn't require (or even like) putting in the return value,
# but I find it clearer.
	return $string;
}


sub sendQuery
{
	my ($self) = @_;

	my $query = '';

	foreach my $arg (@{ $self->{input} }) {

		if ($query) {
			$query .= '&';
		}

		if ($arg =~ /=/) {

			my ($key, $value) = split('=', $arg, 2);
			$query .= webcode($key);
			$query .= '=';
			$query .= webcode($value);
		}
		else {
			$query .= webcode($arg);
		}
	}

	if ($query) {
# Put in characters to ensure query termination.  There seems to
# be an extra \015, but we seem to need this sometimes.
		$query .= "\015\012\012";
	}

	$self->report("query: $query")
		if $self->chatter(3);

	$self->{method} =~ tr/a-z/A-Z/;

	# Send the query to the server.
	if ($query) {
		if ($self->{method} eq 'POST') {
			$self->{sock}->print(
				"$self->{method} $self->{url} HTTP/1.0\012",
				"User_Agent: $self->{toolversion}\012",
				"Content-type: application/x-www-form-urlencoded\012",
				"Content-length: ", length($query)-3, "\012\012", $query,
				);
		}
		else {
			$self->{sock}->print(
				"$self->{method} $self->{url}\?$query" . "HTTP/1.0\012",
				"User_Agent: $self->{toolversion}\012",
				"Content-type: application/x-www-form-urlencoded\012",
				);
		}
	}
	else {
		# Usually we can only use POST if there are parameters, so convert
		# to GET if not.
		if ($self->{method} eq 'POST') {
			$self->{method} = 'GET';
		}
		$self->{sock}->print(
			"$self->{method} $self->{url} HTTP/1.0\012",
			"User_Agent: $self->{toolversion}\012\012",
			);
	}
}


sub saveResults
{
	my ($self) = @_;

	my $buffer;

	my $header = 1;

	while ( (my $len = read($self->{sock}, $buffer, 1024)) > 0) {
		if ($header) {
			# skip HTTP header
			if ((my $start = index($buffer, 'SIMPLE')) > 0) {
				# print "header is |", substr($buffer, 0, $start), "|\n";
				my $used = substr($buffer, $start);
				$self->{out}->print($used);
				$header = 0;
			}
			else {
				$self->error(BAD_OUTPUT,
					"first block missing FITS SIMPLE keyword");
				$self->warning("response |$buffer|");
				return;
			}
		}
		else {
			$self->{out}->print($buffer);
		}
	}
}


sub finalize
{
	my ($self) = @_;

	if ($self->{out}) {
		$self->{out}->close;
	}

	if ($self->{sock}) {
		$self->{sock}->close;
	}
}


1;

