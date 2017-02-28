package HEACORE::PIL;

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD);

require Exporter;
require DynaLoader;
require AutoLoader;

@ISA = qw(Exporter DynaLoader);
# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.
@EXPORT = qw(
  PILGetBool
  PILGetInt
  PILGetReal
  PILGetReal4
  PILGetString
  PILGetFname
  PILPutBool
  PILPutInt
  PILPutReal
  PILPutString
  PILPutFname
  PILInit
  PILClose

);
$VERSION = '1.00';


bootstrap HEACORE::PIL $VERSION;

# Preloaded methods go here.

# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
# Below is the stub of documentation for your module. You better edit it!

=head1 NAME

PIL - Perl extension for PIL

=head1 SYNOPSIS

  use HEACORE::PIL;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for PIL was created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head1 Exported constants



=head1 AUTHOR

Ziqin Pan, zpan@milkyway.gsfc.nasa.gov

=head1 SEE ALSO

perl(1).

=cut
