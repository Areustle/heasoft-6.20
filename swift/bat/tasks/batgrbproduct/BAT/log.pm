
package BAT::log;

sub call {
    my ($self, $log, $cmd, $noprint) = @_;
    my (@errlog);
    my ($status);

    push @$log, "Calling command:";
    push @$log, "  $cmd";

    # Make sure we don't hang for standard input
    if ($cmd !~ m/[<|]/) { $cmd .= "< /dev/null"; }
    @errlog = `$cmd`;
    $status = $?;

    push @$log, @errlog;
    push @$log, "  --> command terminated with status $status";
    push @$log, "";
    if ($status) { 
	print "@errlog" unless($noprint);
    }

    return $status;
}

sub callnote {
    my ($self, $log, $cmd, $note, $noprint) = @_;
    
    push @$log, "** $note";
    $self->call($log, $cmd, $noprint);
}

sub sep {
    my ($self, $log) = @_;
    my ($ts);

    $ts = localtime(time());
    
    push @$log, "============================================ $ts";
}

sub timestamp {
    my ($self, $log) = @_;
    my ($ts);

    $ts = localtime(time());
    
    push @$log, "$ts";
    return $info;
}

sub log {
    my ($self, $log, $string, $noprint) = @_;

    push @$log, "$string";
    print "$string\n" unless ($noprint);

    return $info;
}

1;


