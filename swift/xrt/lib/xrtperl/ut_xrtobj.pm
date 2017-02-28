#!/usr/bin/perl
#
# Modul name: ut-xrtlib.pl
# 
# 
# Description:	
#          Library functions for Unit Tests
#		   
# 
# Author/Date: Italian Swift Archive Center (Frascati)
# 
# History:
# 
#      0.0.1 : CS 02/08/2004 - First Release
#      0.1.0 : FT 10/08/2004 - ftdiff tolerance encreased
#                              to 0.03
#      0.1.1 : BS 21/09/2004 - Added check on warnings and/or
#                              errors when displayed messages
#      0.1.2 : BS 22/09/2004 - Bug fixed during enviroment variable 
#                              check
#      0.1.3 : BS 13/01/2005 - Deleted 'caldbdir' input parameter
#      0.1.4 : BS 24/03/2005 - Build 14
#      0.1.5 :    01/07/2005 - Deleted message on the warning log file
#                              when thes UT does not create it
#      0.1.6 :    28/10/2005 - Modified build version
#      0.1.7 :    16/02/2006 - Modified build version
#      0.1.8 :    18/05/2006 - Modified build version
#      0.1.9 :    08/11/2006 - Modified build version
#      0.2.0 : NS 19/04/2007 - Modified build version
#      0.2.1 :    12/07/2007 - Modified build version
#      0.2.2 :    16/11/2007 - ftdiff tolerance encreased to 0.035
#      0.2.3 :    28/03/2008 - Added ftdiff_mkf function
#      0.2.4 :    24/04/2008 - Modified build version
#      0.2.5 :    03/10/2008 - Modified build version
#      0.2.6 :    10/03/2009 - Modified build version
#      0.2.7 :    14/07/2009 - Modified build version
#      0.2.8 :    26/07/2010 - Modified build version
#      0.2.9 :    01/04/2011 - Modified build version
#      0.3.0 :    10/02/2012 - Modified build version
#      0.3.1 :    24/04/2013 - Modified build version
#      0.3.2 :    18/06/2014 - Modified build version
#      0.3.3 :    19/06/2015 - Modified build version
#      0.3.4 :    18/10/2016 - Modified build version
#                               
#

package ut_xrtobj;

use strict;

use constant STATUS_OK            =>     0;
use constant STATUS_DIRECTORY     =>     1;
use constant STATUS_ENVIRONMENT   =>     2;
use constant STATUS_INPUTFILE     =>     4;
use constant STATUS_OUTPUTFILE    =>     8;
use constant STATUS_PROTOTYPE     =>    16;
use constant STATUS_REFDATAFILE   =>    32;
use constant STATUS_CALDBFILE     =>    64;
use constant STATUS_EXECFILE      =>   128;
use constant STATUS_EXECUTION     =>   256;
use constant STATUS_FVERIFY       =>   512;
use constant STATUS_FTDIFF        =>  1024;
use constant STATUS_DIFF          =>  2048;
use constant STATUS_CALDB          => 4096;
use constant STATUS_WARNING        => 8192;

#use constant STATUS_              => 16384;
#use constant STATUS_              => 32768;
#use constant STATUS_               => 65536;
#use constant STATUS_               => 131072;
#use constant STATUS_               => 262144;
#use constant STATUS_               => 524288;
#use constant STATUS_               => 1048576;
#use constant STATUS_               => 2097152;
#use constant STATUS_               => 4194304;
#use constant STATUS_               => 8388608;
#use constant STATUS_               => 16777216;
#use constant STATUS_               => 33554432;
#use constant STATUS_               => 67108864;
#use constant STATUS_               => 134217728;
#use constant STATUS_               => 268435456;
#use constant STATUS_               => 536870912;
#use constant STATUS_               => 1073741824;
#use constant STATUS_               => 2147483648;
use constant STATUS_LAST           => 4294967296;

my @error_msg;
$error_msg[STATUS_DIRECTORY]     = "directory";
$error_msg[STATUS_ENVIRONMENT]   = "environment";
$error_msg[STATUS_INPUTFILE]     = "input files";
$error_msg[STATUS_OUTPUTFILE]    = "output files";
$error_msg[STATUS_PROTOTYPE]     = "prototype";
$error_msg[STATUS_REFDATAFILE]   = "refdata files";
$error_msg[STATUS_CALDBFILE]     = "caldb files";
$error_msg[STATUS_EXECFILE]      = "executables files";
$error_msg[STATUS_EXECUTION]     = "run task";
$error_msg[STATUS_FVERIFY]       = "fverify errors";
$error_msg[STATUS_FTDIFF]        = "ftdiff errors";
$error_msg[STATUS_DIFF]          = "diff";
$error_msg[STATUS_CALDB]         = "caldb";
$error_msg[STATUS_WARNING]       = "warning messages";

# Info level
use constant INFO_MUTE        =>  0;
use constant INFO_SILENT        =>  2;
use constant INFO_NORMAL        =>  3;
use constant INFO_CHATTY         =>  4;

#########################################################################
# ISTANCE
#########################################################################
sub new
{
	my ($class, %args) = @_;

   my %info = (
                 name       => "",
                 utname     => "",
	         version    => "",
                 build      => "33",
                 pversion   => substr(`xrtversion`, 0, -1),
                 start      => substr(`date`, 0, -1),
                 user       => $ENV{'USER'},
                 hostname   => $ENV{'HOSTNAME'},
                 input_dir  => "",
                 output_dir => "",
                 chatter    => INFO_NORMAL,
	         clobber    => "no",
                 validate   => 1,
                 status     => STATUS_OK,
                 errmsg     => "",
                 %args
            );

	my $this = bless( {%info}, $class);

# Get Command Line Parameter
   $this->setCmdLineParam();
   if ($this->{input_dir} eq "" || $this->{output_dir} eq "")
   {
      $this->usage();
      exit(-1);
   }

# Print Information Parameter
	$this->printInitInfo();

# CleanUp Log file
	$this->{LOGERROR} = $this->{output_dir} . "/" . $this->{utname} . "_error";
	if ( -f $this->{LOGERROR})
	{
	    unlink($this->{LOGERROR});
	}
	$this->{LOGWARN} = $this->{output_dir} . "/" . $this->{utname} . "_warning";
	if ( -f $this->{LOGWARN})
	{
	    unlink($this->{LOGWARN});
	}

# Environment Setting
   $ENV{HEADASNOQUERY} = 1;

	return $this;
}

sub delete
{
    my ($this) = @_;
    
    $this->printEndInfo();
    if ($this->getParam("status") == STATUS_WARNING) {
	rename $this->{LOGERROR}, $this->{LOGWARN};
	exit(2);
    }
    elsif ($this->getParam("status"))
    {
	exit(1);
    }
    else
   {
       exit(0);
   }
}
#########################################################################
# INFO
#########################################################################
sub usage
{
	my ($this) = @_;

   $this->printk (INFO_MUTE, "\n");
   $this->printk (INFO_MUTE, "      " .
           "Usage: ut_$this->{name} <indir> <outdir> [parameter=<value>...]\n");
   $this->printk (INFO_MUTE, "             parameter:\n");
   $this->printk (INFO_MUTE, "                     chatter: (0-5)\n");
   $this->printk (INFO_MUTE, "                     clobber: (yes/no)\n");
   $this->printk (INFO_MUTE, "\n");
}
sub stem
{
	my ($this) = @_;

   my $stem = $this->getParam("utname") . "_" . $this->getParam("version");
   return $stem;
}

sub printEndInfo
{
	my ($this) = @_;

   my $stem = $this->stem();
   my $status = $this->getParam("status");
   my $result;

	if ( $status == STATUS_WARNING ) { $result = 'warnings'; }
	else { $result = $status ? 'failed' : 'passed'; }
	$this->printk (INFO_MUTE, "----------------------------------------");
	$this->printk (INFO_MUTE, "----------------------------------------\n");
   $this->printk (INFO_MUTE, "$stem: ..... : $result ($status)\n");

   for (my $i = 1; $i < STATUS_LAST; $i *= 2)
   {
      if ($status & $i)
      {
         $this->printk (INFO_MUTE,"$stem: ..... : $error_msg[$i]\n");
      }
   }
   if ($status & STATUS_FVERIFY    ||
       $status & STATUS_FTDIFF     ||
       $status & STATUS_DIFF)
   {
      $this->printk (INFO_NORMAL,"$stem: ..... : For details see '$this->{LOGERROR}' file\n");
   }

   elsif ($status & STATUS_WARNING )
   {
      $this->printk (INFO_NORMAL,"$stem: ..... : For details see '$this->{LOGWARN}' file\n");
   }


	$this->printk (INFO_MUTE, "----------------------------------------");
	$this->printk (INFO_MUTE, "----------------------------------------\n");
}

sub printInitInfo
{
	my ($this) = @_;

   my $stem = $this->stem();
	$this->printk (INFO_MUTE, "----------------------------------------");
	$this->printk (INFO_MUTE, "----------------------------------------\n");
   $this->printk (INFO_MUTE, "$stem: ..... running " . 
           $this->getParam("name") .
          " build " .
           $this->getParam("build") .
          " - Test\n");
   $this->printk (INFO_MUTE, "$stem: ..... start date      : " .
           $this->getParam("start") . "\n");
   $this->printk (INFO_MUTE, "$stem: ..... package version : " .
           $this->getParam("pversion") . "\n");
   $this->printk (INFO_MUTE, "$stem: ..... operator name   : " .
           $this->getParam("user") . "\n");
   $this->printk (INFO_MUTE, "$stem: ..... input dir       : " .
           $this->getParam("input_dir") . "\n");
   $this->printk (INFO_MUTE, "$stem: ..... output dir      : " .
           $this->getParam("output_dir") . "\n");
	$this->printk (INFO_MUTE, "----------------------------------------");
	$this->printk (INFO_MUTE, "----------------------------------------\n");
}

sub printk
{
   my ($this, $level, @rest ) = @_;

   if ( $level <= $this->getParam("chatter") )
   {
      printf (@rest);
   }
}

#########################################################################
# EXEC TASK
#########################################################################
sub runTask
{
	my ($this, %task) = @_;

   my $command = $this->getParam("name");
   foreach my $p (keys %task)
   {
      $command .= " $p=$task{$p}";
   }

   $this->printk (INFO_CHATTY, "running command: $command\n");
   my $result = qx($command);
   $this->printk (INFO_NORMAL, $result);
   if ($?)
   {
      $this->error(STATUS_EXECUTION, "running $command");
   }
}

sub fverify
{
    my ($this, $file) = @_;
    
    my $search = "^\\*\\*\\*\\* Verification found 0 warning\\(s\\) and 0 error\\(s\\). \\*\\*\\*\\*\$";
    my $command = "fverify " . $file;

    my $stem = $this->stem();
    $this->printk (INFO_MUTE, "$stem: running fverify on $file\n");
    $this->printk (INFO_CHATTY, "$command\n");
    
    my $result = $this->execCmd($command, $search);
    #  $search =~ s/0/\[0-9\]\+/g;
 
    if ($result =~ $search)
    {
	
	$this->success("$file format is ok\n");
    }
    elsif ($result =~ /and 0 error\(s\)/)
    {
	$this->printk(INFO_MUTE,"Warning: = <$result>\n");
	$this->warning(STATUS_WARNING, "on $file\n");
	
    }
    else
    {
	$this->printk(INFO_CHATTY,"Error: = <$result>\n");
	$this->error(STATUS_FVERIFY, "on $file\n");
	
    }
}
sub ftdiff
{
	my ($this, $file, $template) = @_;

	my $search = "^\\*\\*\\*\\* End of file comparison:  0 differences were found\$";
	my $command = "ftdiff " . $file . " " . $template .
	    " exclude = \"DATE, USER, CREATOR, DATASUM\"" .
		" tolerance=0.035";
	my $stem = $this->stem();
	$this->printk (INFO_MUTE,
		       "$stem: running ftdiff between $file $template\n");
	$this->printk (INFO_CHATTY, "$stem: running $command\n");
	
	my $result = $this->execCmd($command, $search);

#	$search =~ s/0/\[0-9\]\+/;
	if ($result =~ $search)
	{
	    $this->success("No difference found between $file and prototype\n");
	}
	else
	{
	    $this->printk(INFO_CHATTY,"Error: = <$result>\n");
	    $this->error(STATUS_FTDIFF, "difference found from prototype\n");
	}   

}
sub ftdiff_mkf
{
	my ($this, $file, $template) = @_;

	my $search = "^\\*\\*\\*\\* End of file comparison:  0 differences were found\$";
	my $command = "ftdiff " . $file . " " . $template .
	    " exclude = \"DATE, USER, CREATOR, DATASUM, TTYPE*, TFORM*, TNULL*, TSCAL*, TUNIT*, TZERO*\"" .
		" tolerance=0.035";
	my $stem = $this->stem();
	$this->printk (INFO_MUTE,
		       "$stem: running ftdiff between $file $template\n");
	$this->printk (INFO_CHATTY, "$stem: running $command\n");
	
	my $result = $this->execCmd($command, $search);

#	$search =~ s/0/\[0-9\]\+/;
	if ($result =~ $search)
	{
	    $this->success("No difference found between $file and prototype\n");
	}
	else
	{
	    $this->printk(INFO_CHATTY,"Error: = <$result>\n");
	    $this->error(STATUS_FTDIFF, "difference found from prototype\n");
	}   

}
sub diff
{
	my ($this, $file, $template) = @_;

   my $command = "diff " . $file . " " . $template;

   my $stem = $this->stem();
   $this->printk (INFO_MUTE,
                  "$stem: running diff between $file $template\n");
   $this->printk (INFO_CHATTY, "$stem: running $command\n");
   my $result = $this->execCmd($command, "");
   if ($? == 0)
   {
      $this->success("$file format is ok\n");
   }
   elsif ($? == 512)
   {
      $this->abort(STATUS_DIFF, "$result");
   }
   else
   {
      $this->error(STATUS_DIFF, "differences found from prototype\n");
   }
}
sub writeLog
{
   my ($this, $cmd, @buffer) = @_;

   my $stem = $this->stem();
   open  LOG, ">> $this->{LOGERROR}";
   print LOG "\n\n####################################################\n";
   print LOG "# $stem:\n#  $cmd\n";
   print LOG "########################################################\n";
   print LOG @buffer;
   close LOG;
}
sub execCmd
{
   my ($this, $cmd, $str) = @_;

   my $err = 1;
   my $line = 0;
   my @buffer;
   my $p = open(FILEH, "$cmd 2>&1 |");
   if ($p)
   {
      while(<FILEH>)
      {
         $this->printk (INFO_CHATTY, "$_");
         $buffer[$line] = $_;


         if ($str && ($_ =~ $str))
         {
            $err = 0;
            #last;
	}
	 $line ++;
      }
      close(FILEH);
      $_ = "";
      if (!$str && ($#buffer == -1))
      {
         $err = 0;
      }
      if ($err)
      {
	  $this->writeLog($cmd, @buffer);
      }
   }
   return $buffer[$line - 1];
}
#########################################################################
# PARAMETER
#########################################################################
sub setParam
{
	my ($this, $key, $val) = @_;

	$this->{$key} = $val;
}

sub getParam
{
	my ($this, $key) = @_;

	return $this->{$key};
}

sub putParam
{
	my ($this, $args) = @_;
	my ($k, $v);
	my %copy = %{$this};

   print (INFO_MUTE, "------ Parameter ------\n");
	while (($k, $v) = each(%copy))
	{
		print (INFO_MUTE, "$k = $v \n");
	}
   print (INFO_MUTE, "-----------------------\n");
}

sub setInteractParam
{
	my ($this, $key) = @_;

   if (! $this->getParam($key))
   {
      my ($puntoPar) = $this->getParam("name") . ".par";
      my $val = `pquery2 $puntoPar $key`;
      chop($val);
	   $this->setParam($key, $val);
   }
}

sub setCmdLineParam
{
	my ($this) = @_;

   my $i = 0;
   my ($k,$v, $a);
   foreach $a (@ARGV)
   {
      if ($i == 0)
      {
         $this->setParam("input_dir", $a);
      }
      elsif ($i == 1)
      {
         $this->setParam("output_dir", $a);
      }
      else
      {
         ($k,$v) = split("=",$a);
         $this->setParam($k, $v);
      }
      $i ++;
   }
}

#########################################################################
# ERROR
#########################################################################
sub report
{
   my ($this,$type, $msg) = @_;
   my $stem = $this->stem();
   $this->printk (INFO_MUTE, "%s: %s: %s\n", $stem, $type, $msg);
}

sub error
{
   my ($this, $code, @rest) = @_;
   $this->{status} |= $code;
   $this->report('*** Error ***', @rest);
}

sub abort
{
   my ($this, $code, @rest) = @_;
   $this->{status} |= $code;
   $this->report('*** Abort ***', @rest);
}

sub warning
{
   my ($this, $code, @rest) = @_;
   $this->{status} |= $code;
   $this->report('Warning', @rest);
}

sub success
{
   my ($this, @rest) = @_;
   $this->report('Success', @rest);
}

sub isValid
{
   my ($this) = @_;
   if ($this->getParam("validate") == 1)
   {
       return not $this->getParam("status");
   }
   else
   {
      return 1;
   }
}

sub isValidStatus
{
   my ($this, $type_bit) = @_;
   return not ($this->getParam("status") & $type_bit);
}

#########################################################################
# CHECK
#########################################################################
sub checkHead
{
   my ($this, $type) = @_;

   my $stem = $this->stem();
   $this->printk (INFO_MUTE, "%s: ...................... Checking %s\n",
                  $stem, $type);

}
sub checkTail
{
   my ($this, $type, $type_bit) = @_;

   my $stem = $this->stem();
   if ($this->isValidStatus($type_bit))
   {
      $this->printk(INFO_MUTE, "%s: ...................... Checking %s: Success\n",
             $stem, $type);
   }
   else
   {
      $this->printk(INFO_MUTE,"%s: ....................... Checking %s: Error\n", $stem, $type);
   }
   $this->printk (INFO_MUTE, "\n");
}
sub checkPath
{
   my ($this, $file) = @_;

   my $flag_ok = (-x $file);
   foreach my $dir (split(/:/,$ENV{'PATH'}))
   {
      if ($flag_ok)
      {
         return $flag_ok;
      }
      $flag_ok = (-x $dir . "/" . $file);
   }
   return 0;
}
sub check
{
   my ($this, $type, $type_bit, @chk_args) = @_;

   $this->checkHead($type);

   my $flag_ok;
   for(my $i=0; defined($chk_args[$i]) && $chk_args[$i] ne ""; $i++)
   {
      $flag_ok = 0;
      if ($type_bit == STATUS_DIRECTORY)
      {
           $flag_ok = (-d $chk_args[$i]);
      }
      elsif ($type_bit == STATUS_ENVIRONMENT)
      {
#########################################################################
# PATCH - Togliere dalla libreria e gestire nel ut_task
#########################################################################
	  if ( $chk_args[$i] eq "CALDB" && !defined($ENV{$chk_args[$i]}) ) {
	     $this->{status} |= STATUS_CALDB;
	      $this->report('Error', "CALDB environment not set");
	      $this->report('Error', "Please set the CALDB environment");
	     $flag_ok = 0; 
	    
# nel ut_task chiamare: $ut_task_obj->usage();
# quindi fare la delete ed uscire se necessario
	      $this->usage;

	  }
	  else {
	      $flag_ok = defined($ENV{$chk_args[$i]});
	      
	  }
      }
      elsif ($type_bit == STATUS_INPUTFILE ||
             $type_bit == STATUS_OUTPUTFILE ||
             $type_bit == STATUS_PROTOTYPE ||
             $type_bit == STATUS_CALDBFILE)
      {
           $flag_ok = (-e $chk_args[$i]);
      }
      elsif ($type_bit == STATUS_EXECFILE ||
             $type_bit == STATUS_REFDATAFILE)
      {
           $flag_ok = $this->checkPath($chk_args[$i]);
       }
      if ($flag_ok)
      {
	  if ($this->getParam("chatter") >= INFO_CHATTY)
	  {
	      $this->success($chk_args[$i]);
	  }
      }
      else
      {
	  $this->error($type_bit, "$type '$chk_args[$i]'");
	  $this->printEndInfo();
	  exit(-1);
      }
  }

   $this->checkTail($type, $type_bit);
}
sub checkIODirectory
{
   my ($this) = @_;

   $this->checkHead("directory");

   my $dir = $this->getParam("input_dir");
   if (! -d $dir)
   {
      $this->error(STATUS_DIRECTORY, "<indir> $dir");
   }
   else
   {
       if ($this->getParam("chatter") >= INFO_CHATTY)
       {
	   $this->success("input_dir $dir");
       }
   }

   $dir = $this->getParam("output_dir");
   if (! -d $dir)
   {
       system("mkdir -p $dir");
       if ($this->getParam("chatter") >= INFO_CHATTY)
       { 
	   $this->warning(STATUS_OK, "creating output dir $dir");
       } 
   }
   else
   {
       if ($this->getParam("chatter") >= INFO_CHATTY)
       {
	   $this->success("output_dir $dir");
       }
   }
   $this->checkTail("directory", STATUS_DIRECTORY);
}
sub checkExist
{
   my ($this, $title, @chk_file) = @_;

   $this->check("$title Files", STATUS_INPUTFILE, @chk_file);
}
sub checkExecutable
{
   my ($this, @chk_file) = @_;

   $this->check("Executables", STATUS_EXECFILE, @chk_file);
}
sub checkEnvironment
{
   my ($this, @chk_var) = @_;

   $this->check("Environment", STATUS_ENVIRONMENT, @chk_var);
}

#########################################################################
# END
#########################################################################
1;
