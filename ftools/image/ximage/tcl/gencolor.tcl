#
#  Generate color image from event file
#
proc gencolor {args} {

   global xm_tcl_prompt xtkcols

   parseparm [parmloc gencolor] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }

   set tmploc "/tmp"
#
   set cutmode $parval(cutmode);# hist, linear, maxcol, or log
   set rmin $parval(rmin)      ;# Minimum value to use from red image
   set rmax $parval(rmax)      ;# Maximum value to use from red image
   set gmin $parval(gmin)      ;# Minimum value to use from green image
   set gmax $parval(gmax)      ;# Maximum value to use from green image
   set bmin $parval(bmin)      ;# Minimum value to use from blue image
   set bmax $parval(bmax)      ;# Maximum value to use from blue image
   if { $cutmode == "" } {
      set query 1
   } else {
      set query 0
   }
   set chanlist $parval(chanlist)
   set ecol $parval(ecol)
   set infile $parval(infile)
   set outfile $parval(outfile)
   set jpgfile $parval(jpgfile)

   set base $parval(base)      ;# Base used to encode colors 
   if { $parval(displayonly) } {
      if { $base == "" } {
#        Use fitstcl if available
         catch {
            load libfitstcl.so
            set clrObj [fits open $parval(infile)]
            set base [lindex [lindex [$clrObj get keyword MKCBASE] 0] 1]
         }
         if { $base == "" } {
            txwrite " Unable to determine encoding base, assuming 6..." 10
            set base 6
         }
      }
      xan::dispcolor $infile $base $jpgfile
      return
   }
   if { $base == "" } { set base 6 }

   if { $infile == "" } {
      txwrite "No event file entered" 10
      error {}
   }
   if { $outfile == "" } { set outfile "gencolor.img" }
#
# Note: In order to get TELESCOP and INSTRUME event file is
# read in one more time than it really has to be.  To change,
# must spawn fkeyprint and parse or provide header retrieval
# from Tcl
#
   if { [llength $chanlist] == 0 } {
      txwrite " Read in event file to get TELESCOP/INSTRUME" 10
      txwrite "!${xm_tcl_prompt}read_image $infile" 10
      read_image $infile
      set telescop [chheader key=telescop]
      set instrume [chheader key=instrume]
#
# Set default energy ranges
#
      if { [regexp -nocase {^rosat} $telescop] && 
           [regexp -nocase {^pspc}  $instrume] } {
         set ecol PI
         set rminchan 10
         set rmaxchan 40
         set gminchan 41
         set gmaxchan 90
         set bminchan 91
         set bmaxchan 202
      } elseif { [regexp -nocase {^chandra} $telescop] && 
                 [regexp -nocase {^acis}  $instrume] } {
         set ecol energy
         set rminchan 200
         set rmaxchan 1500
         set gminchan 1500
         set gmaxchan 2500
         set bminchan 2500
         set bmaxchan 8000
      } else {
         txwrite "No channel ranges defined for $telescop:$instrume" 10
         error {}
      }
   } else {
      if { [llength $chanlist] != 6 } {
         error { Usage: chanlist={rminch rmaxch gminch gmaxch bminch bmaxch}}
      }
      if { $ecol == "" } { set ecol PI }
      set rminchan [lindex $chanlist 0]
      set rmaxchan [lindex $chanlist 1]
      set gminchan [lindex $chanlist 2]
      set gmaxchan [lindex $chanlist 3]
      set bminchan [lindex $chanlist 4]
      set bmaxchan [lindex $chanlist 5]
   }
#
#  Concatenate read_image parameters
#
   set readArgs {}
#
#     Centering
#
   if { $parval(xpix) != "" } { lappend readArgs xpix=$parval(xpix) }
   if { $parval(ypix) != "" } { lappend readArgs ypix=$parval(ypix) }
   if { $parval(ra) != "" }   { lappend readArgs ra=$parval(ra) }
   if { $parval(dec) != "" }  { lappend readArgs dec=$parval(dec) }
#
#     Sizing
#
   if { $parval(size) != "" } { lappend readArgs size=$parval(size) }
   if { $parval(szx) != "" }  { lappend readArgs szx=$parval(szx) }
   if { $parval(szy) != "" }  { lappend readArgs szy=$parval(szy) }
#
#     Rebin
#
   if { $parval(rebin) != "" } { lappend readArgs rebin=$parval(rebin) }
   lappend readArgs $infile

#
#  Excecute reads and write temporary files
#
   set chanArgs {}
   lappend chanArgs ecol=$ecol
   lappend chanArgs emin=$rminchan
   lappend chanArgs emax=$rmaxchan
   txwrite "!${xm_tcl_prompt}read_image $chanArgs $readArgs" 10
   eval read_image $chanArgs $readArgs
   set tmpfile $tmploc/rtmp.img
   if [file exists $tmpfile] { file delete $tmpfile }
   txwrite "!${xm_tcl_prompt}write_image $tmpfile" 10
   write_image $tmpfile

   set chanArgs {}
   lappend chanArgs ecol=$ecol
   lappend chanArgs emin=$gminchan
   lappend chanArgs emax=$gmaxchan
   txwrite "!${xm_tcl_prompt}read_image $chanArgs $readArgs" 10
   eval read_image $chanArgs $readArgs
   set tmpfile $tmploc/gtmp.img
   if [file exists $tmpfile] { file delete $tmpfile }
   txwrite "!${xm_tcl_prompt}write_image $tmpfile" 10
   write_image $tmpfile

   set chanArgs {}
   lappend chanArgs ecol=$ecol
   lappend chanArgs emin=$bminchan
   lappend chanArgs emax=$bmaxchan
   txwrite "!${xm_tcl_prompt}read_image $chanArgs $readArgs" 10
   eval read_image $chanArgs $readArgs
   set tmpfile $tmploc/btmp.img
   if [file exists $tmpfile] { file delete $tmpfile }
   txwrite "!${xm_tcl_prompt}write_image $tmpfile" 10
   write_image $tmpfile
#
# Generate image with mkcolor
#
   set mkcolorSys "syscall"
   lappend mkcolorSys mkcolor
   if { $cutmode != "" } { lappend mkcolorSys cutmode=$cutmode }
   if { $rmin != "" } { lappend mkcolorSys rmin=$rmin }
   if { $rmax != "" } { lappend mkcolorSys rmax=$rmax }
   if { $gmin != "" } { lappend mkcolorSys gmin=$gmin }
   if { $gmax != "" } { lappend mkcolorSys gmax=$gmax }
   if { $bmin != "" } { lappend mkcolorSys bmin=$bmin }
   if { $bmax != "" } { lappend mkcolorSys bmax=$bmax }
   if { $base != "" } { lappend mkcolorSys base=$base }
   lappend mkcolorSys rfile=$tmploc/rtmp.img
   lappend mkcolorSys gfile=$tmploc/gtmp.img
   lappend mkcolorSys bfile=$tmploc/btmp.img
   lappend mkcolorSys outfile=$outfile
   lappend mkcolorSys clobber=yes
   if { $query } { 
      lappend mkcolorSys mode=ql
   } else {
      lappend mkcolorSys mode=h 
   }
   txwrite "Spawning mkcolor..." 10
   txwrite "$mkcolorSys" 10
   eval $mkcolorSys
#
   if { ![info exists ::debug] } { 
      txwrite "Cleaning up..." 10
      set cmd "syscall rm $tmploc/rtmp.img $tmploc/gtmp.img $tmploc/btmp.img"
      eval $cmd
   }

   if { $jpgfile != "" } {
      if { [file extension $jpgfile] == "" } {
         set cnvCmd "syscall colorcnv.pl $outfile ${jpgfile}.jpg"
      } else {
         set cnvCmd "syscall colorcnv.pl $outfile $jpgfile"
      }
      txwrite $cnvCmd 10
      eval $cnvCmd
   } else {
      xan::dispcolor $outfile $base
   }

}

namespace eval xan {

proc dispcolor {colfile base {jpgfile ""}} {
#
#  Internal routine to plot color image output from mkcolor tool
#
   global xm_tcl_prompt xtkcols default

   if { $colfile != "" } {
      txwrite "Read and display color image: $colfile" 10
      set cmd "read_image $colfile"
      txwrite "$xm_tcl_prompt$cmd" 10
      eval $cmd
   }

   set locfile ""
   if { $jpgfile != "" } {
      set locfile $jpgfile
   } elseif { $base != 4 && $base != 6 } {
      txwrite "Can only display base values of 4 and 6, not $base" 5
      set locfile "gencolor.jpg"
   }

   if { $locfile != "" } {
      if { [file extension $locfile] == "" } {
         set locfile "${locfile}.jpg"
      }
      txwrite "Generating graphic file: $locfile" 5
      set cnvCmd "syscall colorcnv.pl $colfile $locfile"
      txwrite $cnvCmd 10
      eval $cnvCmd
      return
   }

   set cmd "cct set rgb$base"
   txwrite "$xm_tcl_prompt$cmd" 10
   eval $cmd

   set cleanup "levels number=$default(numlevs)"
   set numlevs [expr $base*$base*$base-1]
   set cmd "levels number=$numlevs"
   txwrite "$xm_tcl_prompt$cmd" 10
   eval $cmd

   if { $xtkcols < [expr $numlevs + 20] } {
      set xtkcols [expr $numlevs + 20]
      close
   }

   set cmd "display linear minlev=0 maxlev=[expr $numlevs - 1]"
   txwrite "$xm_tcl_prompt$cmd" 10
   eval $cmd
   eval $cleanup
}

} ;# xan namespace
