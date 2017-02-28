#
#  skymap command fully implemented in Tcl
#
proc skymap {args} {
#
#  Constants (may be editted for location)
#
   set BRSCRIPT "xbrowse_extract.pl"  ;# Name of query script
   set SDCHOST "www.asdc.asi.it"      ;# Host to query with sdc option
   set RADECCOL {{ra dec} {_RAJ2000 _DEJ2000} {1 2}} ;# RA/Dec to try
   set LABELCOL {name Name 3}         ;# Label columns to try

   parseparm [parmloc skymap] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }
#
# Build label args common to all sources
#
   set labelArgs {}
   if { $parval(color) != "" } { lappend labelArgs color=$parval(color) }
   if { $parval(csize) != "" } { lappend labelArgs csize=$parval(csize) }
   if { $parval(lwidth) != "" } { lappend labelArgs lwidth=$parval(lwidth) }
   if { $parval(font) != "" } { lappend labelArgs font=$parval(font) }
   if { $parval(symbol) != "" } { lappend labelArgs symbol=$parval(symbol) }
   if { $parval(symcolor) != "" } { 
      lappend labelArgs symcolor=$parval(symcolor) 
   }
   if { $parval(symcsize) != "" } { 
      lappend labelArgs symcsize=$parval(symcsize) 
   }
   if { $parval(symlwidth) != "" } { 
      lappend labelArgs symlwidth=$parval(symlwidth) 
   }
   lappend labelArgs "clip"
#
# Determine column label (Default: "", meaning numbered)
#
   set incolumns [listclean $parval(incolumns)]
   set collbl ""
   set dolabel $parval(labels)
   if { $parval(class) } {
      set collbl "class"
      set dolabel 1
   } 
   if { $incolumns != "" } {
      if { [llength $incolumns] == 1 } {
         set collbl $incolumns
         set incolumns ""
      } elseif { [llength $incolumns] == 2 } {
         set RADECCOL [list $incolumns]
      } elseif { [llength $incolumns] > 2 } {
         set collbl [lindex $incolumns 2]
         set RADECCOL [list [lrange $incolumns 0 1]]
      }
   }
#
# Determine site to query
#
   if { $parval(sdc) } {
      set site "sdc"
   } else {
      set site "heasarc"
   }
#
# Determine file format
#
   set oldbrowse $parval(browse_dcoord_output)
   if { [string compare $site "sdc"] == 0 } { set oldbrowse 1 }
#
# Query or local file?
#
   set filename $parval(file_input)
   set dbase $parval(dbase)
   if { $dbase == "" } { set dbase "optical" }
   if { $parval(browse_dcoord_output) || $filename != "" } {
      set dbase ""
   }

   if { $dbase == "?" } {
      syscall $BRSCRIPT table= | grep '^|'
      return
   }

   if { [chh key=LOADED] == 0 } {
      txwrite "Map not loaded" 10
      error {}
   }

   if { $filename == "" } {
      set szx [chh key=SZX]
      set szy [chh key=SZY]
      set xcen [expr $szx/2.0 + 0.5]
      set ycen [expr $szy/2.0 + 0.5]
      coord ximg=$xcen yimg=$ycen
      set ra $coord(ra)
      set dec $coord(dec)
      set equinox $coord(equinox)
      set xtr [expr $szx + 0.5]
      set ytr [expr $szy + 0.5]
      coord ximg=$xtr yimg=$ytr

      global tchat lchat
      set cleanup "chat $tchat $lchat"
      chat 5 5
      offset ra1=$ra dec1=$dec ra2=$coord(ra) dec2=$coord(dec)
      eval $cleanup

      if { ![info exists offset(deg)] || [isastbad $offset(deg)] || 
           [isastbad $ra] || [isastbad $dec] } {
         txwrite "Image has undefined values for sky coordinates of center and/or edge" 5
         error {}
      }
#     Radius in arcmin
      set radius [expr $offset(deg)*60.]
      set filename "$dbase.txt"
      regsub -all {[/]} [string tolower $filename] {} filename
#
#  Query database
#
      if { $site == "sdc" } {
         set cmd "syscall xwebquery.pl host='$SDCHOST' \
                  url='/cgi-bin/brodev' method=POST Cat='$dbase' \
                  ra='$ra' dec='$dec' radius='$radius' \
                  eqy='$equinox' > $filename"
      } else {
         set cmd "syscall $BRSCRIPT position=$ra,$dec radius=$radius \
                  equinox=$equinox table=$dbase outfile=$filename \
                  resultmax=0"
      }
      txwrite $cmd 10
      eval $cmd
   }
   if [info exists skymap] { unset skymap }
   if { $oldbrowse } {
#
#  Special routine to read browse dcoord format implented at end of file
#
      xan::rdoldbrowse $filename skymap

   } else {
#
#  Look for '|' in first character of file (browse_extract format)
#  If not, try comma-delimited parsing.  If that fails, try spaces
#
      set fp [open $filename r]
      set line [lindex [split [read $fp 80] "\n"] 0]
      close $fp

      if [regexp {^[|]} $line] {
         txwrite "Input is pipe-delimited" 15
         rdarray var=skymap delim=| {expr=^[^|]} readhead $filename
      } else {
         if [regexp {does not seem to exist} $line] {
            txwrite "Table $dbase does not exist" 5
            txwrite "See $filename for supported values of dbase" 10
            error {}
         }
         txwrite "Try processing input as comma-delimited" 15
         set rdstat [catch {rdarray var=skymap {expr=^\(([0-9]+)\)} \
                                    match=equinox delim=, $filename} ]
         set ncols [llength [array names skymap]]
         if { $rdstat != 0 || $ncols < 2 } {
            txwrite "Try processing input as space-delimited" 15
            rdarray var=skymap {expr=^\(([0-9]+)\)} \
                    match=equinox $filename
         }
      }
      if { [llength [array names skymap]] < 2 } {
         txwrite " Invalid input file format" 10
         error {}
      }
   }
#
#  Look for usable RA/Dec columns
#
   set found 0
   foreach rdpair $RADECCOL {
      set rcol [lindex $rdpair 0]
      set dcol [lindex $rdpair 1]
      if { [info exists skymap($rcol)] &&
           [info exists skymap($dcol)]  } {
         set found 1
         break
      }
   }
   if { !$found } { error {No RA/Dec to plot} }

   if { $dolabel && $collbl == "" } {
      foreach col $LABELCOL {
         if [info exists skymap($col)] {
            set collbl $col
            break
         }
      }
      if { $collbl == "" } {
         txwrite "No default label found, specify with 'incolumns'" 10
      }
   } 
   if { $collbl != "" && ![info exists skymap($collbl)] } {
      txwrite "Column ($collbl) does not exist, available columns:\
               [array names skymap]" 10
      txwrite "Defaulting to numbered labels" 10
      set collbl ""
   }
#
#  Print values to screen
#
   set order [list $rcol $dcol]
   if { $incolumns != "" } {
      set order $incolumns
   } elseif { $collbl == "" } {
      if [info exists skymap(name)] {
         lappend order "name"
      } elseif [info exists skymap(Name)] {
         lappend order "Name"
      }
   } else {
      lappend order $collbl
   }
   if { $parval(out_file) != "" } {
      prarray order=$order filename={$parval(out_file)} skymap iplus
   } else {
      prarray order=$order skymap iplus
   }
#
#  Account for input equinox
#
   set cleanup ""
   global default
   set svequinox $default(equinox)
   if { [info exists equinox] && 
        $equinox != "" && 
        $svequinox != $equinox } {
      set cleanup "set default(equinox) $svequinox; \
                   pgtk::journal {set default(equinox) $svequinox}"
      set default(equinox) $equinox
      pgtk::journal "set default(equinox) $equinox"
   }
#
#  Label all sources
#
   for { set i 0 } { $i < [llength $skymap($rcol)] } { incr i } {
      set tmpArgs $labelArgs
      set raval [lindex $skymap($rcol) $i]
      set decval [lindex $skymap($dcol) $i]
      if { ![regexp {^[\s0-9.:+-]+} $raval] ||
           ![regexp {^[\s0-9.:+-]+} $decval] } { 
         txwrite "Bad RA/Dec value: $raval $decval" 5
         if { $cleanup != "" } { eval $cleanup }
         error {}
      }
      lappend tmpArgs "ra=[lindex $skymap($rcol) $i]"
      lappend tmpArgs "dec=[lindex $skymap($dcol) $i]"
      if { $collbl == "" || !$dolabel } {
         #lappend tmpArgs "text=[expr $i+1]"
         lappend tmpArgs [expr $i+1]
      } else {
         #lappend tmpArgs "text=[lindex $skymap($collbl) $i]"
         lappend tmpArgs [lindex $skymap($collbl) $i]
      }
      if [catch {eval label $tmpArgs}] {
         if { $cleanup != "" } { eval $cleanup }
         error {Failed to draw label}
      }
   }
   if { $cleanup != "" } { eval $cleanup }
   return
}

#
#  Special helper routine to read browse_dcoord_output for skymap
#  Not based on delimiters, but specific column ranges
#

# Hide from general use (skymap use only)
namespace eval xan {

proc rdoldbrowse { filename varname } {
#
#  Column ranges
#
    set COLSPEC(name) {7 16}
    set COLSPEC(ra) {18 27}
    set COLSPEC(dec) {30 40}

    set fp [open $filename r]
    set data [read $fp]
    close $fp

    upvar 1 $varname outary
    if [info exists outary] { unset outary }

    set data [split $data "\n"]
    set start 0
    foreach line $data {
       if [regexp {^\s*$} $line] { continue } ;# Throw out blank lines
       #
       #  Line beginning with '>' indicates start
       #
       if { [string range $line 0 0] == ">" } {
          set start 1
       }
       if { !$start } { continue }
       foreach idx [array names COLSPEC] {
          lappend outary($idx) [string range $line [lindex $COLSPEC($idx) 0] \
                                               [lindex $COLSPEC($idx) 1]]
       }
    }
}

} ;# end xan namespace
