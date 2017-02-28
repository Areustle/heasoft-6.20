#
#  Converts RA and Dec into the image pixel coordinates
#
proc ra_dec_to_pixel {args} {

   parseparm [parmloc ra_dec_to_pixel] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }

   set symbol $parval(symbol)
   set color $parval(color)
   set csize $parval(csize)
   set lwidth $parval(lwidth)
   set lstyle $parval(lstyle)
   set circle $parval(circle)
   set ra $parval(ra)
   set dec $parval(dec)

   global dismap curmap default

   set mapid $curmap
   txwrite "Conversion based on $mapid" 10

   if { [chh map=$mapid key=LOADED] == 0 } {
      txwrite "Image not loaded" 10
      error {}
   }

   set wcsid [chh map=$mapid key=wcsid]
   if { $wcsid eq "" } {
      txwrite "WCSID not found" 10
      error {}
   }

   if { $ra eq "" || $dec eq "" } {
      set svra $default(svra)
      set svdec $default(svdec)
      if { $default(svequinox) eq "" } {
         coord ra=$svra dec=$svdec
      } else {
         coord ra=$svra dec=$svdec equinox=$default(svequinox)
      }
      set raprompt  [format "R.A. (%d d/f= %12s or %7.3f):" \
                    $default(equinox) $coord(xsfmt) $coord(ra)]
      set ra [txread $raprompt]
      if { $ra eq "" } { set ra $coord(ra) }
      set decprompt [format "Dec  (%d d/f=%12s  or %7.3f):" \
                    $default(equinox) $coord(ysfmt) $coord(dec)]
      set dec [txread $decprompt]
      if { $dec eq "" } { set dec $coord(dec) }
   }
   txwrite " Input RA : $ra" 20
   txwrite " Input Dec: $dec" 20
   set default(svra) $ra
   set default(svdec) $dec
   set default(svequinox) $default(equinox)
#
#  Convert radius from arcsec to detector pixels using current image
#
   set radius 0
   if { $circle > 0 } {
      set zmx    [chh key=zmx]
      if { $zmx eq "" }    { set zmx 0 }
      set cdelt1 [chh key=cdelt1]
      if { $cdelt1 eq "" } { set cdelt1 1 }
      if { $cdelt1 != 0 } {
         set radius [expr $circle*$zmx/(abs($cdelt1)*3600.)]
      }
   }
#
#  Calculate x/ypix
#
   coord ra=$ra dec=$dec
   if [isastbad $coord(xpix)] {
      set xpix "Undefined"
   } else {
      set xpix $coord(xpix)
   }
   if [isastbad $coord(ypix)] {
      set ypix "Undefined"
   } else {
      set ypix $coord(ypix)
   }
   txwrite " " 10
   txwrite " Pixel coordinates X = $xpix Y = $ypix" 10
   txwrite " " 10

   if [isastbad $coord(ximg)] {
      set ximg "Undefined"
   } else {
      set ximg $coord(ximg)
   }
   if [isastbad $coord(yimg)] {
      set yimg "Undefined"
   } else {
      set yimg $coord(yimg)
   }
   txwrite " Image coordinates X = $ximg Y = $yimg" 20
   txwrite " " 20
#
#  Plot point or circle
#
   if { $dismap ne "" && $dismap ne $curmap } {

      txwrite " Position not plotted: Current map is not display map" 10

   } else {

      if { $radius > 0 } {
         set drawArgs [list circle $coord(xpix) $coord(ypix) $radius]
         if { $color ne "" }  { lappend drawArgs color=$color }
         if { $lwidth ne "" } { lappend drawArgs lwidth=$lwidth }
         if { $lstyle ne "" } { lappend drawArgs lstyle=$lstyle }
         txwrite "draw $drawArgs" 30
         eval draw $drawArgs
      } else {
         set labelArgs [list xpix=$coord(xpix) ypix=$coord(ypix)]
         if { $symbol ne "" } { lappend labelArgs symbol=$symbol }
         if { $csize ne "" }  { lappend labelArgs csize=$csize }
         if { $color ne "" }  { lappend labelArgs color=$color }
         if { $lwidth ne "" } { lappend labelArgs lwidth=$lwidth }
         txwrite "label $labelArgs" 30
         eval label $labelArgs
      }
   }
}
