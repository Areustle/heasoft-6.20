#
#  Query SKYVIEW for image of given sky survey using the
#  position and size of the current image.
#
rename skyview oskyview
proc skyview {args} {
   parseparm [parmloc skyview] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }

   set mapid CUR
   set file $parval(file)
   set survey $parval(survey_name)
   set list_surveys $parval(list_surveys)
   set size $parval(size)
   set fov $parval(fov)
   set ra $parval(ra)
   set dec $parval(dec)
#
#  Configure
#
   set webquery "xwebquery.pl"
   set skvbatch "skvbatch.pl"
   set host "skys.gsfc.nasa.gov"
   set list_url  "/cgi-bin/showsurveys.pl"

#
#  List surveys by spawning webquery
#
   if { $list_surveys } {
      set cmd "syscall $webquery host=$host url=$list_url"
      txwrite $cmd 20
      txwrite " Skyview surveys will be listed below" 10
      eval $cmd
      return
   }

   set useimg 1
   if { $fov != "" && $ra != "" && $dec != "" } { set useimg 0 }
   if { $fov != "" || $ra != "" || $dec != "" } {
      if { $useimg } {
         txwrite " WARNING: Center and FOV not fully specified" 10
         txwrite "          Using current loaded image" 10
      }
   }
   if { $useimg && [chh map=$mapid key=LOADED] == 0 } {
      txwrite "Image not loaded" 10
      error {}
   }
   txwrite "Input survey: $survey" 20

   if { $useimg } {
      wcs wcs=[chh key=wcsid]
      set scoord $wcs(system)
      if [regexp -nocase {Cartesian} $scoord] { 
         txwrite " No available sky coordinates" 10
         error {}
      }
      if { $wcs(xlab) eq "RA" } { set scoord "Equatorial" }
      set maproj $wcs(projection)

      set szx [chh map=$mapid key=szx]
      set szy [chh map=$mapid key=szy]
#
#  Silence chatter (primarily from offset)
#
      global tchat lchat
      set cleanup "chat $tchat $lchat"
      chat 5 5
#
#  Determine size of image in degrees
#
      coord ximg=0.5 yimg=[expr $szy/2.0 + 0.5]
      set x1pix $coord(xpix)
      set y1pix $coord(ypix)
      coord ximg=[expr $szx + 0.5] yimg=[expr $szy/2.0 + 0.5]
      set x2pix $coord(xpix)
      set y2pix $coord(ypix)
      offset x1pix=$x1pix y1pix=$y1pix x2pix=$x2pix y2pix=$y2pix
      set xfov $offset(value)

      coord ximg=[expr $szx/2.0 + 0.5] yimg=0.5
      set x1pix $coord(xpix)
      set y1pix $coord(ypix)
      coord ximg=[expr $szx/2.0 + 0.5] yimg=[expr $szy + 0.5]
      set x2pix $coord(xpix)
      set y2pix $coord(ypix)
      offset x1pix=$x1pix y1pix=$y1pix x2pix=$x2pix y2pix=$y2pix
      set yfov $offset(value)
  
      set sfactr $xfov
      if { [isastbad $sfactr] || ( ![isastbad $yfov] && $yfov > $sfactr ) } { 
         set sfactr $yfov 
      } 
#
#  Restore chat level
#
      eval $cleanup
#
#  Determine image center
#
      coord ximg=[expr $szx/2.0 + 0.5] yimg=[expr $szy/2.0 + 0.5]
      set vcoord [join [list $coord(xsky) $coord(ysky)] ","]
      set equinx $coord(equinox)

   } else {

      global default
      set scoord "Equatorial"
      set maproj "Gnomonic"
      set vcoord "$ra, $dec"
      set equinx $default(equinox)
      set sfactr $fov

   }
   
   set cmd [list "syscall" $skvbatch "VCOORD=$vcoord" \
                 "SURVEY=\"$survey\"" "MAPPROJ=$maproj" "EQUINX=$equinx" \
                 "SCOORD=$scoord" "PIXELX=$size" "PIXELY=$size" \
                 "file=$file"]
   if [isastbad $sfactr] {
      txwrite "WARNING: Field of view is undefined, using skyview default" 10
   } else {
      lappend cmd "SFACTR=$sfactr"
   }
   txwrite $cmd 15
#
# Spawn skvbatch
#
   txwrite "Querying Skyview server..." 10
   eval $cmd

   if [file exists $file] {
      txwrite " Skyview image written to file: $file" 10
      global tchat lchat
      set tmpmap [map gettmp]
      set cleanup "chat $tchat $lchat"
      chat 5 5
      set rdstat [catch {read_image map=$tmpmap $file}]
      map free $tmpmap
      eval $cleanup
      if { $rdstat } {
         txwrite " ERROR: SKYVIEW output file indicates a problem" 10
         txwrite " For example: incorrect survey name" 10
         if { !$useimg } {
            txwrite "              incorrect RA/Dec formatting" 10
            txwrite "              field of view too large" 10
         }
         error {}
      }
   } else {
      txwrite " ERROR: SKYVIEW failed to create FITS file" 10
      txwrite "        Server may be down or survey name is invalid" 10
      txwrite "        Check survey name against SKYVIEW/LIST" 10
      error {}
   }
}
