itcl::class FVSkyview {
    constructor {args} {}
    destructor {}

    private variable skyvID
    private variable skyvCoord
    private variable skyvCoordSys
    private variable skyvEquinox
    private variable _skyvImgSizeMinutes
    private variable skyvImgSizex
    private variable skyvImgSizey
    private variable skyvCatalogFlag
    private variable skyvPlotFlag
    private variable skyviewProcess

    private variable skyvcatID
    private variable catCoord
    private variable catCoordSys
    private variable catEquinox
    private variable _catImgSizeMinutes
    private variable catOverlayFlag
    private variable catProcess

    public method setskyvCoordSys {}
    public method getskyvcat {}
    public method getskyvfile {} 
    private method skyviewOK {}
    private method skyviewReset {}
    private method catOK {}
    private method catReset {}
    private method configCatalog {}
    private method _encodeString { coord }
    private variable skyvCatalogDesc [list "HST Guide Star Catalog, Version 2.2" \
                                           "Smithsonian Astrophysical Observatory Star Catalog" \
                                           "Bright Star Catalog" \
                                           "Hipparcos Main Catalog" \
                                           "TYCHO-2 Catalog of the 2.5 Million Brightest Stars" \
                                           "Master Optical Catalog" \
                                           "Master X-Ray Catalog" \
                                           "Master Radio Catalog" \
                                           "NGC 2000.0 Catalog" \
                                           "Messier Nebulae" \
                                           "Extraglactic Radio Sources" \
                                           "Gliese Catalog of Nearby Stars, 3rd Edition" \
                                           "Henry Draper (HD) Catalog" \
                                           "CfA Redshift Catalog (June 1995 Version)" \
                                           "IRAS Faint Sources" \
                                           "IRAS Point Sources" \
                                           "Markarian Galaxies" \
                                           "ROSAT All-Sky Survey: Bright Sources" \
                                           "ROSAT All-Sky Survey: Faint Sources" \
                                           "ROSAT Catalog PSPC RX MPE Sources" \
                                           "ROSAT Catalog PSPC WGA Sources" \
                                           "Third Reference Catalog of Galaxies" \
                                           "USNO A-2 (Monet) Catalog" \
                                           "Uppsala General Catalog of Galaxies" \
                                           "Veron Quasars & AGNs (V2001)" \
                                           "Zwicky Clusters" ]

    private variable skyvCatalogTableName [ list "I/271" \
                                                 "heasarc_sao" \
                                                 "heasarc_bsc5p" \
                                                 "heasarc_hipparcos" \
                                                 "heasarc_tycho2" \
                                                 "heasarc_optical" \
                                                 "heasarc_xray" \
                                                 "heasarc_radio" \
                                                 "heasarc_ngc2000" \
                                                 "heasarc_messier" \
                                                 "heasarc_kuehr" \
                                                 "heasarc_cns3" \
                                                 "heasarc_hd" \
                                                 "heasarc_zcat" \
                                                 "heasarc_irasfsc" \
                                                 "heasarc_iraspsc" \
                                                 "heasarc_markarian" \
                                                 "heasarc_rassbsc" \
                                                 "heasarc_rassfsc" \
                                                 "heasarc_rosatsrc" \
                                                 "heasarc_wgacat" \
                                                 "heasarc_rc3" \
                                                 "I/252" \
                                                 "heasarc_ugc" \
                                                 "heasarc_veron2001" \
                                                 "heasarc_zwclusters" ]

    private variable skyvSurveyName [ list { "DSS Original (optical)" "DSS" } \
                                           { "DSS1, Red" "DSS1R" } \
                                           { "DSS1, Blue" "DSS1B" } \
                                           { "DSS2, Red" "DSS2R" } \
                                           { "DSS2, Blue" "DSS2B" } \
                                           { "DSS2, IR" "DSS2IR" } \
                                           { "Sloan DSS U-band" "SDSSu" } \
                                           { "Sloan DSS G-band" "SDSSg" } \
                                           { "Sloan DSS R-band" "SDSSr" } \
                                           { "Sloan DSS I-band" "SDSSi" } \
                                           { "Sloan DSS Z-band" "SDSSz" } \
                                           { "2MASS-J (Infrared 1.25 micron)" "2MASS-J" } \
                                           { "2MASS-H (Infrared 1.65)" "2MASS-H" } \
                                           { "2MASS-K (Infrared 2.17)" "2MASS-K" } \
                                           { "Greenbank (6 cm Radio)" "GB6" } \
                                           { "HI All-Sky Continuum Survey (408 MHz)" "408MHz" } \
                                           { "VLA First (21 cm Radio)" "FIRST" } \
                                           { "Westerbork N. Sky Survey (92 cm Radio)" "wenss" } \
                                           { "NVSS NRAO VLA Sky Survey  (1.4 GHz)" "NVSS" } \
                                           { "IRIS 12 micron (Infrared)" "IRIS12" } \
                                           { "IRIS 25 micron (Infrared)" "IRIS25" } \
                                           { "IRIS 60 micron (Infrared)" "IRIS60" } \
                                           { "IRIS 100 micron (Infrared)" "IRIS100" } \
                                           { "EUVE 83 Angstrom" "euve83" } \
                                           { "EUVE 171 Angstrom" "euve171" } \
                                           { "EUVE 405 Angstrom" "euve405" } \
                                           { "EUVE 555 Angstrom" "euve555" } \
                                           { "ROSAT Wide Field Camera (100 Angstroms)" "wfcf1" } \
                                           { "ROSAT Wide Field Camera (130 Angstroms)" "wfcf2" } \
                                           { "ROSAT PSPC 2.0 Deg (X-ray)" "PSPC2int" } \
                                           { "ROSAT All Sky Broad Band (0.1 - 2.4 keV X-ray)" "RASS+broad" } \
                                           { "ROSAT All Sky Hard Band (0.5 - 2.0 keV X-ray)" "RASS+Hard" } \
                                           { "ROSAT All Sky Soft Band (0.1 - 0.4 keV X-ray)" "RASS+Soft" } \
                                           { "EGRET >100 MeV (Gamma-ray)" "EGREThard" } \
                                           { "EGRET <100 MeV (Gamma-ray)" "EGRETsoft" } ]

} 

itcl::body FVSkyview::constructor {args} {
    set skyvID 0
    set skyvCoord  ""
    set skyvCoordSys "Equatorial"
    set skyvEquinox "2000"
    set _skyvImgSizeMinutes "Default"
    set skyvImgSizex "300"
    set skyvImgSizey "300"
    set skyvCatalogFlag 0
    set skyvPlotFlag 1

    set skyvcatID 0     


    set catCoord  ""
    set catCoordSys "Equatorial"
    set _catImgSizeMinutes ""
    set catEquinox "2000"
    set catOverlayFlag 0

}

itcl::body FVSkyview::destructor {} {   
}


itcl::body FVSkyview::getskyvfile {} {

    global g_titleFont

    if ![info exist skyvID] {
       set skyvID 1
    }

 
    if ![info exist skyvEquinox ] {
       set skyvEquinox "2000"
    }

    if ![info exist _skyvImgSizeMinutes ] {
       set _skyvImgSizeMinutes "Default"
    }


    if ![info exist skyvImgSizex ] {
       set skyvImgSizex "300"
    }

    if ![info exist skyvImgSizey ] {
       set skyvImgSizey "300"
    }

    if ![info exist skyvCatalogFlag ] {
       set skyvCatalogFlag 0
    } 
    
    if [winfo exist .skyviewdlg] {
       focus .skyviewdlg
       raise .skyviewdlg
       return
    }

    toplevel .skyviewdlg -class Dialog
    wm title .skyviewdlg "fv: SkyView"

    label .skyviewdlg.title -anchor w \
       -text "Retrieve an image from NASA's SkyView site." -font g_titleFont 
    pack  .skyviewdlg.title -pady 3 -padx 5 -anchor w

    frame .skyviewdlg.coord -relief flat
    label .skyviewdlg.coord.label -text "Coordinate or Source:" \
       -font g_titleFont -anchor w
    entry .skyviewdlg.coord.entry -width 40 \
            -textvariable [itcl::scope skyvCoord] -relief sunken   -font g_titleFont
    pack  .skyviewdlg.coord -pady 3 -padx 5 -anchor w
    pack  .skyviewdlg.coord.label .skyviewdlg.coord.entry -side left
    
    frame .skyviewdlg.note -relief flat
    label .skyviewdlg.note.label -text \
  "(e.g. \"Eta Carinae\", \"10 45 3.6, -59 41 4.2\", or \"161.265, -59.685\")" \
       -font g_titleFont -anchor w 
    pack  .skyviewdlg.note -pady 3 -padx 5 -anchor w
    pack  .skyviewdlg.note.label -side left
     
    frame .skyviewdlg.surveytext  -relief flat
    label .skyviewdlg.surveytext.label -text "Choose desired survey:" \
        -font g_titleFont -anchor w
    pack  .skyviewdlg.surveytext -pady 3 -padx 5 -anchor w
    pack  .skyviewdlg.surveytext.label -anchor w -side top 

    frame .skyviewdlg.survey  -relief flat
    listbox .skyviewdlg.survey.list  -exportselection 0 -takefocus 0 \
         -height 5 -width 40 -font g_titleFont \
         -yscrollcommand ".skyviewdlg.survey.scroll set"
    scrollbar .skyviewdlg.survey.scroll \
         -command ".skyviewdlg.survey.list yview"   
    foreach token $skyvSurveyName {
        .skyviewdlg.survey.list insert end [lindex $token 0]
    }
    .skyviewdlg.survey.list selection set 0 
    pack  .skyviewdlg.survey -pady 5 -padx 5 -anchor w
    pack .skyviewdlg.survey.list -side left 
    pack .skyviewdlg.survey.scroll -side right -fill y

    frame .skyviewdlg.coordsys -relief flat
    frame .skyviewdlg.coordsys.coord -relief flat
    set opt .skyviewdlg.coordsys.coord
    iwidgets::optionmenu $opt.sys -labeltext "System:" \
        -font g_titleFont -labelfont g_titleFont \
        -command {set skyvCoordSys [.skyviewdlg.coordsys.coord.sys get] }
    $opt.sys insert end "Equatorial"
    $opt.sys insert end "Galactic"
    $opt.sys insert end "Ecliptic"
    $opt.sys select 0

    frame $opt.equinox -relief flat
    label $opt.equinox.label -text "Equinox:  " \
          -font g_titleFont -anchor w
    entry $opt.equinox.entry -width 10 \
            -textvariable [itcl::scope skyvEquinox] -relief sunken   -font g_titleFont
    pack  $opt.equinox.label \
          $opt.equinox.entry  -side left -anchor w

    pack  $opt.sys -anchor w -side top 
    pack  $opt.equinox -anchor w -side top 

    frame .skyviewdlg.coordsys.options -relief flat
    frame .skyviewdlg.coordsys.options.imgsized -relief flat
    label .skyviewdlg.coordsys.options.imgsized.label \
            -text "Image width (minutes):" \
          -font g_titleFont -anchor w
    entry .skyviewdlg.coordsys.options.imgsized.entry -width 10 \
            -textvariable [itcl::scope _skyvImgSizeMinutes] -relief sunken   -font g_titleFont
    pack  .skyviewdlg.coordsys.options.imgsized.label \
          .skyviewdlg.coordsys.options.imgsized.entry \
           -side left -anchor w
    

    frame .skyviewdlg.coordsys.options.imgsizep -relief flat
    label .skyviewdlg.coordsys.options.imgsizep.label \
            -text "Image dimensions(Pixels):" \
          -font g_titleFont -anchor w
    label .skyviewdlg.coordsys.options.imgsizep.labelx \
            -text "X:" \
          -font g_titleFont -anchor w
    entry .skyviewdlg.coordsys.options.imgsizep.entryx -width 5 \
            -textvariable [itcl::scope skyvImgSizex] -relief sunken   -font g_titleFont
    label .skyviewdlg.coordsys.options.imgsizep.labely \
            -text "Y:" \
          -font g_titleFont -anchor w
    entry .skyviewdlg.coordsys.options.imgsizep.entryy -width 5 \
            -textvariable [itcl::scope skyvImgSizey] -relief sunken   -font g_titleFont
    pack  .skyviewdlg.coordsys.options.imgsizep.label  \
          .skyviewdlg.coordsys.options.imgsizep.labelx \
          .skyviewdlg.coordsys.options.imgsizep.entryx \
          .skyviewdlg.coordsys.options.imgsizep.labely \
          .skyviewdlg.coordsys.options.imgsizep.entryy \
           -side left -anchor w

    pack .skyviewdlg.coordsys.options.imgsized \
         .skyviewdlg.coordsys.options.imgsizep \
         -pady 5 -padx 5 -anchor w

    pack  .skyviewdlg.coordsys.coord -pady 3 -padx 5 -anchor w -side left
    pack  .skyviewdlg.coordsys.options -pady 3 -padx 5 -anchor n -side left
    pack  .skyviewdlg.coordsys -pady 3 -padx 5 -anchor w 

    frame .skyviewdlg.choices  -relief flat
#    checkbutton .skyviewdlg.choices.cat \
#         -text "Retrieve catalog(s) of objects within image?" \
#        -font g_titleFont -anchor w -onvalue 1  -offvalue 0 \
#        -variable [itcl::scope skyvCatalogFlag ] \
#        -command [itcl::code $this configCatalog]
#        
#    pack  .skyviewdlg.choices -pady 3 -padx 5 -anchor w
#    pack  .skyviewdlg.choices.cat -anchor w -side top 


#    frame .skyviewdlg.catalog  -relief flat
#    listbox .skyviewdlg.catalog.list  -exportselection 0 -takefocus 0 \
#         -height 5 -width 50 -font g_titleFont -selectmode multiple \
#         -yscrollcommand ".skyviewdlg.catalog.scroll set"
#    scrollbar .skyviewdlg.catalog.scroll \
#         -command ".skyviewdlg.catalog.list yview"   
#    pack  .skyviewdlg.catalog -pady 3 -padx 5 -anchor w
#    pack .skyviewdlg.catalog.list -side left 
#    pack .skyviewdlg.catalog.scroll -side right -fill y
#    configCatalog

    frame .skyviewdlg.plotchoice  -relief flat
    checkbutton .skyviewdlg.plotchoice.plot \
        -text "Display image after retrieving?" \
        -font g_titleFont -anchor w -onvalue 1  -offvalue 0 \
        -variable [itcl::scope skyvPlotFlag ]
    pack  .skyviewdlg.plotchoice -pady 3 -padx 5 -anchor w
    pack  .skyviewdlg.plotchoice.plot -anchor w -side top 

    frame .skyviewdlg.skyviewbt -relief flat
    button .skyviewdlg.skyviewbt.submit -text Submit  -width 8 \
           -font g_titleFont \
           -command [itcl::code $this skyviewOK]
    button .skyviewdlg.skyviewbt.reset -text Reset -width 8 \
           -font g_titleFont \
        -command [itcl::code $this skyviewReset]  
    button .skyviewdlg.skyviewbt.cancel -text Close -width 8 \
           -font g_titleFont \
        -command {destroy .skyviewdlg}
    button .skyviewdlg.skyviewbt.help -text Help -width 8 \
           -font g_titleFont \
        -command {hhelp SkyView}
    pack .skyviewdlg.skyviewbt.submit -side left   -padx 10 
    pack .skyviewdlg.skyviewbt.reset  -side left  -padx 10
    pack .skyviewdlg.skyviewbt.cancel -side left  -padx 10
    pack .skyviewdlg.skyviewbt.help   -side left -padx 10
    pack .skyviewdlg.skyviewbt -side top -pady 5 -padx 5 -expand true 

    bind .skyviewdlg <Key-Return> [itcl::code $this skyviewOK] 
    focus .skyviewdlg.coord.entry
#    tkwait window .skyviewdlg 
}    

itcl::body FVSkyview::configCatalog {} { 
   if {$skyvCatalogFlag == 1} { 
      foreach descLine $skyvCatalogDesc {
              .skyviewdlg.catalog.list insert end $descLine
      }

   } else {
     .skyviewdlg.catalog.list delete 0 end 
   } 
}

itcl::body FVSkyview::skyviewOK {} {
    global g_fitsFileMode
    global g_backupDir

    package require http
    
    if [info exist skyviewProcess ] {
        return 
    }

    set skyviewProcess 1

    # set skyviewfile [file join $g_backupDir skyview.tmp$skyvID]
    set skyviewfile [file join $g_backupDir skyview.tmp_[clock seconds]]
    if [file exists $skyviewfile] {
          file delete $skyviewfile
    }

    set skyvsurvey [lindex [lindex $skyvSurveyName [.skyviewdlg.survey.list curselection]] 1]

    if {$skyvCoord == "" } { 
       tk_dialog .skverr "Skyview Error"  \
                        "Source is not entered." error 0 Ok
       unset skyviewProcess
       return
    }


    set skyviewout [open $skyviewfile w]
    if { $_skyvImgSizeMinutes == "Default" } {
	set imgDegrees "Default"
    } else {
	set imgDegrees [expr $_skyvImgSizeMinutes * 0.0166667]
    }
    set skyvCoordSys [.skyviewdlg.coordsys.coord.sys get]
    set  cmd [list ::http::formatQuery \
              VCOORD $skyvCoord   \
              SURVEY $skyvsurvey   \
              SCOORD $skyvCoordSys \
              SFACTR $imgDegrees \
              PIXELX $skyvImgSizex \
              PIXELY $skyvImgSizey ] 

    if {$skyvCatalogFlag == 1} {
          set catalogs [.skyviewdlg.catalog.list curselection]  
          set l [llength $catalogs]
          if {$l == 0} {
             tk_dialog .skverr "Skyview Error"  \
               "The catalogs are not selected." error 0 Ok
             unset skyviewProcess
             return
          }
          set catstr ""
          for {set i 0} {$i < $l} {incr i} { 
             set j [lindex $catalogs $i]
             set skyvcatalog [.skyviewdlg.catalog.list get $j]
             lappend catstr CATLOG  
             lappend catstr $skyvcatalog
          }
          set cmd [concat $cmd $catstr]
     } 
     set skyviewqst [eval $cmd]

     .skyviewdlg.skyviewbt.submit configure -state disabled
     .skyviewdlg.skyviewbt.reset configure -state disabled
     .skyviewdlg.skyviewbt.cancel configure -state disabled
     
     #set token [::http::geturl "http://skyview.gsfc.nasa.gov/cgi-bin/pskcall" \
     #     -channel $skyviewout -query $skyviewqst  ] 
     set token [::http::geturl "http://skyview.gsfc.nasa.gov/cgi-bin/pskcall?$skyviewqst"]
     fconfigure $skyviewout -translation binary
     puts -nonewline $skyviewout [::http::data $token]

     close $skyviewout

     .skyviewdlg.skyviewbt.submit configure -state normal
     .skyviewdlg.skyviewbt.reset configure -state normal
     .skyviewdlg.skyviewbt.cancel configure -state normal

     set temptest [open $skyviewfile r ]  
     set line [read $temptest 6]
     close $temptest
     if {$line != "SIMPLE" } {  
       set temptest [open $skyviewfile r ]  
       set errstr ""
       set errorflag 0
       while {[gets $temptest line] >= 0} {
          if [regexp {^<P>} $line] {
             set  errorflag 0
          }  
          if [regexp {^ERROR:} $line] {
             set  errorflag 1
          }  
          if {$errorflag == 1} {
             set errstr "${errstr}${line}"
          }
       } 
       close $temptest
       if {$errstr == ""} {
            tk_dialog .skvmsg "Skyview Message"  \
     "No image available at this sky position in the selected survey."  \
                        info 0 Ok
            unset skyviewProcess
            return 
       }	     
       tk_dialog .skverr "Skyview Error"  \
                        $errstr  error 0 Ok
       unset skyviewProcess
       return
     } 

        
     set tmp [fits open $skyviewfile 0] 
     set hdunum [$tmp info nhdu]
     $tmp close 
 
     set oldMode $g_fitsFileMode
     # Set Read-Only flag
     set g_fitsFileMode 1

     set previousIfAutoPlotPrimary $fvPref::ifAutoPlotPrimary
     if { $previousIfAutoPlotPrimary == 0 } {
        if { $skyvPlotFlag == 1} {  
           set fvPref::ifAutoPlotPrimary 1
        }
     }
     set tmp [openFitsFile $skyviewfile]
     if { $skyvCatalogFlag == 1} {
       if { $hdunum == 2} {
        fvCmds::display curve 1 RA {} DEC {} 1 
       }  
       if {$hdunum < 1 } {
         tk_dialog .skvmsg "Skyview Message"  \
                "No desired objects are found in the selected catalog(s)"  \
                     info 0 Ok
       }
     }
     set g_fitsFileMode 1
     catch { $tmp changeFile } err

     unset skyviewProcess

     set fvPref::ifAutoPlotPrimary $previousIfAutoPlotPrimary
}

itcl::body FVSkyview::skyviewReset {} {
      set skyvCoord  ""
      set skyvCoordSys "Equatorial"
      set skyvEquinox "2000"
      set _skyvImgSizeMinutes "Default"
      set skyvImgSizex "300"
      set skyvImgSizey "300"
      set skyvCatalogFlag 0
      set skyvPlotFlag 1
      .skyviewdlg.survey.list selection clear 0 end
      .skyviewdlg.survey.list selection set 0 
      
      # put configCatalog under itcl::code space
      set result [itcl::code $this configCatalog]
}

itcl::body FVSkyview::getskyvcat {} {

    global g_titleFont


    
    if ![info exist skyvcatID] {
       set skyvcatID 1
    }

    if ![info exist catEquinox ] {
       set catEquinox "2000"
    }

    if ![info exist catOverlagFlag ] {
       set catOverlayFlag 0
    }

    if [winfo exist .skyvcatdlg] {
       focus .skyvcatdlg
       raise .skyvcatdlg
       return
    }

    toplevel .skyvcatdlg -class Dialog
    wm title .skyvcatdlg "fv: Catalog"

    label .skyvcatdlg.title -anchor w \
       -text "Retrieve catalog of objects from NASA's SkyView site." \
       -font g_titleFont 
    pack  .skyvcatdlg.title -pady 3 -padx 5 -anchor w

    frame .skyvcatdlg.coord -relief flat
    label .skyvcatdlg.coord.label -text "Coordinate or Source:" \
       -font g_titleFont -anchor w
    entry .skyvcatdlg.coord.entry -width 40 \
            -textvariable [itcl::scope catCoord] -relief sunken   -font g_titleFont
    pack  .skyvcatdlg.coord -pady 5 -padx 5 -anchor w
    pack  .skyvcatdlg.coord.label .skyvcatdlg.coord.entry -side left
    
    frame .skyvcatdlg.note -relief flat
    label .skyvcatdlg.note.label -text \
  "(e.g. \"Eta Carinae\", \"10 45 3.6, -59 41 4.2\", or \"161.265, -59.685\")" \
       -font g_titleFont -anchor w 
    pack  .skyvcatdlg.note -pady 5 -padx 5 -anchor w
    pack  .skyvcatdlg.note.label -side left
     

    frame .skyvcatdlg.radius -relief flat
    label .skyvcatdlg.radius.label -text "Search radius (minutes):" \
          -font g_titleFont -anchor w
    entry .skyvcatdlg.radius.entry -width 10 \
            -textvariable [itcl::scope _catImgSizeMinutes] -relief sunken   -font g_titleFont
    pack  .skyvcatdlg.radius -pady 5 -padx 5 -anchor w
    pack  .skyvcatdlg.radius.label .skyvcatdlg.radius.entry \
           -side left -anchor w

    frame .skyvcatdlg.catalogtext  -relief flat
    label .skyvcatdlg.catalogtext.label -text "Choose desired catalog(s)" \
        -font g_titleFont -anchor w 
    pack  .skyvcatdlg.catalogtext -pady 5 -padx 5 -anchor w
    pack  .skyvcatdlg.catalogtext.label -anchor w -side top 


    frame .skyvcatdlg.catalog  -relief flat
    listbox .skyvcatdlg.catalog.list  -exportselection 0 -takefocus 0 \
         -height 5 -width 50 -font g_titleFont -selectmode multiple \
         -yscrollcommand ".skyvcatdlg.catalog.scroll set"
    scrollbar .skyvcatdlg.catalog.scroll \
         -command ".skyvcatdlg.catalog.list yview"   
    foreach descLine $skyvCatalogDesc {
            .skyvcatdlg.catalog.list insert end $descLine
    }
    # .skyvcatdlg.catalog.list selection set 0 
    pack  .skyvcatdlg.catalog -pady 5 -padx 5 -anchor w
    pack .skyvcatdlg.catalog.list -side left 
    pack .skyvcatdlg.catalog.scroll -side right -fill y

    frame .skyvcatdlg.coordsys -relief flat
    frame .skyvcatdlg.coordsys.coord -relief flat
    label .skyvcatdlg.coordsys.coord.label -text "Cooridinate System:" \
           -font g_titleFont -anchor w 
    radiobutton .skyvcatdlg.coordsys.coord.equatorial  -text "Equatorial" \
          -font g_titleFont -variable [itcl::scope catCoordSys] -value "Equatorial"
    radiobutton .skyvcatdlg.coordsys.coord.galactic  -text "Galactic" \
          -font g_titleFont -variable [itcl::scope catCoordSys] -value "Galactic"
    radiobutton .skyvcatdlg.coordsys.coord.ecliptic  -text "Ecliptic" \
          -font g_titleFont -variable [itcl::scope catCoordSys] -value "Ecliptic"
    .skyvcatdlg.coordsys.coord.equatorial select
    pack  .skyvcatdlg.coordsys.coord.label -anchor w -side top 
    pack .skyvcatdlg.coordsys.coord.equatorial -anchor w -side top 
    pack .skyvcatdlg.coordsys.coord.galactic -anchor w -side top 
    pack .skyvcatdlg.coordsys.coord.ecliptic -anchor w -side top 

    frame .skyvcatdlg.coordsys.options -relief flat
    label .skyvcatdlg.coordsys.options.label -text "Equinox:" \
          -font g_titleFont -anchor w
    entry .skyvcatdlg.coordsys.options.entry -width 10 \
            -textvariable [itcl::scope catEquinox] -relief sunken   -font g_titleFont
    checkbutton .skyvcatdlg.coordsys.options.checkbox \
        -text "Plot on the current graph? " \
        -font g_titleFont -anchor w -onvalue 1  -offvalue 0 \
        -variable [itcl::scope catOverlayFlag ]
    pack  .skyvcatdlg.coordsys.options.label -anchor w -side top
    pack  .skyvcatdlg.coordsys.options.entry -anchor w -side top
    pack  .skyvcatdlg.coordsys.options.checkbox -anchor w -side bottom

    pack  .skyvcatdlg.coordsys.coord -pady 5 -padx 5 -anchor w -side left
    pack  .skyvcatdlg.coordsys.options -pady 5 -padx 5 -anchor n -side left \
           -fill y
    pack  .skyvcatdlg.coordsys -pady 5 -padx 5 -anchor w 


    frame .skyvcatdlg.skyvcatbt -relief flat
    button .skyvcatdlg.skyvcatbt.submit -text Submit  -width 8 \
           -font g_titleFont \
           -command [itcl::code $this catOK ]
    button .skyvcatdlg.skyvcatbt.reset -text Reset -width 8 \
           -font g_titleFont \
        -command  [itcl::code $this catReset ]
    button .skyvcatdlg.skyvcatbt.cancel -text Close -width 8 \
           -font g_titleFont \
        -command {destroy .skyvcatdlg}
    button .skyvcatdlg.skyvcatbt.help -text Help -width 8 \
           -font g_titleFont \
        -command {hhelp catalog}
    pack .skyvcatdlg.skyvcatbt.submit -side left   -padx 10 
    pack .skyvcatdlg.skyvcatbt.reset  -side left  -padx 10
    pack .skyvcatdlg.skyvcatbt.cancel -side left  -padx 10
    pack .skyvcatdlg.skyvcatbt.help   -side left -padx 10
    pack .skyvcatdlg.skyvcatbt -side top -pady 5 -padx 5 -expand true 

    bind .skyvcatdlg.coord.entry <Key-Return> \
        {focus  .skyvcatdlg.radius.entry } 
    bind .skyvcatdlg.radius.entry <Key-Return> [itcl::code $this catOK]   
    focus .skyvcatdlg.coord.entry  
#    tkwait window .skyvcatdlg
}    

itcl::body FVSkyview::catOK {} {
    global g_fitsFileMode
    global g_backupDir
    package require http

    if [info exist catProcess ] {
        return 
    }

    set catProcess 1

    set skyvcatfile [file join $g_backupDir skyvcat.tmp_[clock seconds]]
    if [file exists $skyvcatfile] {
          file delete $skyvcatfile
    }

    set skyvcatout [open $skyvcatfile w]
    set skyvcattemp ""

    if {$catCoord == "" } { 
       tk_dialog .skverr "Catalog Error"  \
                        "Source is not entered." error 0 Ok
       unset catProcess
       return
    }
    if {$_catImgSizeMinutes == ""} {
       set _catImgSizeMinutes "default"
    }

    # set catDegrees [expr $_catImgSizeMinutes * 0.0166667]
    set catDegrees $_catImgSizeMinutes
    set catalogs [.skyvcatdlg.catalog.list curselection]

    set l [llength $catalogs]
    if {$l == 0} {
       tk_dialog .skverr "Catalog Error"  \
           "The catalogs are not selected." error 0 Ok
       unset catProcess
       return
    }

    # Entry 
    set skyvcatqst [format "%s=%s&" Entry [_encodeString $catCoord]]

    # Coordinates
    set skyvcatqst [format "%s%s=%s&" $skyvcatqst Coordinates $catCoordSys]

    # Equinox
    set skyvcatqst [format "%s%s=%s&" $skyvcatqst Equinox $catEquinox]

    # Radius
    set skyvcatqst [format "%s%s=%s&" $skyvcatqst Radius $catDegrees]

    # ResultMax
    set skyvcatqst [format "%s%s=%s&" $skyvcatqst ResultMax 0]

    # Displaymode
    set skyvcatqst [format "%s%s=%s&" $skyvcatqst displaymode FitsDisplay]

    # tablehead
    foreach i $catalogs {
        set skyvcatqst [format "%s%s=%s3D%s&" $skyvcatqst tablehead \
                               "name%" [_encodeString [lindex $skyvCatalogTableName $i]] ]
    }

    .skyvcatdlg.skyvcatbt.submit configure -state disabled
    .skyvcatdlg.skyvcatbt.reset configure -state disabled
    .skyvcatdlg.skyvcatbt.cancel configure -state disabled
    
    #set token [::http::geturl "http://heasarc.gsfc.nasa.gov/cgi-bin/W3Browse/w3query.pl" \
    #                          -channel $skyvcatout -query $skyvcatqst ]

    set token [::http::geturl "http://heasarc.gsfc.nasa.gov/cgi-bin/W3Browse/w3query.pl?$skyvcatqst"]
    fconfigure $skyvcatout -translation binary
    puts -nonewline $skyvcatout [::http::data $token]

    close $skyvcatout

    .skyvcatdlg.skyvcatbt.submit configure -state normal
    .skyvcatdlg.skyvcatbt.reset configure -state normal
    .skyvcatdlg.skyvcatbt.cancel configure -state normal
    

     set temptest [open $skyvcatfile r ]
     set line [read $temptest 6]
     close $temptest
     if {$line != "SIMPLE" } {
       set temptest [open $skyvcatfile r ]
       set errstr ""
       set errorflag 0
       while {[gets $temptest line] >= 0} {
          if [regexp {^<P>} $line] {
             set  errorflag 0
          }
          if [regexp {^ERROR:} $line] {
             set  errorflag 1
          } 
          if {$errorflag == 1} {
             set errstr "${errstr}${line}"
          }
       }
       close $temptest
       if {$errstr == ""} {
            tk_dialog .skvmsg "Catalog Message"  \
                        "No desired objects are found in selected catalog(s)"  \
                        info 0 Ok
            unset catProcess
            return
       }
       tk_dialog .skverr "Catalog Error"  \
                        $errstr  error 0 Ok
       unset catProcess
       return
     }
    set oldMode $g_fitsFileMode

    set tmp [fits open $skyvcatfile 0]
    set hdunum [$tmp info nhdu]
    $tmp close

    # Set Read-Only flag
    set g_fitsFileMode 1
    set tmp [openFitsFile $skyvcatfile]
    $tmp changeFile
    set g_fitsFileMode $oldMode
    incr skyvcatID

    # Plot on the current graph
    if { $catOverlayFlag == 1} {
       # fvCmds::select $skyvcatfile
        if { $hdunum >= 2} {
           #$tmp plotData1 2 
           set errorFlag [ catch { 
               fvCmds::display curve 1 RAJ2000_ {} DEJ2000_ {} 2 
           } err ]

           if { $errorFlag } {
              set errorFlag [ catch { 
                  fvCmds::display curve 1 RA {} DEC {} 2 
              } err ]
           }
           
           if { $errorFlag } {
              tk_messageBox -icon error -type ok \
                            -message "Keyword RAJ2000_, DEJ2000_, RA, and DEC do not exists.\nPlease choose columns to plot."
              $tmp plotData1 2 
           }
           set g_fitsFileMode 1
        } 
    }
    unset catProcess

}

itcl::body FVSkyview::catReset {} {
          set catCoord  ""
          set _catImgSizeMinutes ""
          set catEquinox "2000"
          set catOverlayFlag 0
    .skyvcatdlg.catalog.list selection clear 0 end
}


itcl::body FVSkyview::_encodeString { coord } {
     regsub -all {,} $coord {;} coord
     regsub -all {"} $coord {} coord

     set token [split $coord ";"]
     set coord ""

     for {set i 0} {$i < [llength $token]} {incr i} {
         if { $i > 0 } {
            set coord [format "%s;" $coord] 
         }

         set coord [format "%s%s" $coord [string trim [lindex $token $i]]] 
     }

     # What this function does is hex encode any char that isn't a-z, A-Z, 0-9, an
     # underscore, a period, or a dash.  

     # the following regsub is to mask all the character that fits the above description
     # anything else left will be the one to encode

     set totalMasked 0

     set totalMasked [expr $totalMasked + [regsub -all {[a-z]} $coord {#} result]]
     set totalMasked [expr $totalMasked + [regsub -all {[A-Z]} $result {#} result]]
     set totalMasked [expr $totalMasked + [regsub -all {[0-9]} $result {#} result]]
     set totalMasked [expr $totalMasked + [regsub -all {_} $result {#} result]]
     set totalMasked [expr $totalMasked + [regsub -all {\.} $result {#} result]]
     set totalMasked [expr $totalMasked + [regsub -all {\-} $result {#} result]]

     if { $totalMasked == [string length $coord] } {
        # no encoding need
        return $coord
     }

     set newValue ""
     for {set i 0} {$i < [string length $coord]} {incr i} {
         set currentChar [string range $coord $i $i]
         set matchChar [string range $result $i $i]

         if { $matchChar != "#" && $matchChar != ";"} {
            binary scan $currentChar c value
            set newValue [format "%s%%%x" $newValue $value]
         } else {
            set newValue [format "%s%s" $newValue $currentChar]
         }
     }

     return $newValue
}
