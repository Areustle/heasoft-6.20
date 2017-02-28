itcl::class FVVizier {
    constructor {args} {}
    destructor {}

    public method selectVizier {}
    private method _fetchVizier {}
# temporary defaults for testing this search out
    private variable _catalogID ""
    private variable _coneCenter ""
    private variable _searchRadius 10
    private variable _maxRows 1000
    private variable _selectionBoxID 2
    private variable _mirrorBoxID 0
    private variable _vizierFileID 0
} 

itcl::body FVVizier::constructor {args} {
    set _vizierFileID 0
}

itcl::body FVVizier::destructor {} {   
}

itcl::body FVVizier::selectVizier {} {
    if [winfo exist .vizier] {
       focus .vizier
       raise .vizier
       return
    }

    global g_titleFont

    toplevel .vizier -class Dialog
    wm title .vizier "fv: VizieR"

    label .vizier.title -anchor w \
       -text "Retrieve a table from VizieR." -font g_titleFont 
    pack  .vizier.title -pady 3 -padx 5 -anchor w

    # a frame with catalog on the left, mirror sites on the right
    # cm = catmirror

    frame .vizier.cm -relief flat

    iwidgets::entryfield .vizier.cm.catalog -labeltext "Catalog ID:" \
	    -width 17 \
            -labelfont g_titleFont \
            -textfont g_titleFont \
	    -textvariable [itcl::scope _catalogID]

    frame .vizier.cm.mirror  -relief flat
    frame .vizier.cm.mirror.text  -relief flat
    label .vizier.cm.mirror.text.label -text "Choose mirror site:" \
        -font g_titleFont -anchor w
    pack  .vizier.cm.mirror.text.label -anchor w -side top 
    frame .vizier.cm.mirror.sites -relief flat
    listbox .vizier.cm.mirror.sites.list -exportselection 0 -takefocus 0 \
         -height 8 -width 25 -font g_titleFont
    .vizier.cm.mirror.sites.list insert end "CDS (Strasbourg, France)"
    .vizier.cm.mirror.sites.list insert end "CfA (Harvard, USA)"
    .vizier.cm.mirror.sites.list insert end "ADAC (Tokyo, Japan)"
    .vizier.cm.mirror.sites.list insert end "CADC (Canada)"
    .vizier.cm.mirror.sites.list insert end "Cambridge (UK)"
    .vizier.cm.mirror.sites.list insert end "IUCAA (Pune, India)"
    .vizier.cm.mirror.sites.list insert end "Bejing (Bejing, China)"
    .vizier.cm.mirror.sites.list insert end "UKIRT (Hawaii, USA)"
    .vizier.cm.mirror.sites.list selection set $_mirrorBoxID
    pack .vizier.cm.mirror.sites.list -side left -padx 2

    pack .vizier.cm.mirror.text  -anchor n -side left
    pack .vizier.cm.mirror.sites -anchor n -side left

    pack .vizier.cm.catalog -anchor n -side left
    pack .vizier.cm.mirror -anchor n -side right -padx 7

    pack .vizier.cm -pady 3 -padx 5 -anchor w

    frame .vizier.surveytext  -relief flat
    label .vizier.surveytext.label -text "Or choose desired catalog:" \
        -font g_titleFont -anchor w
    pack  .vizier.surveytext -padx 5 -anchor w
    pack  .vizier.surveytext.label -anchor w -side top 

    frame .vizier.survey  -relief flat
    listbox .vizier.survey.list  -exportselection 0 -takefocus 0 \
         -height 7 -width 70 -font g_titleFont \
         -yscrollcommand ".vizier.survey.scroll set"
    scrollbar .vizier.survey.scroll \
         -command ".vizier.survey.list yview"   

    .vizier.survey.list insert end "Mega Catalogues"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(I/239)  The Hipparcos and Tycho Catalogues (ESA 1997)"
    .vizier.survey.list insert end "(I/196)  Hipparcos Input Catalogue, Version 2 (Turon+ 1993)"
    .vizier.survey.list insert end "(I/259)  The Tycho-2 Catalogue (Hog+ 2000)"
    .vizier.survey.list insert end "(I/250)  The Tycho Reference Catalogue (Hog+ 1998)"
    .vizier.survey.list insert end "(I/197A)  Tycho Input Catalogue, Revised version (Egret+ 1992)"
    .vizier.survey.list insert end "(I/207)  Preliminary list from Tycho observations (TIC data) (Halbwachs+ 1994)"
    .vizier.survey.list insert end "(I/247)  The AC2000 Catalogue (Urban+ 1997)"
    .vizier.survey.list insert end "(I/246)  The ACT Reference Catalog (Urban+ 1997)"
    .vizier.survey.list insert end "(I/252)  The USNO-A2.0 Catalogue (Monet+ 1998)"
    .vizier.survey.list insert end "(I/243)  The PMM USNO-A1.0 Catalogue (Monet 1997)"
    .vizier.survey.list insert end "(I/271)  The GSC 2.2 Catalogue (STScl, 2001)"
    .vizier.survey.list insert end "(I/284)  The USNO-B1.0 Catalog (Monet+ 2003)"
    .vizier.survey.list insert end "(I/289)  The USNO 2nd CCD Astrograph Catalogue (Zacharias+ 2003)"
    .vizier.survey.list insert end "(B/denis)  The DENIS database (DENIS Consortium, 1998)"
    .vizier.survey.list insert end "(J/A+AS/135/133)  First DENIS I-band extragalactic catalog (Vauglin+ 1999)"
    .vizier.survey.list insert end "(II/228A)  DENIS Catalogue toward Magellanic Clouds (DCMC) (Cioni+ 2000)"
    .vizier.survey.list insert end "(II/246)   2MASS All-Sky Catalog of Point Sources (Cutri+ 2003)"
    .vizier.survey.list insert end "(I/255)  The HST Guide Star Catalog, Version GSC-ACT (Lasker+ 1996-99)"
    .vizier.survey.list insert end "(I/254)  The HST Guide Star Catalog, Version 1.2 (Lasker+ 1996)"
    #.vizier.survey.list insert end "(I/220)  The HST Guide Star Catalog, Version 1.1 (Lasker+ 1992)"
    .vizier.survey.list insert end "(B/2mass)  The 2MASS database (IPAC/UMass, 2000)"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "Galaxies and QSOs"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(VII/155)  Third Reference Cat. of Bright Galaxies (RC3) (de Vaucouleurs+ 1991)"
    .vizier.survey.list insert end "(VII/16)  Reference Catalogue of Bright Galaxies (RC1; de Vaucouleurs+ 1964)"
    .vizier.survey.list insert end "(VII/119)  Catalogue of Principal Galaxies (PGC) (Paturel+ 1989)"
    .vizier.survey.list insert end "(VII/26D)  Uppsala General Catalogue of Galaxies (UGC) (Nilson 1973)"
    .vizier.survey.list insert end "(VII/145)  Nearby Galaxies Catalogue (NBG) (Tully 1988)"
    .vizier.survey.list insert end "(VII/157)  The Extended 12um galaxy sample (Rush+ 1993)"
    .vizier.survey.list insert end "(VII/4A)  Abell and Zwicky Clusters of Galaxies (Abell+ 1974)"
    .vizier.survey.list insert end "(VII/118)  NGC 2000.0 (Sky Publishing, ed. Sinnott 1988)"
    .vizier.survey.list insert end "(VII/1B)  Revised New General Catalogue (Sulentic+, 1973)"
    .vizier.survey.list insert end "(VII/224)  Quasars and Active Galactic Nuclei (10th Ed.) (Veron+ 2001)"
    .vizier.survey.list insert end "(VII/173)  Catalogue of Seyfert Galaxies (Lipovetsky+, 1988)"
    .vizier.survey.list insert end "(J/A+A/335/912)  Seyferts in galaxy pairs and groups (Kelm+ 1998)"
    .vizier.survey.list insert end "(J/AJ/114/2353)  AGNs and QSOs behind nearby galaxies (Crampton+, 1997)"
    .vizier.survey.list insert end "(J/A+AS/139/575)  RASS AGN sample (Wei+, 1999)"
    .vizier.survey.list insert end "(J/A+AS/133/171)  The Marseille Schmidt survey I. (Surace+ 1998)"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "Stars"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(I/131A)  SAO Star Catalog J2000 (SAO Staff 1966; USNO, ADC 1990)"
    .vizier.survey.list insert end "(V/102)  SKY2000 - Master Star Catalog, Version 2 (Sande+ 1998)"
    .vizier.survey.list insert end "(V/50)  Bright Star Catalogue, 5th Revised Ed. (Hoffleit+, 1991)"
    .vizier.survey.list insert end "(V/70A)  Nearby Stars, Preliminary 3rd Version (Gliese+ 1991)"
    .vizier.survey.list insert end "(III/135A)  Henry Draper Catalogue and Extension (Cannon+ 1918-1924; ADC 1989)"
    .vizier.survey.list insert end "(I/122)  Bonner Durchmusterung (Argelander 1859-62)"
    .vizier.survey.list insert end "(I/108)  Cape Photographic Durchmusterung (Gill+ 1895-1900)"
    .vizier.survey.list insert end "(I/114)  Cordoba Durchmusterung (Thome 1892-1932)"
    .vizier.survey.list insert end "(III/190B)  WEB Catalog of Radial Velocities (Duflot+ 1995)"
    .vizier.survey.list insert end "(III/213)  General Catalog of mean radial velocities (Barbier-Brossat+, 2000)"
    .vizier.survey.list insert end "(III/184)  3rd Bibliog. Cat. of Stellar Radial Vel. (Barbier-Brossat+, 1994)"
    .vizier.survey.list insert end "(III/191)  Radial Velocities of Nearby Stars (Tokovinin, 1992)"
    .vizier.survey.list insert end "(II/214A)  Combined General Catalogue of Variable Stars (Kholopov+ 1998)"
    .vizier.survey.list insert end "(II/139B)  General Catalog of Variable Stars, 4th Ed. (GCVS4) (Kholopov+ 1988)"
    .vizier.survey.list insert end "(II/219)  New Catalogue of Suspected Variable Stars Supplement (Kazarovets+ 1998)"
    .vizier.survey.list insert end "(I/238)  Yale Trigonometric Parallaxes, Fourth Edition (van Altena+ 1995)"
    .vizier.survey.list insert end "(I/146)  Positions and Proper Motions - North (Roeser+, 1988)"
    .vizier.survey.list insert end "(I/79)  Lowell Proper Motion Survey 8991 Northern Stars (Giclas 1971)"
    .vizier.survey.list insert end "(I/112)  Lowell Proper Motion Survey - Southern Hemisphere (Giclas+ 1978)"
    .vizier.survey.list insert end "(I/193)  Positions and Proper Motions - South (Bastian+ 1993)"
    .vizier.survey.list insert end "(I/98A)  NLTT Catalogue (Luyten, 1979)"
    .vizier.survey.list insert end "(I/87B)  LHS Catalogue, 2nd Edition (Luyten 1979)"
    .vizier.survey.list insert end "(II/226)  Stellar polarization catalogs agglomeration (Heiles, 2000)"
    .vizier.survey.list insert end "(II/215)  uvby-beta Catalogue (Hauck+ 1997)"
    .vizier.survey.list insert end "(III/42)  Selected MK Spectral Types (Jaschek, 1978)"
    .vizier.survey.list insert end "(III/198)  Palomar/MSU nearby star spectroscopic survey (Hawley+ 1997)"
    .vizier.survey.list insert end "(I/237)  The Washington Visual Double Star Catalog, 1996.0 (Worley+, 1996)"
    .vizier.survey.list insert end "(V/76)  Chromospherically Active Binaries (Strassmeier+ 1993)"
    .vizier.survey.list insert end "(J/A+AS/124/75)  Multiple star catalogue (MSC) (Tokovinin 1997-1999)"
    .vizier.survey.list insert end "(I/211)  CCDM (Components of Double and Multiple stars) (Dommanget+ 1994)"
    .vizier.survey.list insert end "(III/49)  White Dwarf Catalogue (Luyten 1970)"
    .vizier.survey.list insert end "(V/99)  Cataclysmic Binaries and LMXB Catalogue (Ritter+ 1998)"
    .vizier.survey.list insert end "(VII/92A)  Open Cluster Data 5th Edition (Lynga 1987)"
    .vizier.survey.list insert end "(V/84)  Strasbourg-ESO Catalogue of Galactic Planetary Nebulae (Acker+, 1992)"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "X-Ray Surveys"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(IX/10A)  ROSAT All-Sky Bright Source Catalogue (1RXS) (Voges+ 1999)"
    .vizier.survey.list insert end "(IX/29)  ROSAT All-Sky Survey Faint Source Catalog (Voges+ 2000)"
    .vizier.survey.list insert end "(IX/30)  Second ROSAT PSPC Catalog (ROSAT, 2000)"
    .vizier.survey.list insert end "(IX/31)  The WGACAT version of ROSAT sources (White+ 2000)"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "IR Surveys"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(II/125)  IRAS catalogue of Point Sources, Version 2.0 (IPAC 1986)"
    .vizier.survey.list insert end "(II/156A)  IRAS Faint Source Catalog, |b| > 10, Version 2.0 (Moshir+ 1989)"
    .vizier.survey.list insert end "(II/126)  IRAS Serendipitous Survey Catalog (IPAC 1986)"
    .vizier.survey.list insert end "(III/197)  IRAS Low Resolution Spectra (IRAS team, 1987)"
    .vizier.survey.list insert end "(II/225)  Catalog of Infrared Observations, Edition 5 (Gezari+ 1999)"
    .vizier.survey.list insert end "(V/98)  MSX Infrared Astrometric Catalog (Egan+ 1996)"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "Radio Surveys"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(J/ApJS/105/369)  Galactic H I column densities (Murphy+ 1996)"
    .vizier.survey.list insert end "(VIII/37)  The Third Bologna Survey (B3) (Ficarra+ 1985)"
    .vizier.survey.list insert end "(VIII/36)  The Second Bologna Survey (Colla+ 1970-1974)"
    .vizier.survey.list insert end "(VIII/5)  Bright Extragalactic Radio Sources (1Jy) (Kuehr+, 1981)"
    .vizier.survey.list insert end "(VIII/15)  Parkes Radio Sources Catalogue (PKSCAT90) (Wright+ 1990)"
    .vizier.survey.list insert end "(VIII/40)  GB6 catalog of radio sources (Gregory+ 1996)"
    .vizier.survey.list insert end "(VIII/42)  Texas Survey of radio sources at 365MHz (Douglas+ 1996)"
    .vizier.survey.list insert end "(VIII/14)  87GB Catalog of radio sources (Gregory et al., 1991)"
    .vizier.survey.list insert end "(VIII/38)  The Parkes-MIT-NRAO 4.85GHz (PMN) Surveys (Griffith+ 1993-1996)"
    .vizier.survey.list insert end "(III/175)  Optical spectroscopy of radio sources (Stickel+, 1989-94)"
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "Misc."
    .vizier.survey.list insert end ""
    .vizier.survey.list insert end "(VI/90)  Wide-Field Plate Database (Tsvetkov+ 1997)"
    .vizier.survey.list insert end "(B/hst)  HST Archived Exposures Catalog (STScI, 2001)"

    .vizier.survey.list selection set $_selectionBoxID
    pack .vizier.survey -pady 5 -padx 5 -fill both -expand 1
    pack .vizier.survey.list -side left -fill both -expand 1
    pack .vizier.survey.scroll -side right -fill y


    iwidgets::entryfield .vizier.conecenter \
	    -labeltext "Cone center (name or coordinates, e.g., 3c273):" \
            -labelfont g_titleFont -textfont g_titleFont \
	    -textvariable [itcl::scope _coneCenter]
    pack .vizier.conecenter -pady 3 -padx 5 -anchor w -fill x

    iwidgets::entryfield .vizier.searchradius \
	    -labeltext "Search radius (minutes):" \
            -labelfont g_titleFont -textfont g_titleFont \
	    -textvariable [itcl::scope _searchRadius]
    pack .vizier.searchradius -pady 3 -padx 5 -anchor w -fill x

    iwidgets::entryfield .vizier.maxrows \
	    -labeltext "Maximum number of rows:" \
            -labelfont g_titleFont -textfont g_titleFont \
	    -textvariable [itcl::scope _maxRows]
    pack .vizier.maxrows -pady 3 -padx 5 -anchor w -fill x

#    iwidgets::buttonbox .vizier.bbox
#    .vizier.bbox add submit -text "Submit" -command [itcl::code $this _fetchVizier]
#    .vizier.bbox add cancel -text "Cancel" -command "destroy .vizier"
#    pack .vizier.bbox -pady 5 -padx 5 -anchor w


    button .vizier.submit -text "Submit" -command [itcl::code $this _fetchVizier] -font g_titleFont
    button .vizier.cancel -text "Cancel" -command "destroy .vizier" -font g_titleFont
    button .vizier.help   -text "Help"   -command "hhelp VizieR" -font g_titleFont
    pack .vizier.submit -pady 5 -padx 5 -side left
    pack .vizier.cancel -pady 5 -padx 5 -side left
    pack .vizier.help   -pady 5 -padx 5 -side left

    # tkwait window .vizier 
}    

itcl::body FVVizier::_fetchVizier {} {
    global g_backupDir
    global g_fitsFileMode

    if { $_coneCenter == "" } {
	error "Please enter cone center"
	return
    }
    if { $_searchRadius == "" } {
	error "Please enter search radius"
	return
    }
    if { $_maxRows == "" } {
	error "Please enter maximun rows"
	return
    }

#    set sugName "vizier.tmp$_vizierFileID"

    set vizierFileName [file join $g_backupDir vizier.tmp_[clock seconds]]

    # get VizieR catalog
    set i [.vizier.survey.list curselection]
    set vizierSelect [.vizier.survey.list get $i ]
    # remember selection next time VizieR window is displayed
    set _selectionBoxID $i
    if { $_catalogID == "" } {
	# set catalogID
	regsub {\).*} $vizierSelect "" vizierSelect
	regsub {\(} $vizierSelect "" vizierSelect

	set catalogID $vizierSelect
    } else {
	set catalogID $_catalogID
    }

    # get VizieR mirror site
    set j [.vizier.cm.mirror.sites.list curselection]
    set vizierMirror [.vizier.cm.mirror.sites.list get $j ]
    # remember selection next time VizieR window is displayed
    set _mirrorBoxID $j

    switch $vizierMirror {
	"CDS (Strasbourg, France)" {
	    set vizierURL "http://vizier.u-strasbg.fr/cgi-bin/asu-fits"
	}
	"CfA (Harvard, USA)" {
	    # set vizierURL "http://adc.gsfc.nasa.gov/viz-bin/asu-fits"
	    set vizierURL "http://vizier.cfa.harvard.edu/viz-bin/asu-fits"
	}
	"ADAC (Tokyo, Japan)" {
	    set vizierURL "http://vizier.nao.ac.jp/viz-bin/asu-fits"
	}
	"CADC (Canada)" {
	    set vizierURL "vizier.hia.nrc.ca/viz-bin/asu-fits"
	}
	"Cambridge (UK)" {
	    set vizierURL "http://archive.ast.cam.ac.uk/viz-bin/asu-fits"
	}
	"IUCAA (Pune, India)" {
	    set vizierURL "http://urania.iucaa.ernet.in/viz-bin/asu-fits"
	}
	"Bejing (Bejing, China)" {
	    set vizierURL "http://data.bao.ac.cn/viz-bin/asu-fits"
	}
	"UKIRT (Hawaii, USA)" {
	    set vizierURL "http://www.ukirt.jach.hawaii.edu/viz-bin/asu-fits"
	}
    }

    # replace " " with "+" in _coneCenter
    regsub -all {[, ]} $_coneCenter {%20} coneCenter


    .vizier.submit configure -state disabled
    .vizier.cancel configure -state disabled

    if { ![catch {set urlVizier "$vizierURL?-source=$catalogID&-c=$coneCenter&-c.rm=$_searchRadius&-out.all&-out.max=$_maxRows&-out.add=RAJ2000,DEJ2000&-oc.form=dec"}] } {
	set vizierFile [open $vizierFileName w]

	#package require http 2.3
	package require http

	# catch { set token [::http::geturl $urlVizier -channel $vizierFile] } err

	catch { set token [::http::geturl $urlVizier] } err
     
        fconfigure $vizierFile -translation binary
        puts -nonewline $vizierFile [::http::data $token]
 
        close $vizierFile
    } else {
	error "Could not retrieve VizieR file.  Please recheck your search or contact VizieR."
    }

    .vizier.submit configure -state normal
    .vizier.cancel configure -state normal

    # open up Vizier file for viewing
    if { [catch {set tmp [openFitsFile $vizierFileName]}] } {
        tk_messageBox -icon warning -type ok \
                      -message "No matching record found within search radius."
	# error "The VizieR file has invalid format.  There was probably a mistake in your search entry.  Please recheck your search or contact VizieR."
    } else {
	set oldMode $g_fitsFileMode
	# Set Read-Only flag
	set g_fitsFileMode 1
	$tmp changeFile
	set g_fitsFileMode $oldMode

	# only when file is successfully closed and saved, do we increment
	incr _vizierFileID
    }
}
