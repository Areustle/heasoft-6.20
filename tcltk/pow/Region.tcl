########################################################################
#
#   class:  Region     superclass:  Shape
#
# purpose:  Assign a shape to a graph and perform coordinate transforms
#           between SAO region descriptions and the canvas.
#         
#   usage:  Create a new region normally through the RegionList class
#           which allows the user to draw regions using the mouse.
#
#           To create a region programmaticly:
#                set rgn [gRegion graph canvas]
#           Once created, the region's graph and canvas cannot be
#           changed.  The essential parameters for a region are its
#           sign, geometric shape, shape descriptor, and rotation.
#           Set these individually using Shape's setShape and
#           setRotation and Region's setGraphCoords and setSign.  Or,
#           if you have the SAO representation in a list, use
#           setFunction.
#
#           To obtain the coords of a region, use "getGraphCoords" for
#           a list of points on the graph making up the shape.  Typically
#           the format is "X0 Y0 X1 Y1" where (X0,Y0) is the object center
#           and (X1,Y1) is a corner point.  Rotation is obtained separately
#           through "getRotation".  Use "getFunction" to get the SAO
#           region parameters... center, width, rotation.
#
#           The user can move and resize a region.  When this occurs,
#           the region notifies its "owner", via the Shape superclass.
#           Become the owner of a region using Shape's "setOwner" method.
#
# WARNING:  The setFunction/getFunction routines may change a Circle
#           region to an Ellipse region if the pixel->graph translation
#           is asymetric (ie, pixel scale is different on the two
#           axes)...  the shape must be converted to an ellipse to
#           retain its proper dimensions.
#
########################################################################

itcl::class Region {
   inherit Shape

   constructor { graph canvas } {Shape::constructor $canvas} {}
   destructor {}

   public {
      method setSign { sign }
      method getSign {      } { return $itsSign }

      method setSignColors { pos neg } { set signColors [list $pos $neg] }
      method setOutlineColor { color } { setColor $color }
      method setBoundaryColor { color } { setBoundaryClr $color }
      method setHandleColor   { color } { setHandleClr $color }
      method setLineWidth     { width } { setLineW $width }

      method getOutlineColor  {} { return [getColor] }
      method getBoundaryColor {} { return [getBoundaryClr] }
      method getHandleColor   {} { return [getHandleClr] }
      method getLineWidth     {} { return [getLineW] }

      method setStaticFlag { axis } { set static_axis $axis ; setStatic $axis}

      method getGraph {     } { return $itsGraph }

      method getFunction { units       }
      method setFunction { units descr }

      method getPropertyOrder { } { return $propertyOrder }
      method setPropertyOrder { order } { set propertyOrder $order }

      method notify { obj msg opts }

      # Override Shape's implementations to handle new coord system

      method getCoords      {        }
      method setCoords      { coords }

      method setGraphCoords { coords }
      method getGraphCoords {        }

      method draw {}
      method finishModification { }

      method click {}
   }

   private {
      variable itsSign "+"
      variable itsGraph
      variable grphCoords {}
      variable coordState ""
      variable signColors [list lightblue red]
      variable static_axis ""
      variable propertyOrder "Source"

      method processParameters { shape descr units }
   }

}

########################################################################
#
#  gRegion graph canvas
#
#  Create a new region in the global namespace.
#
########################################################################

proc gRegion { args } {
   return [uplevel #0 Region #auto $args]
}


########################################################################
#
#
#
########################################################################

itcl::body Region::constructor { graph canvas } {
   global powPlotParam
   global powRotation

   set itsGraph $graph

   if { [powWCSexists $itsGraph] } {
      if { [info exists itsGraph] && [info exists powRotation($itsGraph)] } {
         setRotation $powRotation($itsGraph)
      }
   }

   eval setClip [$itsCanvas coords ${itsGraph}box]
   setScale $powPlotParam(xmagstep,$itsGraph) $powPlotParam(ymagstep,$itsGraph)

   addTags [list ${itsGraph}region]

   set NC [gNotifications default]
   $NC addObserver $this notify $itsGraph graphHasResized
   $NC addObserver $this notify $itsGraph graphHasMoved
   $NC addObserver $this notify $itsGraph graphHasFinishedDrawing
   $NC addObserver $this notify $itsGraph graphHasBeenSelected

   $itsCanvas bind "$itsTag" <<BtnPress>> [itcl::code $this click]
}

itcl::body Region::destructor {} {
   [gNotifications default] removeObserver $this
}


itcl::body Region::notify { obj msg opts } {
   global powPlotParam

   switch -- $msg {

      "graphHasBeenSelected" {
         #  Redraw graph handles in new locations
         if { $isSelected } select
         $itsCanvas raise $itsTag
      }

      "graphHasMoved" {
         # Must force recalculation of canvas coordinates
         set coordState "graph"
         $itsCanvas move $itsTag          [lindex $opts 0] [lindex $opts 1]
         $itsCanvas move rgnHandle$itsTag [lindex $opts 0] [lindex $opts 1]
      }

      "graphHasFinishedDrawing" -
      "graphHasResized" {
         # Must force recalculation of canvas coordinates
         set coordState "graph"
         setScale $powPlotParam(xmagstep,$itsGraph) \
               $powPlotParam(ymagstep,$itsGraph)
         draw
         #  Redraw graph handles in new locations
         if { $isSelected } select
      }
   }

}


itcl::body Region::draw {} {
   if { !$ignoreClip } {
      eval setClip [$itsCanvas coords ${itsGraph}box]
   }
   Shape::draw
}


itcl::body Region::click {} {
#puts "itsGraph: $itsGraph"
#puts "currentGraph: [powGetCurrentGraph]"
#puts "allTags: $allTags"
   #  If region is dragable without activating the graph, don't
   if { $itsGraph != [powGetCurrentGraph] && [lsearch $allTags DragAble]==-1 } {
      powSelectGraph $itsGraph
   }
   if { [lsearch $allTags DragAble]!=-1 } {
      select
   }
}


itcl::body Region::setSign { sign } {
   if { $static_axis == "" } {
      if { $sign=="+" } {
         setColor [lindex $signColors 0]
      } elseif { $sign=="-"} {
         setColor [lindex $signColors 1]
      }
   }

   set itsSign $sign
}


itcl::body Region::finishModification { } {
   # Make sure grphCoords are up-to-date
   getGraphCoords
   Shape::finishModification
}

#####################################################################
#
#   Convert Coordinates between various formats
#
#####################################################################

itcl::body Region::getCoords { } {
   if { $coordState == "graph" } {
      # Convert graph to canvas coords
      set canvCoords {}
      foreach [list X Y] $grphCoords {
         foreach {x y} [powGraphToCanvas $itsGraph $X $Y] {}
         lappend canvCoords $x $y
      }
      Shape::setCoords $canvCoords
      set coordState ""
   }
   return [Shape::getCoords]
}

itcl::body Region::getGraphCoords { } {
   if { $coordState == "canvas" } {
      # Convert canvas to graph coords
      set grphCoords {}
      foreach [list x y] [Shape::getCoords] {
         foreach [list X Y] [powCanvasToGraph $itsGraph $x $y] {}
         lappend grphCoords $X $Y
      }
      set coordState ""
   }
   return $grphCoords
}

itcl::body Region::setCoords { coords } {
   Shape::setCoords $coords
   set coordState "canvas"
}

itcl::body Region::setGraphCoords { coords } {
   set nelem [llength $coords]
   if { [expr $nelem%2] } {
      error "Shape coordinates must contain an even number of elements"
   }
   set grphCoords $coords
   set coordState "graph"
}

########################################################################
#
#   Convert between shape-specific functions and internal graph coords
#
########################################################################

itcl::body Region::getFunction { units } {
   # Convert parameters to the shape specific graph-based description.
   global powPlotParam
   global powRotation
   global regionParam

   if { [llength $units]==1 } {
      switch -- $units {
         "pixels" -
         "image" {
            set posUnits "pixels"
            set sizUnits "pixels"
         }
         "linear" {
            set posUnits "linear"
            set sizUnits "linear"
         }
         "saotng" {
            set posUnits "degrees"
            set sizUnits "pixels"
         }
         "pixel2pos" {
            set posUnits "degrees"
            set sizUnits "degrees"
         }
         default {
            set posUnits "degrees"
            set sizUnits "pixels"
         }
      }
   } else {
      set posUnits [lindex $units 0]
      set sizUnits [lindex $units 1]
   }

   set cnt 0
   foreach [list x y] [getCoords] [list X Y] [getGraphCoords] {
      set x$cnt $x
      set y$cnt $y
      set X$cnt $X
      set Y$cnt $Y
      incr cnt
   }
   set dx [expr abs($x1-$x0)/$xScale]
   set dy [expr abs($y1-$y0)/$yScale]
    
   if { [info exists itsGraph] && [info exist powRotation($itsGraph)] } {
      set rot [expr [getRotation] - $powRotation($itsGraph)]
   } else {
      set rot   [getRotation]
   }
   set shape [getShape]

   set descr [list $X0 $Y0]
   switch $shape {
      Box     { lappend descr [expr $dx+$dx] [expr $dy+$dy] $rot }
      Circle  { lappend descr [expr sqrt($dx*$dx+$dy*$dy)] }
      Ellipse { 
         lappend descr [expr 1.41421356*$dx] [expr 1.41421356*$dy] \
               $rot
      }
      Polygon {
         for {set i 1} {$i<$cnt} {incr i} {
            eval lappend descr \$X$i \$Y$i
         }
      }
      Line    { lappend descr $X1 $Y1 }
      Point   { }
   }

   #
   #   descr is now in the Graph's prefered decimal coordinate system
   #   ...  graph positions, pixel sizes, degree rotations
   #
   ####################################################################
   #
   # Convert description to desired coordinate system if necessary
   #

   if { [powWCSexists $itsGraph] } {
      set WCS 1
   } else {
      set WCS 0
   }
   set wcsObj $powPlotParam(currimg,$itsGraph)
   if { $wcsObj=="NULL" } {
      set wcsObj [lindex $powPlotParam(curves,$itsGraph) 0]
   }

   set newD {}
   if { $shape=="Line" || $shape=="Polygon" || $shape=="Point" } {

      #  These objects consist of just pairs of coordinates

      foreach [list x y] $descr {

         if { $posUnits=="pixels" } {
            foreach [list x y] [powGraphToPixel $wcsObj $x $y] {}
            set x [expr $x + 1]
            set y [expr $y + 1]

            if { [info exists regionParam(format)] && $regionParam(format) == "Physical (Pixels)" } {
               set result [powConvertImage2Physical $x $y]
               set x [lindex $result 0]
               set y [lindex $result 1]
            }

         } elseif { $posUnits=="degrees" && !$WCS } {
            error "Cannot code region in degrees, since graph\
                  lacks WCS information."
         }
         lappend newD $x $y

      }

   } else {

      #  Remaining objects consist of center, sizes, and rotations

      #  Parse the center position

      foreach [list x y] [lrange $descr 0 1] {

         if { $posUnits=="pixels" } {
            foreach [list x y] [powGraphToPixel $wcsObj $x $y] {}
            set x [expr $x + 1]
            set y [expr $y + 1]
            set image_x $x
            set image_y $y
            
            if { [info exists regionParam(format)] && $regionParam(format) == "Physical (Pixels)" } {
               set result [powConvertImage2Physical $x $y]
               set x [lindex $result 0]
               set y [lindex $result 1]
            }

         } elseif { $posUnits=="degrees" && !$WCS } {
            error "Cannot code region in degrees, since graph\
                  lacks WCS information."
         }
         lappend newD $x $y
      }

      #  Parse remaining size parameters (given in image pixel coords)

      if { $shape=="Circle" } {

         set radius [lindex $descr 2]
         if { [info exists regionParam(format)] && $regionParam(format) == "Physical (Pixels)" } {
            set radius [powConvertRadiusImage2Physical $image_x $image_y $x $radius]
         }

         if { $radius == 0.0 } {

         } elseif { ($sizUnits=="degrees" && $WCS) || ($sizUnits=="linear") } {
            foreach [list dx dy] [powPixelVToGraphV $wcsObj $radius 0] {}
            set degWidth [expr sqrt($dx*$dx+$dy*$dy)]
            foreach [list dx dy] [powPixelVToGraphV $wcsObj 0 $radius] {}
            set degHeight [expr sqrt($dx*$dx+$dy*$dy)]
            set ratio [expr abs($degWidth/$degHeight-1.0)]
            if { $ratio > 0.1 } {
               #  Degree Height and Width aren't the same... change to
               #  and ellipse...
               set shape "Ellipse"
               setShape $shape
               lappend newD $degWidth $degHeight
               # Set radius to rotation value, 0.0
               set radius 0.0
            } else {
               set radius [expr 0.5*($degWidth+$degHeight)]
            }
         } elseif { $sizUnits=="degrees" && !$WCS } {
            error "Cannot code region in degrees, since graph\
                  lacks WCS information."
         }
         lappend newD $radius

      } else {

         set width  [lindex $descr 2]
         set height [lindex $descr 3]
         set rot    [lindex $descr 4]

         if { ($sizUnits=="degrees" && $WCS) || ($sizUnits=="linear") } {
            foreach [list dx dy] [powPixelVToGraphV $wcsObj $width 0] {}
            set width [expr sqrt($dx*$dx+$dy*$dy)]
            foreach [list dx dy] [powPixelVToGraphV $wcsObj 0 $height] {}
            set height [expr sqrt($dx*$dx+$dy*$dy)]
         } elseif { $sizUnits=="degrees" && !$WCS } {
            error "Cannot code region in degrees, since graph\
                  lacks WCS information."
         }
         lappend newD $width $height $rot

     }
   }
   set descr $newD
   return $descr
}


#  descr must consist of pure numbers; no unit/formatting allowed

itcl::body Region::setFunction { units descr } {
   global powRotation

   set shape [getShape]
   foreach [list shape descr] \
         [processParameters $shape $descr $units] {}
   if { $shape != [getShape] } {
      setShape $shape
   }

   #
   #   descr is now in the Graph's prefered decimal coordinate system
   #   ...  graph positions, pixel sizes, degree rotations
   #
   ####################################################################
   #
   # Convert the shape specific description to standard 2n parameters
   #     rot & x0 y0 x1 y1 (... xn yn for polygons) in *canvas* coords
   #  Must use canvas coords because widths/heights of objects cannot
   #  be calculated simply in degree space
    
   set cnt 0
   foreach p $descr {
      incr cnt
      set p$cnt $p
   }

   set rot 0.0
   foreach {x0 y0} [powGraphToCanvas $itsGraph $p1 $p2] {}
   if {$cnt>2} {
      set dx [expr $p3*$xScale]
      if {$cnt>3} {
         set dy [expr $p4*$yScale]
         if {$cnt==5} {
            set rot $p5
            if { [powWCSexists $itsGraph] } {
               if { [info exists itsGraph] && [info exists powRotation($itsGraph)] } {
                  set rot [expr $powRotation($itsGraph) + $rot]
               }
            }
         }
      }
   }

   set newParams [list $x0 $y0]
   switch $shape {
      Box     { lappend newParams [expr $x0+0.5*$dx] [expr $y0+0.5*$dy] }
      Circle  { lappend newParams [expr $x0+$dx/1.41421356] \
                                  [expr $y0+$p3*$yScale/1.41421356] }
      Ellipse { lappend newParams [expr $x0+$dx/1.41421356] \
                                  [expr $y0+$dy/1.41421356] }
      Polygon {
         foreach {x y} [lrange $descr 2 end] {
            foreach {x y} [powGraphToCanvas $itsGraph $x $y] {}
            lappend newParams $x $y
         }
      }
      Line    { 
         foreach {x1 y1} [powGraphToCanvas $itsGraph $p3 $p4] {}
         lappend newParams $x1 $y1
      }
      Point   { lappend newParams $x0 $y0 }
   }

   setRotation $rot
   setCoords $newParams
}


itcl::body Region::processParameters { shape descr units } {
   global powPlotParam

   if { [llength $units]==1 } {
      switch -- $units {
         "pixels" -
         "image" {
            set posUnits "pixels"
            set sizUnits "pixels"
         }
         "linear" {
            set posUnits "linear"
            set sizUnits "linear"
         }
         "saotng" {
            set posUnits "degrees"
            set sizUnits "pixels"
         }
         default {
            set posUnits "degrees"
            set sizUnits "degrees"
         }
      }
   } else {
      set posUnits [lindex $units 0]
      set sizUnits [lindex $units 1]
   }

# Convert description to the graph's coordinate system if necessary
   
   if { [powWCSexists $itsGraph] } {
      set WCS 1
   } else {
      set WCS 0
   }
   set wcsObj $powPlotParam(currimg,$itsGraph)
   if { $wcsObj=="NULL" } {
      set wcsObj [lindex $powPlotParam(curves,$itsGraph) 0]
   }

   set newD {}
   if { $shape=="Line" || $shape=="Polygon" || $shape=="Point" } {

      #  These objects consist of just pairs of coordinates

      foreach [list x y] $descr {
         if { $posUnits=="pixels" } {
            foreach [list x y] [powPixelToGraph $wcsObj \
                  [expr $x-1] [expr $y-1]] {}
         } elseif { $posUnits=="degrees" && !$WCS } {
            error "Region coded in degrees, but graph lacks WCS information."
         }
         lappend newD $x $y
      }

   } else {

      #  Remaining objects consist of center, sizes, and rotations

      #  Parse the center position

      foreach [list x y] [lrange $descr 0 1] {
         if { $posUnits=="pixels" } {
            foreach [list x y] [powPixelToGraph $wcsObj \
                  [expr $x-1] [expr $y-1]] {}
         } elseif { $posUnits=="degrees" && !$WCS } {
            error "Region coded in degrees, but graph lacks WCS information."
         }
         lappend newD $x $y
      }

      #  Parse remaining size parameters... to current image's pixel dims

      if { $shape=="Circle" } {

         set radius [lindex $descr 2]
         if { $radius == 0.0 } {
            #  Do nothing (prevent divide by zero in next block)
         } elseif { ($sizUnits=="degrees" && $WCS) || ($sizUnits=="linear") } {
            foreach [list dx dy] [powPixelVToGraphV $wcsObj 1 0] {}
            set pixWidth [expr $radius/sqrt($dx*$dx+$dy*$dy)]
            foreach [list dx dy] [powPixelVToGraphV $wcsObj 0 1] {}
            set pixHeight [expr $radius/sqrt($dx*$dx+$dy*$dy)]
            set ratio [expr abs($pixWidth/$pixHeight-1.0)]
            if { $ratio > 0.1 } {
               #  Pixel Height and Width aren't the same... change to
               #  an ellipse...
               set shape "Ellipse"
               lappend newD $pixWidth $pixHeight
               # Set radius to rotation value, 0.0
               set radius 0.0
            } else {
               set radius [expr 0.5*($pixWidth+$pixHeight)]
            }
         } elseif { $sizUnits=="degrees" && !$WCS } {
            error "Region coded in degrees, but graph lacks WCS information."
         }
         lappend newD $radius

      } else {

         set width  [lindex $descr 2]
         set height [lindex $descr 3]
         set rot    [lindex $descr 4]

         if { ($sizUnits=="degrees" && $WCS) || ($sizUnits=="linear") } {
            foreach [list dx dy] [powPixelVToGraphV $wcsObj 1 0] {}
            set width [expr $width/sqrt($dx*$dx+$dy*$dy)]
            foreach [list dx dy] [powPixelVToGraphV $wcsObj 0 1] {}
            set height [expr $height/sqrt($dx*$dx+$dy*$dy)]
         } elseif { $sizUnits=="degrees" && !$WCS } {
            error "Region coded in degrees, but graph lacks WCS information."
         }
         lappend newD $width $height $rot

      }

   }

   return [list $shape $newD]
}
