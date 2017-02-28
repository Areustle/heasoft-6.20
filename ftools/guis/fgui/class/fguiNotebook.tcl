# A fguiNotebook widget for Tcl/Tk
# $Revision: 1.1 $
#
# Copyright (C) 1996,1997,1998 D. Richard Hipp
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.
#
# Author contact information:
#   drh@acm.org
#   http://www.hwaci.com/drh/


#
# Create a new notebook widget
#
proc fguiNotebook:create {w args} {
  global fguiNotebook powbg
  global g_notebookTitleFont

  set fontName [font names]
  if { [string first "g_notebookTitleFont" $fontName] >= 0 } {
     set fguiNotebook($w,fontName) g_notebookTitleFont
  } else {
     set fguiNotebook($w,fontName) [list System 16]
  }
  set fguiNotebook($w,width) 300
  set fguiNotebook($w,height) 300
  set fguiNotebook($w,pages) {}
  set fguiNotebook($w,top) 0
  set fguiNotebook($w,pad) 5
  set fguiNotebook($w,fg,on) black
  set fguiNotebook($w,textFg,on) black
  set fguiNotebook($w,textFg,off) grey50
  set fguiNotebook($w,fg,off) grey50
  canvas $w -bd 0 -highlightthickness 0 -takefocus 0
  set fguiNotebook($w,bg) [$w cget -bg]
  bind $w <1> "fguiNotebook:click $w %x %y"
  bind $w <Configure> "fguiNotebook:scheduleExpand $w"
  eval fguiNotebook:config $w $args
}

#
# Change configuration options for the notebook widget
#
proc fguiNotebook:config {w args} {
  global fguiNotebook
  foreach {tag value} $args {
    switch -- $tag {
      -width {
        set fguiNotebook($w,width) $value
      }
      -height {
        set fguiNotebook($w,height) $value
      }
      -pages {
        set fguiNotebook($w,pages) $value
      }
      -pad {
        set fguiNotebook($w,pad) $value
      }
      -bg {
        set fguiNotebook($w,bg) $value
      }
      -fg {
        set fguiNotebook($w,fg,on) $value
      }
      -disabledforeground {
        set fguiNotebook($w,fg,off) $value
      }
      -font {
        set fguiNotebook($w,fontName) $value
      }
      -textFg {
        set fguiNotebook($w,textFg,on) $value
      }
    }
  }

  #
  # After getting new configuration values, reconstruct the widget
  #
  $w delete all
  set fguiNotebook($w,x1) $fguiNotebook($w,pad)
  set fguiNotebook($w,x2) [expr $fguiNotebook($w,x1)+2]
  set fguiNotebook($w,x3) [expr $fguiNotebook($w,x2)+$fguiNotebook($w,width)]
  set fguiNotebook($w,x4) [expr $fguiNotebook($w,x3)+2]
  set fguiNotebook($w,y1) [expr $fguiNotebook($w,pad)+2]
  set fguiNotebook($w,y2) [expr $fguiNotebook($w,y1)+2]
  set fguiNotebook($w,y5) [expr $fguiNotebook($w,y1)+30]
  set fguiNotebook($w,y6) [expr $fguiNotebook($w,y5)+2]
  set fguiNotebook($w,y3) [expr $fguiNotebook($w,y6)+$fguiNotebook($w,height)]
  set fguiNotebook($w,y4) [expr $fguiNotebook($w,y3)+2]
  set x $fguiNotebook($w,x1)
  set cnt 0
  set y7 [expr $fguiNotebook($w,y1)+10]
  foreach p $fguiNotebook($w,pages) {
    set fguiNotebook($w,p$cnt,x5) $x
    set id [$w create text 0 0 -text $p -anchor nw -tags "p$cnt t$cnt" \
                               -font $fguiNotebook($w,fontName) \
                               -fill $fguiNotebook($w,textFg,on)]
    set bbox [$w bbox $id]
    set width [lindex $bbox 2]
    $w move $id [expr $x+10] $y7
    $w create line \
       $x $fguiNotebook($w,y5)\
       $x $fguiNotebook($w,y2) \
       [expr $x+2] $fguiNotebook($w,y1) \
       [expr $x+$width+16] $fguiNotebook($w,y1) \
       -width 2 -fill white -tags p$cnt
    $w create line \
       [expr $x+$width+16] $fguiNotebook($w,y1) \
       [expr $x+$width+18] $fguiNotebook($w,y2) \
       [expr $x+$width+18] $fguiNotebook($w,y5) \
       -width 2 -fill black -tags p$cnt
    set x [expr $x+$width+20]
    set fguiNotebook($w,p$cnt,x6) [expr $x-2]
    if {![winfo exists $w.f$cnt]} {
      frame $w.f$cnt -bd 0
    }

    place $w.f$cnt -x $fguiNotebook($w,x2) -y $fguiNotebook($w,y6) \
      -width $fguiNotebook($w,width) -height $fguiNotebook($w,height)
    $w.f$cnt config -bg $fguiNotebook($w,bg)
    incr cnt
  }
  $w create line \
     $fguiNotebook($w,x1) [expr $fguiNotebook($w,y5)-2] \
     $fguiNotebook($w,x1) $fguiNotebook($w,y3) \
     -width 2 -fill white
  $w create line \
     $fguiNotebook($w,x1) $fguiNotebook($w,y3) \
     $fguiNotebook($w,x2) $fguiNotebook($w,y4) \
     $fguiNotebook($w,x3) $fguiNotebook($w,y4) \
     $fguiNotebook($w,x4) $fguiNotebook($w,y3) \
     $fguiNotebook($w,x4) $fguiNotebook($w,y6) \
     $fguiNotebook($w,x3) $fguiNotebook($w,y5) \
     -width 2 -fill black
  if {![info exists fguiNotebook($w,expand)]} {
     $w config -width [expr $fguiNotebook($w,x4)+$fguiNotebook($w,pad)] \
	   -height [expr $fguiNotebook($w,y4)+$fguiNotebook($w,pad)] \
	   -bg $fguiNotebook($w,bg)
  }
  set top $fguiNotebook($w,top)
  set fguiNotebook($w,top) -1
  fguiNotebook:raise.page $w $top
}

#
# This routine is called whenever the mouse-button is pressed over
# the notebook.  It determines if any page should be raised and raises
# that page.
#
proc fguiNotebook:click {w x y} {
  global fguiNotebook
  if {$y<$fguiNotebook($w,y1) || $y>$fguiNotebook($w,y6)} return
  set N [llength $fguiNotebook($w,pages)]
  for {set i 0} {$i<$N} {incr i} {
    if {$x>=$fguiNotebook($w,p$i,x5) && $x<=$fguiNotebook($w,p$i,x6)} {
      fguiNotebook:raise.page $w $i
      break
    }
  }
}

#
# For internal use only.  This procedure raised the n-th page of
# the notebook
#
proc fguiNotebook:raise.page {w n} {
  global fguiNotebook
  if {$n<0 || $n>=[llength $fguiNotebook($w,pages)]} return
  set top $fguiNotebook($w,top)
  if {$top>=0 && $top<[llength $fguiNotebook($w,pages)]} {
    $w move p$top 0 2
  }
  $w move p$n 0 -2
  $w delete topline
  if {$n>0} {
    $w create line \
       $fguiNotebook($w,x1) $fguiNotebook($w,y6) \
       $fguiNotebook($w,x2) $fguiNotebook($w,y5) \
       $fguiNotebook($w,p$n,x5) $fguiNotebook($w,y5) \
       $fguiNotebook($w,p$n,x5) [expr $fguiNotebook($w,y5)-2] \
       -width 2 -fill white -tags topline
  }
  $w create line \
    $fguiNotebook($w,p$n,x6) [expr $fguiNotebook($w,y5)-2] \
    $fguiNotebook($w,p$n,x6) $fguiNotebook($w,y5) \
    -width 2 -fill white -tags topline
  $w create line \
    $fguiNotebook($w,p$n,x6) $fguiNotebook($w,y5) \
    $fguiNotebook($w,x3) $fguiNotebook($w,y5) \
    -width 2 -fill white -tags topline
  set fguiNotebook($w,top) $n
  raise $w.f$n
}

#
# Change the page-specific configuration options for the notebook
#
proc fguiNotebook:pageconfig {w name args} {
  global fguiNotebook
  set i [lsearch $fguiNotebook($w,pages) $name]
  if {$i<0} return
  foreach {tag value} $args {
    switch -- $tag {
      -state {
        if {"$value"=="disabled"} {
          $w itemconfig t$i -fg $fguiNotebook($w,fg,off)
        } else {
          $w itemconfig t$i -fg $fguiNotebook($w,fg,on)
        }
      }
      -onexit {
        set fguiNotebook($w,p$i,onexit) $value
      }
    }
  }
}

#
# This procedure raises a notebook page given its name.  But first
# we check the "onexit" procedure for the current page (if any) and
# if it returns false, we don't allow the raise to proceed.
#
proc fguiNotebook:raise {w name} {
  global fguiNotebook
  set i [lsearch $fguiNotebook($w,pages) $name]
  if {$i<0} return
  if {[info exists fguiNotebook($w,p$i,onexit)]} {
    set onexit $fguiNotebook($w,p$i,onexit)
    if {"$onexit"!="" && [eval uplevel #0 $onexit]!=0} {
      fguiNotebook:raise.page $w $i
    }
  } else {
    fguiNotebook:raise.page $w $i
  }
}

#
# Return the frame associated with a given page of the notebook.
#
proc fguiNotebook:frame {w name} {
  global fguiNotebook
  set i [lsearch $fguiNotebook($w,pages) $name]
  if {$i>=0} {
    return $w.f$i
  } else {
    return {}
  }
}

#
# Try to resize the notebook to the next time we become idle.
#
proc fguiNotebook:scheduleExpand w {
  global fguiNotebook
  if {[info exists fguiNotebook($w,expand)]} return
  set fguiNotebook($w,expand) 1
  after idle "fguiNotebook:expand $w"
}

#
# Resize the notebook to fit inside its containing widget.
#
proc fguiNotebook:expand w {
  global fguiNotebook
  set wi [expr [winfo width $w]-($fguiNotebook($w,pad)*2+4)]
  set hi [expr [winfo height $w]-($fguiNotebook($w,pad)*2+36)]
  fguiNotebook:config $w -width $wi -height $hi
  catch {unset fguiNotebook($w,expand)}
}

#
# Locate minimum dimensions of frame and expand to it
#

proc fguiNotebook:resize w {
   global fguiNotebook
   
   update
   set minWid [expr [winfo width $w]-($fguiNotebook($w,pad)*2+4)]
   set minHgt [expr [winfo height $w]-($fguiNotebook($w,pad)*2+36)]
   foreach pg $fguiNotebook($w,pages) {
      set frm [fguiNotebook:frame $w $pg]
      set hgt [winfo reqheight $frm]
      set wid [winfo reqwidth  $frm]
      if { $hgt>$minHgt } { set minHgt $hgt }
      if { $wid>$minWid } { set minWid $wid }
   }
   fguiNotebook:config $w -width $minWid -height $minHgt
}
