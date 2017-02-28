# A Notebook widget for Tcl/Tk
# $Revision: 1.10 $
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
proc Notebook:create {w args} {
  global Notebook powbg
  global g_notebookTitleFont

  set fontName [font names]
  if { [string first "g_notebookTitleFont" $fontName] >= 0 } {
     set Notebook($w,fontName) g_notebookTitleFont
  } else {
     set Notebook($w,fontName) [list System 16]
  }
  set Notebook($w,width) 300
  set Notebook($w,height) 300
  set Notebook($w,pages) {}
  set Notebook($w,top) 0
  set Notebook($w,pad) 5
  set Notebook($w,fg,on) black
  set Notebook($w,textFg,on) black
  set Notebook($w,textFg,off) grey50
  set Notebook($w,fg,off) grey50
  canvas $w -bd 0 -highlightthickness 0 -takefocus 0
  set Notebook($w,bg) [$w cget -bg]
  bind $w <1> "Notebook:click $w %x %y"
  bind $w <Configure> "Notebook:scheduleExpand $w"
  eval Notebook:config $w $args
}

#
# Change configuration options for the notebook widget
#
proc Notebook:config {w args} {
  global Notebook
  global notebookWndw

  set notebookWndw $w
  foreach {tag value} $args {
    switch -- $tag {
      -width {
        set Notebook($w,width) $value
      }
      -height {
        set Notebook($w,height) $value
      }
      -pages {
        set Notebook($w,pages) $value
      }
      -pad {
        set Notebook($w,pad) $value
      }
      -bg {
        set Notebook($w,bg) $value
      }
      -fg {
        set Notebook($w,fg,on) $value
      }
      -disabledforeground {
        set Notebook($w,fg,off) $value
      }
      -font {
        set Notebook($w,fontName) $value
      }
      -textFg {
        set Notebook($w,textFg,on) $value
      }
    }
  }

  #
  # After getting new configuration values, reconstruct the widget
  #
  $w delete all
  set Notebook($w,x1) $Notebook($w,pad)
  set Notebook($w,x2) [expr $Notebook($w,x1)+2]
  set Notebook($w,x3) [expr $Notebook($w,x2)+$Notebook($w,width)]
  set Notebook($w,x4) [expr $Notebook($w,x3)+2]
  set Notebook($w,y1) [expr $Notebook($w,pad)+2]
  set Notebook($w,y2) [expr $Notebook($w,y1)+2]
  set Notebook($w,y5) [expr $Notebook($w,y1)+30]
  set Notebook($w,y6) [expr $Notebook($w,y5)+2]
  set Notebook($w,y3) [expr $Notebook($w,y6)+$Notebook($w,height)]
  set Notebook($w,y4) [expr $Notebook($w,y3)+2]
  set x $Notebook($w,x1)
  set cntx 0
  set y7 [expr $Notebook($w,y1)+10]
  foreach p $Notebook($w,pages) {
    set cnt [lindex [split $p "_"] 1]
    set Notebook($w,p$cnt,x5) $x
    set id [$w create text 0 0 -text [lindex $Notebook($w,text) $cntx] \
                               -anchor nw -tags "p$cnt t$cnt" \
                               -font $Notebook($w,fontName) \
                               -fill $Notebook($w,textFg,on)]
    set bbox [$w bbox $id]
    set width [lindex $bbox 2]
    $w move $id [expr $x+10] $y7

    set xwidth 0
    if { $cnt > 0 } {
       image create bitmap exitIcon -data {
          #define exit_width 13
          #define exit_height 13
          static char exit_bits[] = {
            0xfc,0xef,0xfe,0xff,0xff,0xff,0xe7,0xf9,0xc7,0xf8,0x0f,0xfc,0x1f,0xfe,0x1f,
            0xfe,0x0f,0xfc,0xc7,0xf8,0xe7,0xf9,0xff,0xff,0xfe,0xff};
       } -foreground red

       set xid [$w create image 0 0 -anchor nw -image exitIcon -tags "p$cnt x$cnt"]
       set xbbox [$w bbox $xid]
       set xwidth [expr [lindex $xbbox 2] + 5]

       $w bind x$cnt <1> {
          global Notebook
          global notebookWndw
          global NotebookTerm

          bind .ftoolframe <Destroy> {}
          set current [%W find withtag current]
          set cnt [string range [lindex [split [%W itemcget current -tags] " "] 0] 1 end]
          set displayTop false
          if { $cnt == [expr [llength $Notebook($notebookWndw,pages)] - 1] } {
             set displayTop true
          }

          set targetIdx [lsearch $Notebook($notebookWndw,pages) Tab_$cnt]
          Notebook:delete $notebookWndw [lindex $Notebook($notebookWndw,pages) $targetIdx]
          Notebook:resize $notebookWndw

          if { $displayTop == "false" } {
             Notebook:click $notebookWndw %x %y
          } else {
             Notebook:raise.page $notebookWndw 0
          }
          bind .ftoolframe <Destroy> { exit }
       }

       $w move $xid [expr $x+$width+17] $y7
    }

    $w create line \
       $x $Notebook($w,y5)\
       $x $Notebook($w,y2) \
       [expr $x+2] $Notebook($w,y1) \
       [expr $x+$width+$xwidth+16] $Notebook($w,y1) \
       -width 2 -fill white -tags p$cnt
    $w create line \
       [expr $x+$width+$xwidth+16] $Notebook($w,y1) \
       [expr $x+$width+$xwidth+18] $Notebook($w,y2) \
       [expr $x+$width+$xwidth+18] $Notebook($w,y5) \
       -width 2 -fill black -tags p$cnt
    set x [expr $x+$width+$xwidth+20]
    set Notebook($w,p$cnt,x6) [expr $x-2]
    if {![winfo exists $w.f$cnt]} {
       frame $w.f$cnt -bd 0
    }

    place $w.f$cnt -x $Notebook($w,x2) -y $Notebook($w,y6) \
      -width $Notebook($w,width) -height $Notebook($w,height)
    $w.f$cnt config -bg $Notebook($w,bg)

    incr cntx
  }
  $w create line \
     $Notebook($w,x1) [expr $Notebook($w,y5)-2] \
     $Notebook($w,x1) $Notebook($w,y3) \
     -width 2 -fill white -tags p$cntx
  $w create line \
     $Notebook($w,x1) $Notebook($w,y3) \
     $Notebook($w,x2) $Notebook($w,y4) \
     $Notebook($w,x3) $Notebook($w,y4) \
     $Notebook($w,x4) $Notebook($w,y3) \
     $Notebook($w,x4) $Notebook($w,y6) \
     $Notebook($w,x3) $Notebook($w,y5) \
     -width 2 -fill black -tags p$cntx
  if {![info exists Notebook($w,expand)]} {
     $w config -width [expr $Notebook($w,x4)+$Notebook($w,pad)] \
	   -height [expr $Notebook($w,y4)+$Notebook($w,pad)] \
	   -bg $Notebook($w,bg)
  }
  set top $Notebook($w,top)
  set Notebook($w,top) -1
  Notebook:raise.page $w $top
}

proc Notebook:updateTitle {w page newText } {
  global Notebook

  set cntx [lsearch $Notebook($w,pages) $page]
  set cnt [lindex [split $page "_"] 1]
  $w itemconfigure t$cnt -text $newText
  set Notebook($w,text) [lreplace $Notebook($w,text) $cntx $cntx $newText]
  Notebook:resize $w
}

proc Notebook:modifyTerm {w page t} {
  global NotebookTerm

  set NotebookTerm($w,$page) $t
}

proc Notebook:add {w page} {
  global Notebook
  set cnt [lindex [split $page "_"] 1]

  lappend Notebook($w,pages) $page
  lappend Notebook($w,text)  $page
  set currentWidth [$w cget -width]

  set Notebook($w,x1) [expr $Notebook($w,pad) + $currentWidth]
  set Notebook($w,x2) [expr $Notebook($w,x1)+2]
  set Notebook($w,x3) [expr $Notebook($w,x2)]
  set Notebook($w,x4) [expr $Notebook($w,x3)+2]
  set Notebook($w,y1) [expr $Notebook($w,pad)+2]
  set Notebook($w,y2) [expr $Notebook($w,y1)+2]
  set Notebook($w,y5) [expr $Notebook($w,y1)+30]
  set Notebook($w,y6) [expr $Notebook($w,y5)+2]
  set Notebook($w,y3) [expr $Notebook($w,y6)+$Notebook($w,height)]
  set Notebook($w,y4) [expr $Notebook($w,y3)+2]
  set x $currentWidth
  set y7 [expr $Notebook($w,y1)+10]

  set Notebook($w,p$cnt,x5) $x
  set id [$w create text 0 0 -text $page -anchor nw -tags "p$cnt t$cnt" \
                             -font $Notebook($w,fontName) \
                             -fill $Notebook($w,textFg,on)]

  set bbox [$w bbox $id]
  set width [lindex $bbox 2]

  image create bitmap exitIcon -data {
     #define exit_width 13
     #define exit_height 13
     static char exit_bits[] = {
       0xfc,0xef,0xfe,0xff,0xff,0xff,0xe7,0xf9,0xc7,0xf8,0x0f,0xfc,0x1f,0xfe,0x1f,
       0xfe,0x0f,0xfc,0xc7,0xf8,0xe7,0xf9,0xff,0xff,0xfe,0xff};
  } -foreground red

  set xid [$w create image 0 0 -anchor nw -image exitIcon -tags "p$cnt x$cnt"]
  set xbbox [$w bbox $xid]
  set xwidth [expr [lindex $xbbox 2] + 5]

  $w move $id [expr $x+10] $y7
  $w move $xid [expr $x+$width+17] $y7

  $w create line \
     $x $Notebook($w,y5)\
     $x $Notebook($w,y2) \
     [expr $x+2] $Notebook($w,y1) \
     [expr $x+$width+$xwidth+16] $Notebook($w,y1) \
     -width 2 -fill white -tags p$cnt
  $w create line \
     [expr $x+$xwidth+$width+16] $Notebook($w,y1) \
     [expr $x+$width+$xwidth+18] $Notebook($w,y2) \
     [expr $x+$width+$xwidth+18] $Notebook($w,y5) \
     -width 2 -fill black -tags p$cnt
  set x [expr $x+$width+$xwidth+20]
  set Notebook($w,p$cnt,x6) [expr $x-2]
  if {![winfo exists $w.f$cnt]} {
    frame $w.f$cnt -bd 0
  }

  place $w.f$cnt -x $Notebook($w,x2) -y $Notebook($w,y6) \
    -width $Notebook($w,width) -height $Notebook($w,height)
  $w.f$cnt config -bg $Notebook($w,bg)

  $w create line \
     $Notebook($w,x1) [expr $Notebook($w,y5)-2] \
     $Notebook($w,x1) $Notebook($w,y3) \
     -width 2 -fill white
  $w create line \
     $Notebook($w,x1) $Notebook($w,y3) \
     $Notebook($w,x2) $Notebook($w,y4) \
     $Notebook($w,x3) $Notebook($w,y4) \
     $Notebook($w,x4) $Notebook($w,y3) \
     $Notebook($w,x4) $Notebook($w,y6) \
     $Notebook($w,x3) $Notebook($w,y5) \
     -width 2 -fill black

  if {![info exists Notebook($w,expand)]} {
     $w config -width [expr $Notebook($w,x4)+$Notebook($w,pad)] \
	   -height [expr $Notebook($w,y4)+$Notebook($w,pad)] \
	   -bg $Notebook($w,bg)
  }
  set top $Notebook($w,top)
  set Notebook($w,top) -1
  Notebook:raise.page $w $top
  return $cnt
}

#
# This routine is called whenever the mouse-button is pressed over
# the notebook.  It determines if any page should be raised and raises
# that page.
#
proc Notebook:click {w x y} {
  global Notebook
  global mainSession

  if {$y<$Notebook($w,y1) || $y>$Notebook($w,y6)} return
  set N [llength $Notebook($w,pages)]
  for {set i 0} {$i<$N} {incr i} {
    set idx [lindex [split [lindex $Notebook($w,pages) $i] "_"] 1]
    if {$x>=$Notebook($w,p$idx,x5) && $x<=$Notebook($w,p$idx,x6)} {
      if [winfo exists $w.f$idx] {
         Notebook:raise.page $w $i
         $mainSession setCurrentTab [lindex $Notebook($w,pages) $i]
      }
      break
    }
  }
}

#
# For internal use only.  This procedure raised the n-th page of
# the notebook
#
proc Notebook:raise.page {w n} {
  global Notebook

  if {$n<0 || $n>=[llength $Notebook($w,pages)]} return
  set n [lindex [split [lindex $Notebook($w,pages) $n] "_"] 1]
  set top $Notebook($w,top)
  $w delete topline
  if {$n>0} {
    $w create line \
       $Notebook($w,x1) $Notebook($w,y6) \
       $Notebook($w,x2) $Notebook($w,y5) \
       $Notebook($w,p$n,x5) $Notebook($w,y5) \
       $Notebook($w,p$n,x5) [expr $Notebook($w,y5)-2] \
       -width 2 -fill white -tags topline
  }
  $w create line \
    $Notebook($w,p$n,x6) [expr $Notebook($w,y5)-2] \
    $Notebook($w,p$n,x6) $Notebook($w,y5) \
    -width 2 -fill white -tags topline
  $w create line \
    $Notebook($w,p$n,x6) $Notebook($w,y5) \
    $Notebook($w,x3) $Notebook($w,y5) \
    -width 2 -fill white -tags topline
  set Notebook($w,top) $n
  raise $w.f$n
}

#
# Change the page-specific configuration options for the notebook
#
proc Notebook:pageconfig {w name args} {
  global Notebook
  set i [lsearch $Notebook($w,pages) $name]
  if {$i<0} return
  foreach {tag value} $args {
    switch -- $tag {
      -state {
        if {"$value"=="disabled"} {
          $w itemconfig t$i -fg $Notebook($w,fg,off)
        } else {
          $w itemconfig t$i -fg $Notebook($w,fg,on)
        }
      }
      -onexit {
        set Notebook($w,p$i,onexit) $value
      }
    }
  }
}

#
# This procedure raises a notebook page given its name.  But first
# we check the "onexit" procedure for the current page (if any) and
# if it returns false, we don't allow the raise to proceed.
#
proc Notebook:raise {w name} {
  global Notebook
  set i [lsearch $Notebook($w,pages) $name]
  if {$i<0} return
  if {[info exists Notebook($w,p$i,onexit)]} {
    set onexit $Notebook($w,p$i,onexit)
    if {"$onexit"!="" && [eval uplevel #0 $onexit]!=0} {
      Notebook:raise.page $w $i
    }
  } else {
    Notebook:raise.page $w $i
  }
}

#
# Return the frame associated with a given page of the notebook.
#
proc Notebook:frame {w name} {
  global Notebook
  set i [lsearch $Notebook($w,pages) $name]
  if {$i>=0} {
    return $w.f$i
  } else {
    return {}
  }
}

#
# Try to resize the notebook to the next time we become idle.
#
proc Notebook:scheduleExpand w {
  global Notebook
  if {[info exists Notebook($w,expand)]} return
  set Notebook($w,expand) 1
  after idle "Notebook:expand $w"
}

#
# Resize the notebook to fit inside its containing widget.
#
proc Notebook:expand w {
  global Notebook
  set wi [expr [winfo width $w]-($Notebook($w,pad)*2+4)]
  set hi [expr [winfo height $w]-($Notebook($w,pad)*2+36)]
  Notebook:config $w -width $wi -height $hi
  catch {unset Notebook($w,expand)}
}

#
# Locate minimum dimensions of frame and expand to it
#

proc Notebook:resize w {
   global Notebook
   
   update
   set minWid [expr [winfo width $w]-($Notebook($w,pad)*2+4)]
   set minHgt [expr [winfo height $w]-($Notebook($w,pad)*2+36)]
   foreach pg $Notebook($w,pages) {
      set frm [Notebook:frame $w $pg]
      if [winfo exists $frm] {
         set hgt [winfo reqheight $frm]
         set wid [winfo reqwidth  $frm]
         if { $hgt>$minHgt } { set minHgt $hgt }
         if { $wid>$minWid } { set minWid $wid }
      }
   }
   Notebook:config $w -width $minWid -height $minHgt
}

proc Notebook:delete {w page} {
  global Notebook
  global NotebookTerm

  if { [llength $Notebook($w,pages)] <= 1 } {
     # at least one tab
     return
  }

  $NotebookTerm($w,$page) shutdown

  set idx [lsearch -exact $Notebook($w,pages) $page]
  set Notebook($w,pages) [lreplace $Notebook($w,pages) $idx $idx]
  set Notebook($w,text) [lreplace $Notebook($w,text) $idx $idx]

  $w delete p$idx
}
