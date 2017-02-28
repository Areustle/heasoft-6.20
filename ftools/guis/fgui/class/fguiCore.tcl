proc fguiInit { args } {
     global g_ftoolflag
     global g_titleFont
     global tcl_platform
     global isWin isMac

     # Load Itcl/Itk packages

     wm withdraw .

     package require Itcl
     package require Itk
     package require Iwidgets

     set isWin 0
     set isMac 0
     if { $tcl_platform(platform) == "windows" } {
        set isWin 1
        font create g_titleFont -family Arial             -size -12
        font create g_notebookTitleFont -family Arial     -size -14
     } elseif { $tcl_platform(platform) == "macintosh" } {
        set isMac 1
        font create g_titleFont -family system    -size -12
        font create g_notebookTitleFont -family system -size -14
     } else {
        font create g_titleFont -family Helvetica -size -12 -weight bold
        font create g_notebookTitleFont -family Helvetica -size -14 -weight bold
     }
 
     font create g_entryFont -family Courier -size -12
     set g_charPix      [font measure g_entryFont m]

     # These lines guarantee that the default fonts are always loaded.
     entry .dummyFnt1
     entry .dummyFnt2 -font g_entryFont
     entry .dummyFnt3 -font g_titleFont
     entry .dummyFnt4 -font g_notebookTitleFont

     if ![info exists g_ftoolflag ] {
        set g_ftoolflag 1
        FtoolInstance fguiFtool
     }
     catch { fguiFtool MainMenu } err
}

set ::g_fguiHelpFiles [list \
      "FTOOL Execution"        ftool.html            \
      ]

proc hhelp {topic_} {
  global g_fguiHelpFiles
  global isWin
  global env

  if { [string match "*.html" $topic_] } {
     set topic_ [string range $topic_ 0 end-5]
  }

  if { [winfo exist .hyperHelp] == 0} {
      if { $isWin } {
         set size large
      } else {
         set size medium
      }
      set allTopics {}
      foreach [list aTitle aTopic] $g_fguiHelpFiles {
         lappend allTopics [list $aTitle $aTopic]
      }
      iwidgets::hyperhelp .hyperHelp -title "fgui: Hyperhelp" \
            -topics $allTopics \
            -fontname courier -fontsize $size \
            -helpdir     $env(FGUI_HELPDIR)
  }
    .hyperHelp showtopic $topic_
    catch {.hyperHelp activate}
}

