proc screengrab {args} {
#
#  Dump screen and crop with viewer
#  Valid viewers: xv, display, convert
#
   parseparm [parmloc screengrab] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }

   if { ![info exists ::mainpg] } {
      error { No /xtk device available for screen grab}
   }
   global mainpg
   if { ![tk::winfo viewable $mainpg] } {
      error { No /xtk device viewable for screen grab}
   }

   set viewer ""
   if { $parval(display) } { set viewer display }
   if { $parval(convert) } { set viewer convert }
   if { $parval(xv) } { set viewer xv }
   set outfile $parval(outfile)
#
#  Defaults
#
   if { $viewer == "" } { 
      txwrite " No cropping method selected, assuming convert..." 10
      set viewer convert
   }
   if { $outfile == "" } { set outfile pgplot.gif }

   set window [tk::winfo toplevel $mainpg]
   set dumpfile pgplot.xwd

   if {![tk::winfo exist $window]} {
       return -code error "window $window doesn't exist"
   } else {
       tk::raise $window
       update
   }
   # Check file creation
   if {[catch {open $dumpfile w} msg]} {
       global errorCode errorInfo
       return -code error -errorcode $errorCode -errorinfo $errorInfo $msg
   }
   ::close $msg
   set id [tk::winfo id $window]
   
   set cmd "syscall xwd -id $id -out $dumpfile"
   txwrite $cmd 15
   eval $cmd
   set grabwid [winfo width $mainpg]
   set grabhgt [winfo height $mainpg]
   if { $viewer == "xv" } {
      set cmd "syscall xv -crop 0 0 $grabwid $grabhgt $dumpfile &"
      txwrite $cmd 15
      eval $cmd
   } elseif { $viewer == "display" } {
      set cmd "syscall display -crop ${grabwid}x${grabhgt}+0+0 $dumpfile &"
      txwrite $cmd 15
      eval $cmd
   } elseif { $viewer == "convert" } {
      set cmd "syscall convert -crop ${grabwid}x${grabhgt}+0+0 $dumpfile $outfile"
      txwrite $cmd 15
      eval $cmd
      return "Output image: $outfile"
      set cmd "syscall rm $dumpfile"
      txwrite $cmd 15
      eval $cmd
   } else {
      txwrite " Invalid viewer ($viewer)" 10
      txwrite " Load $dumpfile into image viewer manually" 10
   }
}
