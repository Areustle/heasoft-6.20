
if {[string equal $browser "open"]} {
   exec open $fileLoc
} elseif {[string equal $browser "herahtmlbrowser"]} {
   puts "herahtmlbrowser $fileLoc"
} elseif {[string equal $browser "cygstart"]} {
   cygstart $fileLoc
} else {
# If browser is not already open remote command will fail,
# in which case catch block should swallow error message.
   if {$online == 1} {
      if {[catch {exec $browser -remote "openurl($fileLoc)"} result]} {
         exec $browser $fileLoc &
      }
   } else {
      if {[catch {exec $browser -remote "openFile($fileLoc)"} result]} {
         exec $browser $fileLoc &
      }      
   }
} 
