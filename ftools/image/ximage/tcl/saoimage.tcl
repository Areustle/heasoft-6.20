#
#  Emulate SAOIMAGE command
#  Runs WRITE_IMAGE, then spawns saoimage 
#
proc saoimage {args} {
   parseparm [parmloc saoimage] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }
#
# Write command
#
   set writeArgs ""
   if { $parval(template) != "" } { 
      append writeArgs " template=$parval(template)" 
   } 
   if { $parval(filename) != "" } { 
      set tmpfile $parval(filename)
      if ![regexp {\.} $tmpfile] {
         set tmpfile "$tmpfile.img"
      }
   } else {
      set tmpfile sao_temp.img 
   } 
   append writeArgs " file=$tmpfile" 
#
# Delete file if exists
#
   if { [file exists $tmpfile] } {
      file delete $tmpfile
   }
#
# Write temporary file
#
   txwrite "write_image $writeArgs" 20
   eval write_image $writeArgs
#
# Spawn saoimage
#
   puts "Spawning saoimage..."
   syscall saoimage -fits $tmpfile &
}
