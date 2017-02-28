proc read args {
#
#  If channelId run tclread, else run read_image
#
   set arg1 [lindex $args 0]
   if { $arg1 == "-nonewline" ||
        ![catch {fconfigure $arg1}] } {
      return [uplevel tclread $args]
   } else {
      return [uplevel read_image $args]
   }
}
