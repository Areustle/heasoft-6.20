proc close args {
#
#  If no args run close_pg_window else run tclclose
#
   if { [llength $args] == 0 } {
      return [close_pg_window]
   } else {
      return [eval tclclose $args]
   }
}
