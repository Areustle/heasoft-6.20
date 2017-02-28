proc tcloutr args {
   global xspec_tclout
   eval "tclout $args"
   return $xspec_tclout
}
