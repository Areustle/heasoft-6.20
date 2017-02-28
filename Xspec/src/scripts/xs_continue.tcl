## xs_continue.tcl - xspec internal script for grabbing continuation line from prompt
##

gets stdin continuation;

while { [string index $continuation [expr [string length $continuation] - 1]  ] == "-"}  {
        set continuation [string trim [string trimright $continuation '-']];
        gets stdin tmp;
        append continuation " " $tmp;
}

return $continuation;
