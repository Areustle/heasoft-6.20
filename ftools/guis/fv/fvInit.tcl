global env

if {$tcl_platform(platform) == "windows"} {
    set fullname [info nameofexecutable]
    set home [file dirname $fullname]
    set env(FV_HOME) [file dirname $home]
    set libdir [file join $env(FV_HOME) lib]
    set env(FITSVIEWER_LIBRARY) [file join $libdir fv]
}


# Add auto_load path
lappend auto_path [file join "$env(FITSVIEWER_LIBRARY)" class]    

# Add pathes for help files.
set env(FV_HELPDIR) [file join "$env(FITSVIEWER_LIBRARY)" doc]

eval fvInit $argv

