global env
global auto_path


set fullname [info nameofexecutable]
set home [file dirname $fullname]
set env(FV_HOME) [file dirname $home]
set libdir [file join $env(FV_HOME) lib]
set env(FITSVIEWER_LIBRARY) [file join $libdir fv]
set env(FV_HELPDIR) [file join $env(FITSVIEWER_LIBRARY) doc]


# Add auto_load path
lappend auto_path [file join "$env(FITSVIEWER_LIBRARY)" class]    

# Add pathes for help files.
set env(FV_HELPDIR) [file join "$env(FITSVIEWER_LIBRARY)" doc]


