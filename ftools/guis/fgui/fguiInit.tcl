global env

if {$tcl_platform(platform) == "windows"} {
    set fullname [info nameofexecutable]
    set home [file dirname $fullname]
    set env(FGUI_HOME) [file dirname $home]
    set libdir [file join $env(FGUI_HOME) lib]
    set env(FITSVIEWER_LIBRARY) [file join $libdir fgui]
}

set env(P2FILES) [file join $env(LHEASOFT) fguipfiles]
# Add auto_load path
lappend auto_path [file join "$env(FITSVIEWER_LIBRARY)" class]    

# Add pathes for help files.
set env(FGUI_HELPDIR) [file join "$env(FITSVIEWER_LIBRARY)" doc]

eval fguiInit $argv

