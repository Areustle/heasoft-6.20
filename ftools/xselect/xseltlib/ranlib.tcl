#!/software/lheasoft/develop/OSF1_4.0_alpha/bin/tclsh
set b [ pwd ]
pkg_mkIndex $b xsel_help.tcl xsel_menu.tk xsel_help.tk xsel_file.tk \
            xsel_miss.tcl  xsel_ctrl.tk  xsel_plot.tk xsel_learn.tk
puts $tcl_version

