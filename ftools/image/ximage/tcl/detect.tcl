#
#  Emulate old DETECT command
#  Runs BACKGROUND, EXCESS, SEARCH
#
proc detect {args} {
   parseparm [parmloc detect] $args

   if { $cmdargc != 0 } {
      txwrite " Wrong number of arguments: $cmdargv" 10
      error {}
   }

#
# background command
#
   if { $parval(back_box_size) != "" } {
      set backgroundArgs "box_size=$parval(back_box_size)"
   } else {
      set backgroundArgs "optimize"
   }
   if { $parval(draw_rej_back_boxes) } { 
      append backgroundArgs " draw_rej_back_boxes" 
   }
   if { $parval(flat_value) != "" } { 
      append backgroundArgs " flat_value=$parval(flat_value)" 
   }
#
# excess command
#
   set excessArgs ""
   if { $parval(bright) } { set excessArgs " bright" }
   if { $parval(plot_excesses) } { set excessArgs " plot_excesses" }
   if { $parval(thr_scaling) != "" } { 
      append excessArgs " thr_scaling=$parval(thr_scaling)" 
   }
   if { $parval(source_box_size) != "" } { 
      append excessArgs " source_box_size=$parval(source_box_size)" 
   }
#
# search command
#
   set searchArgs ""
   if { $parval(snr_threshold) != "" } { 
      append searchArgs " snr_threshold=$parval(snr_threshold)" 
   }
   if { $parval(prob_limit) != "" } { 
      append searchArgs " prob_limit=$parval(prob_limit)" 
   }
   if { $parval(nolabel) } { set searchArgs " nolabel" }
   if { $parval(filedet) != "" } { 
      append searchArgs " filedet=$parval(filedet)" 
   }
   if { $parval(fitsdet) != "" } { 
      append searchArgs " fitsdet=$parval(fitsdet)" 
   }
   if { $parval(color) != "" } { append searchArgs " color=$parval(color)" }
   if { $parval(lwidth) != "" } { append searchArgs " lwidth=$parval(lwidth)" }
   if { $parval(lstyle) != "" } { append searchArgs " lstyle=$parval(lstyle)" }
   if { $parval(csize) != "" } { append searchArgs " csize=$parval(csize)" }
   if { $parval(font) != "" }  { append searchArgs " font=$parval(font)" }

#
# Run the XIMAGE commands
#
   txwrite "background $backgroundArgs" 20
   eval background $backgroundArgs
   txwrite "excess $excessArgs" 20
   eval excess $excessArgs
   txwrite "search $searchArgs" 20
   eval search $searchArgs
}
