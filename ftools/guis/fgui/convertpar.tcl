# call by "tclsh convertpar.tcl <all | ftools | chandra | xmm-sas>
# convertField.txt must be presented
 
proc createField { contents f targetTokenList specialCase specialValue } {
     set specialTokenList {}
     if { $specialCase != "NONE" } {
        set specialTokenList [split $specialCase ","]
     }

     foreach line $contents {
         set line [string trim $line]
         if { [string length $line] <= 0 } continue
         if { [string range $line 0 0] == "#" } continue
         set token [split $line ","]
         set paramName [lindex $token 0]
         set idx [lsearch -exact $targetTokenList $paramName]
         if { $idx >= 0 } {
            set action "I"
         } else {
            if { [lsearch -exact $specialTokenList $paramName] >= 0 } {
               set action "I"
            } else {
               if { [string first "file" $paramName] >= 0 } {
                  set action "F"
               } else {
                  set action "NONE"
               }
            }
         }     

         set idx [lsearch -glob $specialValue [list $paramName *]]
         if { $idx >= 0 } {
            set value [lindex [lindex $specialValue $idx] 1]
         } else {
            set value "NONE"
         }

         # puts $f [format "%s|%s|%s" $paramName $action $value]
         puts $f [format "%s|%s" $paramName $action]
     } 
}

global env 

set f [open convertField.txt r]
set targetTokenList [split [read $f [file size convertField.txt]] "\n"]
close $f

set f [open specialCase.txt r]
set specialTokenFileList [split [read $f [file size specialCase.txt]] "\n"]
close $f

set specialTokenList {}
foreach lineToken $specialTokenFileList {
    set token [split $lineToken "|"]
    lappend specialTokenList $token
}

set f [open specialValue.txt r]
set specialTokenValueList [split [read $f [file size specialValue.txt]] "\n"]
close $f

set specialValueList {}
foreach lineToken $specialTokenValueList {
    set token [split $lineToken "|"]
    lappend specialValueList $token
}

set paramToken [list [list "ftools" "$env(HEADAS)/syspfiles" "$env(HEADAS)/fguipfiles"] \
                     ]

set targetParamToken $paramToken
if { [llength $argv] == 1 && [lindex $argv 0] != "all" } {
   set targetTool [lindex $argv 0] 
   set idx [lsearch -glob $paramToken [list $targetTool * *]]
   if { $idx < 0 } {
      puts "No such target: $targetTool"
      exit
   } 
   set targetParamToken [list [lindex $paramToken $idx]]
}
    
foreach tokenList $targetParamToken {
    set sourceDir [lindex $tokenList 1]
    set targetDir [lindex $tokenList 2]
    catch { file delete -force $targetDir }
    catch { file mkdir $targetDir }
    set fileNameList [eval glob -directory $sourceDir -nocomplain "*.par"]
    foreach fileName $fileNameList {
        set idx [lsearch -glob $specialTokenList [list $targetDir [file tail $fileName] *]]

        set f [open $fileName "r"]
        set contents [split [read $f [file size $fileName]] \n]
        close $f

        set f [open "$targetDir/[file tail $fileName]2" "w+"]

        if { $idx < 0 } {
           createField $contents $f $targetTokenList "NONE" $specialValueList
        } else {
           createField $contents $f $targetTokenList [lindex [lindex $specialTokenList $idx] 2] $specialValueList
        }
        close $f
    }
}
