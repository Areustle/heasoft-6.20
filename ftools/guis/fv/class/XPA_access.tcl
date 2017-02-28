#
#  This file contains code which makes fv more scriptable either from
#  TCL or via XPA entry points
#


namespace eval fvCmds {
   variable currObj ""
   variable currHDU 1

   variable winList {}


   proc open { args } {

      global g_listObjs
      variable currObj
      variable currHDU


      if { [llength $args]==0 } {
	 # Return list of opened files
	 set files ""
	 foreach file $g_listObjs {
	    append files "[$file getOrigName]\n"
	 }
	 return  $files
      }

      foreach file $args {
         set errorFlag [ catch { 
             set currObj [openFitsFile $file] 
         } err ]

         if { $errorFlag } {
            error " Can't open file: $file.\n"
         }
      }
      set currHDU 1
   }


   proc close { args } {
      variable currObj

      if {[catch {checkForCurrentFile } err ]} {
           exitCmd
           return
      }
      set argc [llength $args]
      if { $argc==0 } {
         $currObj closeCmd
      } elseif {[lindex $args 0] == "-1" } {
         set numexts [$currObj getNumHdus]
         for {set i 1} { $i <= $numexts} { incr i} {
            $currObj closeExtension $i
         }
      } else {
         foreach ext $args {
            if { $ext == "current" || $ext == "." } {
               set ext [expr $currHDU - 1]
            }
            set numexts [$currObj getNumHdus]
            if { $ext < 0 || $ext >= $numexts } {
              error "valid extension:0-[expr $numexts -1]\n"
              return
            }
                
            if {[catch {$currObj closeExtension [expr $ext+1] } err ]} {
              error "close extension error\n"
            }
         }
      }
   }

   proc help { args } {
        set displayList [list close create delete display export exporttable open opentool pow preference quit save select sort tcl minimize version]

        if { $args != "" } {
           set displayList $args
        }


        set result ""
        

        foreach func $displayList {
            switch $func {
                 help {
                    append result "close usage: xpaget help cmd..\n"
                 }
                 close {
                    append result "close usage: xpaset -p fv close <Extension>..\n"
                 }
                 create {
                    append result "create usage:\n"
                    append result "        file: xpaset -p fv create file <name> <signed/unsigned> <bitpix> <dimension>\n"
                    append result "       image: xpaset -p fv create extension <name> image <signed/unsigned> <bitpix> <dimension>\n"
                    append result "                                            <name> table <Binary/ASCII>\n"
                    append result "       example of dimension: 20 30, 20 20 20\n"
                 }
                 display {
                    append result "display usage:\n"
                    append result "      header: xpaset -p fv display header <Extension>\n"
                    append result "       table: xpaset -p fv display table <Extension> <columnName> <->\n"
                    append result "       image: xpaset -p fv display image <Extensions>\n"
                    append result "       curve: xpaset -p fv display curve <Extension> -rows|-cols <start> <end> <Y/N>\n"
                    append result "       curve: xpaset -p fv display curve <Extension> <X> <xErr> <Y> <yErr> <Y/N> <rowrange>\n"
                 }
                 export {
                    append result "export usage: xpaset -p fv export <option> <extension> ?<outfile>?\n"
                    append result "       example of option: hdu, summary\n"
                    append result "       example of extension: 0,1,3 4\n"
                 }
                 exporttable {
                    append result "exporttable usage: xpaset -p fv exporttable <fileName> <option> <extension>\n"
                    append result "            example of option: hdu, text\n"
                    append result "            example of extension: 0,1,3 4\n"
                 }
                 open {
                    append result "open usage: xpaset -p fv open <fileName>...\n"
                    append result "open usage: xpaget fv open\n"
                 }
                 opentool {
                    append result "opentool usage: xpaset -p fv opentool <value>\n"
                    append result "        possible value: skyview, catalog, vizier, ftools\n"
                 }
                 pow {
                    append result "pow usage: xpaset -p fv pow <powCommand>...\n"
                 }
                 preference {
                    append result "preference usage: xpaset -p fv preference <parameter> <value>\n"
                    append result "         parameter              possible value\n"
                    append result "         ===================    =====================================\n"
                    append result "    color choice\n"
                    append result "         activebackground       red, green, blue, etc\n"
                    append result "         activeforeground       red, green, blue, etc\n"
                    append result "         background             red, green, blue, etc\n"
                    append result "         checkbuttoncolor       red, green, blue, etc\n"
                    append result "         foreground             red, green, blue, etc\n"
                    append result "\n    graph display\n"
                    append result "         displayer              ds9 pow saotng\n"
                    append result "         graphsize              300x300 500x300 300x500 500x500 700x500 500x700 700x700\n"
                    append result "\n    preference window\n"
                    append result "         open                   N/A\n"
                    append result "         close                  N/A\n"
                    append result "\n    desktop manager help\n"
                    append result "         help                   N/A\n"
                    append result "\n    options\n"
                    append result "         autoupdatechecksum     0 - never, 1 - if exist, 2 - always\n"
                    append result "         dispfitsonly           0 - deselect, 1 - select\n"
                    append result "         autoplotprimary      0 - deselect, 1 - select\n"
                    append result "         autoreformatkeys     0 - deselect, 1 - select\n"
                    append result "         leftjustifystring    0 - right, 1 - left\n"
                    append result "         protectedkeys        0 - deselect, 1 - select"
                    append result "         usedesktopmanager    0 - deselect, 1 - select\n"
                    append result "         wcsinfo              0 - deselect, 1 - select\n"
                    append result "         writehiskey          0 - deselect, 1 - select\n"
                    append result "         winmanagement          Keep, Hide, Close\n"
                 }
                 quit {
                    append result "quit usage: xpaset -p fv quit\n" 
                 }
                 save {
                    append result "save usage: xpaset -p fv save <file Name>\n" 
                 }
                 select {
                    append result "select usage: xpaset -p fv select <file Index/file Name>\n" 
                 }
                 sort {
                    append result "sort usage: xpaset -p fv sort extNum ?-unique? -ascending|-descending ?-select? <column name>\n"
                 }
                 plot {
                    append result "sort usage: xpaset -p fv plot extNum \n"
                 }
                 delete {
#                    append result "delete usage: xpaset -p fv delete extnum <-row ranges > <-col colnames>\n"
                    append result "delete usage: xpaset -p fv delete rows <extnum>  <ranges> \n"
                    append result "delete usage: xpaset -p fv delete cols <extnum>  <colnames>\n"
                 }
                 insert {
#         append result "insert usage: xpaset -p fv insert extnum <-row afterrow  rownum> <-col beferecol name form>\n"
         append result "insert usage: xpaset -p fv insert rows <extnum> <afterrow>  <numRows>\n"
         append result "insert usage: xpaset -p fv insert cols <extnum> <colname> <colform> ?<beforeCol>?\n"
                 }
                 tcl {
                    append result "tcl usage: xpaset -p fv tcl <tcl code>\n" 
                 }
                 version {
                    append result "version usage: xpaget fv version\n" 
                 }
                 default {
                 }
            }
        }
        return $result
   }

   proc getKeyword {fFile keyword} {
        catch { set data [$fFile get keyword] } err

        set returnList {}
        foreach str $data {
            set tokenStr [string trim [lindex $str 0]]
            set valueStr [lindex $str 1]
            if { $keyword == $tokenStr } {
               lappend returnList [string trim $valueStr {' }]
            }
        }

        return $returnList
   }
   proc exporttable { args } {
       global isWin
       global isMac

       global g_listObjs
       global g_histoFileID
       global g_backupDir

       variable currObj
       variable currHDU


       set result ""

       if { [llength $args] < 3 } {
          error "invalid option. type 'xpaget fv help exporttable' for help\n"
          return
       }
#       catch { set tmpfitsfile [fits open $openedFileName 0] } err

       set openedFileName [lindex $args 0]
       set option    [lindex $args 1]
       set ext [lindex $args 2]

       if {!($option =="text" || $option =="hdu")} {
                  error "an example option:text, hdu\n"
                  return
       }

       set currObj [openFitsFile $openedFileName]

      
       set currHDU [expr $ext+1]

#       file copy -force $fileName $::env(HOME)/.fv/[file tail $fileName]
#       catch { set openedFileName $::env(HOME)/.fv/[file tail $fileName] } err


       set fT [$currObj openTable $currHDU [lrange $args 3 end] ] 
       switch $option {
              "text" {
                  $fT trySaveASCII 
               }
              "hdu" {
                  $fT tryExport
               }
               default {
                  error "an example option:text, hdu\n"
               }
       }
                 
            
   }

   proc export { args } {
       global isWin
       global isMac
       global g_backupDir
       variable currObj
       variable currHDU


       set result ""

       if { [llength $args] < 2 } {
          error "invalid arguments. type 'xpaget fv help export' for help"
       }

      if {[catch {checkForCurrentFile } err ]} {
          error "no file  was opened"
          return
      }
      set fileName [$currObj getOrigName]
#       set fileName  [lindex $args 0]
       set option    [lindex $args 0]
       set extension [lindex $args 1]
       set outfile [lindex $args 2]



       if { $extension != "all" } {
#          set extension [string range $args [expr [string length $option] + 1] end]
          set extList [split $extension " "]
       }

       file copy -force $fileName $g_backupDir/[file tail $fileName]
       catch { set openedFileName $g_backupDir/[file tail $fileName] } err

       switch $option {
              "hdu" {
                  set t [urlTail $fileName]
                  set r [file root $t]
                  set e [file ext $t]
                  if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
                     set e [file ext $r]
                     set r [file root $r]
                  } 
                  
                  if { $outfile == "" } {
                  set saveFileName "${r}_hdu${e}"
                  } else {
                  set saveFileName $outfile
                  }
                  if { [file exist $saveFileName] } {
                     set feedback [promptMsg "File $saveFileName exists\n overwrite?" \
                                             return Yes No]
                     if { $feedback == "CANCEL" } return
                  } 
               
                  catch { file delete $saveFileName } err
                  catch { set tmpfitsfile [fits open $openedFileName 0] } err
#                  set tmpfisfile $currObj
               
                  set nhdus [$tmpfitsfile info nhdu]
                  set newIdx 0


                  for {set i 0} {$i < $nhdus} {incr i} {
                      # means to move to current HDU
                      if { $extension != "all" } {
                         set idx [lsearch $extList $i]
                      } else {
                         set idx $i
                      }

              catch { $tmpfitsfile move [expr $i + 1]} err
#                      catch { $tmpfitsfile move $idx } err

                      if { $idx >= 0 } {
                         if { $newIdx == 0 } {
                            catch { $tmpfitsfile copy $saveFileName } err
                         } else {
                            catch { $tmpfitsfile append $saveFileName } err
                         }
                         incr newIdx
                      }
                      
                  }
               
                  $tmpfitsfile close
                  
                  # copy permissions of original but make writeable
                  if { $isWin } {
                     file attributes $saveFileName -system $filePermission
                     file attributes $saveFileName -readonly 0
                     set filePermission [file attributes $saveFileName -system]
                  } elseif { $isMac } {
                      # Make file fvEd/FITS type
                      file attributes $saveFileName -creator "fvEd" -type "FITS"
                  } else {
                     set filePermission "00644"
                     file attributes $saveFileName -permissions $filePermission
                  }
                  
                  set currObj [openFitsFile $saveFileName]

              }
              "summary" {
                  set t [urlTail $fileName]
                  set r [file root $t]
                  set e [file ext $t]
                  if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
                     set e [file ext $r]
                     set r [file root $r]
                  }

                  file copy -force $fileName $g_backupDir/[file tail $fileName]
                  catch { set openedFileName $g_backupDir/[file tail $fileName] } err

                  catch { set tmpfitsfile [fits open $openedFileName 0] } err
                  catch  { set currentHDU [$tmpfitsfile info chdu] } err

                  set curext 0

                  set extensionList  {}
                  set typeList       {}
                  set dimList        {}
                  while {1} {

                      incr curext
                      if {[catch {$tmpfitsfile move $curext} err]} {
                          #we're done.
                          break
                      }
                      catch { set ext [eval getKeyword $tmpfitsfile "EXTNAME"] } err

                      if { $ext == "" } {
                         lappend extensionList "NoName"
                      } else {
                         lappend extensionList $ext
                      }

                      catch { lappend typeList [$tmpfitsfile info hdutype] } err

               	   if { [$tmpfitsfile info hdutype] =="Image extension" } {
                      set errFlag [ catch { 
                          set data [$tmpfitsfile info imgdim] 
                      } err ]
                     
                      if { !$errFlag && $data != "" } {
                         catch { lappend dimList $data } err
                      } else {
                         catch { lappend dimList [list 0] } err
                      }
                    } else {
                         set data [$tmpfitsfile info ncols]
                         set data1 [$tmpfitsfile info nrows]
                         lappend dimList "$data $data1" 
                     }
                  }

                  set _numExts [llength $extensionList]

                  $tmpfitsfile close
               
                  if { $outfile =="" } {
                  set saveFileName "${r}_hdu.txt"
                  } else {
                  set saveFileName $outfile
                  }

                  if { [file exist $saveFileName] } {
                     set feedback [promptMsg "File $saveFileName exists\n overwrite?" \
                           return Yes No]
                     if { $feedback == "CANCEL" } return
                     catch { file delete -force $saveFileName } err
                  }

                  # we have to use "::open" because there is an open command in the namespace
                  catch { set f [::open $saveFileName "w"] } err

                  puts $f [format "%-5s   %-30s   %-10s   %-20s" Index Extension Type Dimension]
                  puts $f [format "%-5s   %-30s   %-10s   %-20s" "=====" \
                                                                 "==============================" \
                                                                 "==========" \
                                                                 "===================="]



                  for {set i 0} {$i < $_numExts} {incr i} {
                      set idx $i
                      if { $extension != "all" } {
#                         set idx [lsearch $extList [expr $i + 1]]
                         set idx [lsearch $extList $i ]
                      }

                      if { $idx >= 0 } {
                         set token [lindex $dimList $i]
                         set j 0
                         set result ""
                         foreach dim $token {
                             if { $j == 0 } {
                                set result [format "%s" $dim]
                             } else {
                                set result [format "%sx%s" $result $dim]
                             }
                             incr j
                         }
                         set data_type [lindex [split [lindex $typeList $i] " "] 0]
                         if { $i != 0 } {
                         puts $f [format "%-5s   %-30s   %-10s   %-20s" $i \
                                                [lindex $extensionList $i] \
                                                $data_type $result]
                         } else {
                         puts $f [format "%-5s   %-30s   %-10s   %-20s" $i \
                                                "PRIMARY" \
                                                $data_type $result]
                         }

                      }
                  }

                  # again we need "::"
                  ::close $f

                  # copy permissions of original, but make read/write
                  set filePermission "00644"
#                  file attributes $saveFileName_ -permissions $filePermission
                  file attributes $saveFileName -permissions $filePermission
              }
              deafult {
                 error "example of option: hdu, summary"
              }
       }
   }



   proc create { args } {
       variable currObj

       global isMac
       global g_listObjs

       set createType [lindex $args 0]
       set typeName   [lindex $args 1]


       
       switch $createType {
              "file" {
                  if { [llength $args] < 5 } {
                       error "invalid usage of create. for help type 'xpaget fv help create'\n"
                       return
                  }

                  set option    [lindex $args 2]
                  set bitpix    [lindex $args 3]
                  set dim [string range $args [string first [lindex $args 4] $args] end]
           
                  if { [string tolower $option] != "unsigned" && [string tolower $option] != "signed" } {
                     error "valid option: Unsigned, Signed\n"
                     return
                  }
           
                  if { $bitpix != 8 && $bitpix != 16 && $bitpix != 32 && $bitpix != 64 } {
                     error "valid bitpix: 8 16 32 64"
                     return
                  }
           
                  if { [file exist $typeName ] == 1 } {
        set feedback [promptMsg "File $typeName exists\n Do you want to overwrite?" return Yes No]
                    if { $feedback == "CANCEL" } return
                  }
           
                  if [file exist $typeName] {
                     file delete -force $typeName
                  }
           
                  set filemode 2
                  set token [split $dim " "]
                  set _naxes [llength $token]
            
                  set _axis_list {}
            
                  for {set i 0} {$i < $_naxes} {incr i} {
                      lappend _axis_list [lindex $token $i]
                  }

            
                  if { [catch {set newfits [fits open $typeName $filemode]} err] } {
                      error $err
                      return
                  }


            
                  if { [catch { $newfits put ihd -p $bitpix $_naxes $_axis_list} err] } {
                      error $err
                      return
                  }

                  if { ([string tolower $option]=="unsigned" && $bitpix!=8) || ([string tolower $option]=="signed" && $bitpix==8) } {
                      # Write keywords to offset values
                     switch $bitpix {
                        8 {
                           set bzero -128
                        }
                        16 {
                           set bzero 32768
                        }
                        32 {
                           set bzero 2147483648
                        }
                     }
                     $newfits put keyword "BZERO  $bzero  Make values $option"
                     $newfits put keyword "BSCALE 1       Make values $option"
                  }
                  $newfits close
                  if { $isMac } {
                     file attributes $typeName -creator "fvEd" -type "FITS"
                  }
           
                 set currObj [openFitsFile $typeName]
              }
              "extension" {

                  set fileName  [lindex $args 2]
                  if { [string tolower $fileName] == "image" || [string tolower $fileName] == "table" } {
                     set extType   [lindex $args 2]
                     set option    [lindex $args 3]
                     set fileName  ""
                     set currentfileName 1
                  } else {
                     set extType   [lindex $args 3]
                     set option    [lindex $args 4]
                     set currentfileName 0
                  }

                  set errflag 0

                  if { $fileName != "" } {
                      if { [string tolower $extType] != "image" && [string tolower $extType] != "table" } {
                            set errflag 1
                      } elseif { ([string tolower $extType] == "image" && [llength $args] < 7) } {
                            set errflag 1
                      } elseif { ([string tolower $extType] == "table" && [llength $args] < 5 )} {
                            set errflag 1
                      } 
                  } elseif { [string tolower $extType] != "image" && [string tolower $extType] != "table" } {
                            set errflag 1
                  } elseif { [string tolower $extType] =="image" && [llength $args] < 6} {
                            set errflag 1
                  } elseif { [string tolower $extType] == "table" && [llength $args] < 4} {
                            set errflag 1
                  }
                     
                  if { $errflag == 1} {
                      error "invalid usage of create. type 'xpaget fv help create' for help\n"
                      return
                  }

                  if { [catch {checkForCurrentFile} err ] } {
                        if { $currentfileName == 1 } {
                            error "check for current file error:$err\n"
                            return
                        }
                  }

                  if { $currentfileName == 1 } {
#                     set fileName [$currObj getOrigName]
                     set fileName [$currObj getBackup]
                  }
                     

                  if ![file exists $fileName] {
                     if { $fileName !="" } {
                     error "file $fileName does not exists.\n"
                     return
                     } else {
                     error "current file does not exists.\n"
                     return
                     }
                
                  }

                  set filePermission "00644"
                  file attributes $fileName -permissions $filePermission
                  set filemode 1
                  if { [catch {set newfits [fits open $fileName $filemode]} err] } {
                     error $err
                     return
                  }

                  if { $extType == "image" } {
                     set bitpix    [lindex $args 5]
                     set dim [string range $args [string first [lindex $args 6] $args] end]
                     if { $currentfileName == 1 } {
                        set bitpix    [lindex $args 4]
                        set dim [string range $args [string first [lindex $args 5] $args] end]
                     }
 
                     if { [string tolower $option] != "unsigned" && [string tolower $option] != "signed" } {
                        error "valid option: unsigned, signed"
                        return
                     }
           
                     if { $bitpix != 8 && $bitpix != 16 && $bitpix != 32 && $bitpix != 64 } {
                        error "valid bitpix: 8 16 32 64"
                        return
                     }

                     set token [split $dim " "]
                     set _naxes [llength $token]

                     set _axis_list {}

                     for {set i 0} {$i < $_naxes} {incr i} {
                         lappend _axis_list [lindex $token $i]
                     }

                     catch { $newfits move [$newfits info nhdu] } err
                     catch { $newfits put ihd $bitpix $_naxes $_axis_list } err


                     if { ([string tolower $option]=="unsigned" && $bitpix!=8) || \
                          ([string tolower $option]=="signed" && $bitpix==8) } {
                         # Write keywords to offset values
                        switch $bitpix {
                           8 { 
                              set bzero -128
                           }
                           16 {
                              set bzero 32768
                           }
                           32 {
                              set bzero 2147483648
                           }
                        }
                        catch { $newfits put keyword "BZERO  $bzero  Make values $option" } err
                        catch { $newfits put keyword "BSCALE 1       Make values $option" } err
                     }

                    $newfits put keyword "EXTNAME $typeName       Extension Name"
                    $newfits close 


                    if { $currentfileName == 1 } {
                        $currObj redrawHighLight
                    } else {
                        set currObj [openFitsFile $fileName]
                    }

                  } else {
                     if { [string tolower $option] != "binary" && [string tolower $option] != "ascii" } {
                        error "valid option: Binary, ASCII"
                        return
                     }

                     catch { $newfits move [$newfits info nhdu] } err

                     if { [string tolower $option] == "ascii" } {
                        $newfits put ahd 1 0 {} {} {} {} "$typeName" 0
                     } else {
                        $newfits put bhd 1 0 {} {} {} "$typeName"
                     }
                     $newfits close
                     if { $currentfileName == 1 } {
                        $currObj redrawHighLight
                     } else {
                        set currObj [openFitsFile $fileName]
                     }

                  }
              }
              default {
                 error "invalid usage of create. for help type 'xpaget fv help create' \n" 
                 return
              }
       }
   }

   proc display { args } {
      global g_listObjs
      global g_histoFileID
      global g_backupDir

      variable currObj
      variable currHDU
      variable presetParamList

      checkForCurrentFile
      if { [llength $args]<2 } {
         error "Usage: display header|table|image|curve|histogram extNum ?opts?"
      }


      set ext [lindex $args 1]


      if { !($ext == "current" || $ext == ".") } {
      	set extnum [$currObj getNumHdus]
      	if { $ext < 0 || $ext > [expr $extnum -1] } {
          error "valid extension:0-[expr $extnum -1]"
          return
      	}
         set currHDU [expr $ext+1]
      } else {
         lset args 1 [expr $currHDU -1]
      }

      

      switch -- [lindex $args 0] {
         "header" {
            $currObj openHeader $currHDU
         }
         "table" {
            $currObj openTable  $currHDU [lrange $args 2 end] 1
         }
         "image" {
            set hdu [lrange $args 1 1]
            set slice [lrange $args 2 end]

            foreach idx $hdu {
                set type [$currObj getHDUtype [expr $idx + 1]]

                if { [string tolower $type] != "image" && [string tolower $type] != "unknown" } {
                   error "input HDU [lrange $args end end] is not an image type"
                } elseif { [string tolower $type] == "unknown" } {

                } else {
                   $currObj plotData [expr $idx +1] $slice
                }
            }
         }
         "curve" {
            $currObj plotData $currHDU [lrange $args 2 end]
         }
         "histogram" {
            set nameList [eval glob -directory {$g_backupDir} -nocomplain "histo.tmp*"]

            set previousID 1
            foreach name $nameList {
                set histoFileID [lindex [split [file tail $name] "histo.tmp"] end]
                if { $histoFileID > $previousID } {
                   set previousID $histoFileID
                }
            }
           
            if { $previousID != 1 } {
               incr previousID
            }
            set g_histoFileID $previousID
            set fileName ""
            set fileName [file join $g_backupDir histo.tmp$g_histoFileID]
            set presetParamList [list weight rows]
            set g_histoParam [eval constructCmd "list" $fileName [lrange $args 2 end]]

            set idx [lsearch -exact $g_listObjs $currObj]
            set file [lindex $g_listObjs $idx]
            set openedFileName [$file getOrigName]

            if { [catch {set fitsfile [fits open "$openedFileName" 1]} err ] } {
               error "Cannot open file"
            }

#            $fitsfile move [expr $ext + 1]
            $fitsfile move $currHDU

            if ![file exists $fileName] {
               catch { eval $fitsfile histogram $g_histoParam} err
            }

            set foundFlag false
            catch { set fftmp [openFitsFile $fileName] } err

            $fftmp plotHisto
         }
         default {
            error "Unrecognized display object: [lindex $args 0]"
         }
      }
   }

   proc constructCmd { type fileName args } {
        variable presetParamList

        set tokens [split $args " "]

        set optionList {}
        set paramList {}
        set valueList {}
        set resultString ""
        set valueString ""

        set newParam false

        foreach cmd $tokens {
            if { $cmd == "" } continue

            set token [split $cmd "="]
            if { [llength $token] == 2 } {
               set newParam true
               if { $type == "list" } {
                  if { [llength $valueList] > 0 } {
                     lappend paramList $valueList
                     set valueList {}
                  } 
               } else {
                  if { $valueString != "" } {
                     set resultString [format "%s %s\}" $resultString $valueString]
                     set valueString ""
                  }
               }

               set param [lindex $token 0]
               set value [lindex $token 1]

               if { [llength $presetParamList] > 0 } {
                  set idx [lsearch $presetParamList $param]
               } else {
                  set idx -1
               }

               if { $idx >= 0 } {
                  lappend optionList [format "-%s" $param]
                  lappend optionList $value
               } else {
                  if { $type == "list" } {
                     lappend valueList [lindex $token 1]
                  } else {
                     set valueString [format "%s \{%s" [string toupper $param] $value]
                  }
               }
            } else {
               if { $type == "list" } {
                  set newParam false
                  lappend valueList $cmd
               } else {
                  set valueString [format "%s %s" $valueString $cmd]
               }
            }

        }

        if { $type == "list" } {
           set returnList $optionList
           lappend returnList $fileName
           foreach dataList $paramList {
               lappend returnList $dataList
           }
           # last parameter list is not on the paramList when we reach here
           lappend returnList $valueList
        } else {
           set returnList [format "%s %s\}" $resultString $valueString]
        }

        return $returnList
   }

   proc pow { args } {
      if { [llength $args]==0 } {
         error "Usage: pow powCmd ?args ...?"
      }
      return [namespace eval ::powCmds $args]
   }

   proc quit {} {
      exitCmd
   }


   proc save { args } {
      variable currObj
      
      checkForCurrentFile
      set argc [llength $args]
      if { $argc == 0 } {
         $currObj save
      } else {
         $currObj saveAs [lindex $args 0]
      }
   }

   proc opentool { args } {
        global g_backupDir
        variable presetParamList

        set token [split $args " "]
        set parameter [lindex $token 0]
        if { [llength $token] < 1 } {
           error "Missing fields. Input is: $args"
        }
        switch $parameter {
               "skyview" { 
                 global g_skyvflag
                 if ![info exists g_skyvflag ] {
                      set g_skyvflag 1
                      FVSkyview skyv
                 }
                 skyv getskyvfile
               }
               "catalog" { 
                   global g_skyvflag
                   if ![info exists g_skyvflag ] {
                         set g_skyvflag 1
                         FVSkyview skyv
                   }
                   skyv getskyvcat
               }
               "vizier" { 
                   global g_vizier
                   if ![info exists g_vizier ] {
                        set g_vizier 1
                        FVVizier viz
                   }
                   viz selectVizier
               }
               "ftools" { 
                   global g_ftoolflag
                   if ![info exists g_ftoolflag ] {
                         set g_ftoolflag 1
                         FtoolInstance fvFtool
                   }
                   fvFtool MainMenu
               }
               default {
                   error "invalid value. type 'xpaget fv help opentool' for help"
                   return
               }
        }
   }

   proc test { args } {
         set feedback [promptMsg "File exists\n overwrite?" return yes no]
         puts $feedback

   }

   proc preference { args } {
        variable _tabLabels [list App Keywords Tables Graphs Colors]

        # preference parameters: <parameter> <value>
        if { $args == "" } {
           error "Missing arguments."
        }
        set token [split $args " "]
        set parameter [lindex $token 0]
        if { [llength $token] < 2 } {
           switch $parameter {
                  "open" {
                     fvPref edit
                     return
                  }
                  "close" {
                     if [winfo exists .fvpref] {
                        .fvpref deactivate
                     }
                     return
                  }
                  "minimize" {
                     wm withdraw .fvpref 
                  }
                  "deiconify" {
                     wm deiconify .fvpref
                  }
                  "help" {
                     hhelp fv_scripting 
                     return
                  }
                  default {
                     error "Missing fields. Input is: $args"
                  }
           }
        }

        set value     [lindex $token 1]
        set allowValueList {}

        switch $parameter {
               "fileSelection" {
                  if { [string tolower $value] == "open" } {
                     .fD activate Open
                  } else {
                     .fD deactivate
                  }
               }
               "page" {
                  fvPref raisePage $value
               }
               "displayer" {
                  set allowValueList [list DS9 POW SAOTNG]
                  set value [string toupper $value]
               }
               "graphsize" {
                  set allowValueList [list 300x300 500x300 300x500 500x500 700x500 500x700 700x700]
               }
               "writehiskey" {
                  set allowValueList [list 0 1]
               }
               "dispfitsonly"  {
                  set allowValueList [list 0 1]
               }
               "wcsinfo" {
                  set allowValueList [list 0 1]
               }
               "protectedkeys" {
                  set allowValueList [list 0 1]
               }
               "autoreformatkeys" {
                  set allowValueList [list 0 1]
               }
               "usedesktopmanager" {
                  set allowValueList [list 0 1]
               }
               "autoplotprimary" {
                  set allowValueList [list 0 1]
               }
               "leftjustifystring" {
                  set allowValueList [list 0 1]
               } 
               "winmanagement" {
                  set allowValueList [list Keep Hide Close]
                  set value [format "%s%s" [string toupper [string range $value 0 0]] \
                                           [string tolower [string range $value 1 end]]]
               }
               "autoupdatechecksum" {
                  set allowValueList [list 0 1 2]
               }
        }

        if { [llength $allowValueList] > 0 && [lsearch $allowValueList $value] < 0 } {
           puts "preference usage: xpaset -p fv preference <parameter> <value>"
           puts "         parameter              possible value"
           puts "         ===================    ======================================================="
           puts "    basic operation"
           puts "         open                   N/A"
           puts "         close                  N/A"
           puts "         minimize               N/A"
           puts "         deiconify              N/A"
           puts "         page                   App, Keywords, Tables, Graphs, and Colors"
           puts "         fileSelection          Open, Close"
           puts "\n    color choice"
           puts "         activebackground       red, green, blue, etc"
           puts "         activeforeground       red, green, blue, etc"
           puts "         background             red, green, blue, etc"
           puts "         checkbuttoncolor       red, green, blue, etc"
           puts "         foreground             red, green, blue, etc"
           puts "\n    graph display"
           puts "         displayer              ds9 pow saotng"
           puts "         graphsize              300x300 500x300 300x500 500x500 700x500 500x700 700x700"
           puts "\n    desktop manager help"
           puts "         help                   N/A"
           puts "\n    options"
           puts "         autoplotprimary      0 - deselect, 1 - select"
           puts "         autoreformatkeys     0 - deselect, 1 - select"
           puts "         autoupdatechecksum     0 - never, 1 - if exist, 2 - always"
           puts "         dispfitsonly           0 - deselect, 1 - select"
           puts "         leftjustifystring    0 - right, 1 - left"
           puts "         protectedkeys        0 - deselect, 1 - select"
           puts "         usedesktopmanager    0 - deselect, 1 - select"
           puts "         wcsinfo              0 - deselect, 1 - select"
           puts "         writehiskey          0 - deselect, 1 - select"
           puts "         winmanagement          Keep, Hide, Close"
           return
        }

        switch $parameter {
               "displayer" {
                  set fvPref::imgDisplayer $value
               }
               "graphsize" {
                  set fvPref::graphDispSize $value
               }
               "background" {
                  set fvPref::globalBgColor $value
               }
               "foreground" {
                  set fvPref::globalFgColor $value
               }
               "activebackground" {
                  set fvPref::activeBgColor $value
               }
               "activeforeground" {
                  set fvPref::activeFgColor $value
               }
               "checkbuttoncolor" {
                  set fvPref::checkBBgColor $value
               }
               "writehiskey" {
                  set fvPref::ifWriteHisKey $value
               } 
               "dispfitsonly" {
                  set fvPref::ifDispFitsOnly $value
                  .fD changeListFITSoption
               }
               "wcsinfo" {
                  set fvPref::ifWCSInfo $value
               }
               "protectedkeys" {
                  set fvPref::ifProtectedKeys $value
               }
               "autoreformatkeys" {
                  set fvPref::ifAutoReformatKeys $value
               }
               "winmanagement" {
                  set fvPref::winManagement $value
               }
               "autoupdatechecksum" {
                  set fvPref::ifAutoUpdateChecksum $value
               }
               "leftjustifystring" {
                  set fvPref::ifLeftJustifyString $value
               }
               "usedesktopmanager" {
                  if { $::tcl_platform(platform) != "macintosh" } {
                     set fvPref::ifUseManager $value
                  }
                  .fvwinkeeper updateVisibility
               }
               "autoplotprimary" {
                  set fvPref::ifAutoPlotPrimary $value
               }
        }

        fvPref save
   }

   proc select { args } {
      global g_listObjs
      variable currObj
      variable currHDU

      if { [llength $args]==0 } {
         checkForCurrentFile
         if { $currObj == "" } {
            return "no opened file."
         }
         set idx [lsearch -exact $g_listObjs $currObj]
         set file [lindex $g_listObjs $idx]
         return "[$file getOrigName]\n"
      } else {
         if { [llength $args]>1 } {
            set currHDU [expr [lindex $args 1]+1]
         } else {
            set currHDU 1
         }
         set theIdx [lindex $args 0]
         if { [regexp {^[0-9]*$} $theIdx] || $theIdx=="first" || $theIdx=="end" } {
            set currObj [lindex $g_listObjs $theIdx]
         } elseif { $currObj != ""  && $theIdx != "current" && $theIdx!="."} {
            set findFlag false
            foreach file $g_listObjs {
               set f [$file getOrigName]
               if { $theIdx==$f || $theIdx==[urlTail $f] } {
                  set findFlag true
                  set currObj $file
                  return
               }
            }
            error "No file found matching $theIdx"
         } else {
            error "$args is not opened."
         }
      }

   }


   proc plot { args } {
      variable currObj
      variable currHDU

      set option 0
      checkForCurrentFile
      set ext [lindex $args 0]
      if { $ext!="current" && $ext!="." } {
         set currHDU [expr $ext+1]
      }
      if { $option !=0 } {
      set fT [$currObj openTable $currHDU - 0]
      set range [lindex $args 2]
      }

   }
   proc insert { args } {
      variable currObj
      variable currHDU

      set option 0
      foreach arg [lrange $args 0 0] {
         switch -glob -- $arg {
            "row*" {
               set option 1
            }
            "col*" {
               set option 2
            }
         }
      }

      checkForCurrentFile
      set ext [lindex $args 1]
      if { $ext!="current" && $ext!="." } {
         set currHDU [expr $ext+1]
      }
      if { $option !=0 } {
      set fT [$currObj openTable $currHDU - 0]
      set range [lindex $args 2]
      }



      if { $option == 1 } {
          $fT _addRows [lindex $args 2] [lindex $args 3]
      } elseif { $option == 2 } {
          if {[lindex $args 4] =="" } {
          $fT _addCols - [lindex $args 2] [lindex $args 3]   
          } else {
          $fT _addCols [lindex $args 4] [lindex $args 2] [lindex $args 3]  
          } 
      } else {
#          puts "ext $ext, $currObj"
#          $currObj deleteExt $ext
#          puts "currObj" 
      }

  }

   proc delete { args } {
      variable currObj
      variable currHDU
      
      set argc [llength $args]
      if { $argc < 3 } {
         error "type 'xpaget fv help delete' for help"
      }
           

      set option 0
      foreach arg [lrange $args 0 0] {
         switch -glob -- $arg {
            "row*" {
               set option 1
            }
            "col*" {
               set option 2
            }
         }
      }

      checkForCurrentFile
      set ext [lindex $args 1]
      set numexts [$currObj getNumHdus]

      if { $ext < 0 || $ext >= $numexts } {
          error "invalid extension number"
      }

      if { $ext!="current" && $ext!="." } {
         set currHDU [expr $ext+1]
      }

      if { $option !=0 } {
      set fT [$currObj openTable $currHDU - 0]
      set range [lindex $args 2]
      }


      if { $option == 1 } {
          $fT _tryDelRows $range
      } elseif { $option == 2 } {
          $fT _tryDelCols [lrange $args 2 end]
      } else {
          $currObj deleteExt $ext
      }

       
       

  }


   proc sort { args } {
      variable currObj
      variable currHDU

      checkForCurrentFile
      set ext [lindex $args 0]
      if { $ext!="current" && $ext!="." } {
         set currHDU [expr $ext+1]
      }
      set fT [$currObj openTable $currHDU - 0]

      set keys    {}
      set dirs    {}
      set unique  0
      set currDir 1
      foreach arg [lrange $args 1 end] {
         switch -glob -- $arg {
            "-asc*" {
               set currDir 1
            }
            "-des*" {
               set currDir 0
            }
            "-un*" {
               set unique 1
            }
            "-sel*" {
               set select 1
            }
            default {
               lappend keys $arg
               lappend dirs $currDir
            }
         }
      }

      $fT doSort $keys $dirs $unique 
   }

   proc version {} {
      global g_fvVersion

      return "FV version is: $g_fvVersion\n"
   }


  proc minimize {args} {
    variable winList

    set winlist [winfo children .]



    foreach win $winlist {

       if {  1 == [winfo ismapped $win] } {
           lappend winList $win
           wm withdraw $win
       }
    }



 }

 proc deiconify {args} {
     variable winList

     foreach win $winList {
          wm deiconify $win
     }
     set winList {}
 }

 

   ########################
   # Utility routines, not public command
   ########################

   proc checkForCurrentFile {} {
      global g_listObjs
      variable currObj
      variable currHDU

      if { $currObj=="" || [lsearch -exact $g_listObjs $currObj]==-1 } {
         # currObj is invalid.  Can we select a new default?
         if { [llength $g_listObjs]==0 } {
            error "No files available"
         }
         set currObj [lindex $g_listObjs end]
         set currHDU 1
      }

   }

}


####################   XPA  Entry Points   ####################

namespace eval fvXPA {
   variable xpaPt

   proc init {} {
      global env
      variable xpaPt

      # Pan Chai: package require tclxpa statement is required for one-click version
      #           Do not delete.
      if { [catch {package require tclxpa} err ] } {
         if { [catch {load [file join $env(LHEASOFT)/lib libtclxpa[info sharedlibextension]]} err1 ] && \
              [catch {load "" xpa} err2 ] } return
      }

      set xpaPt [xpacmdnew "" fv]

      foreach {cmd snd rcv hlp} [list                                    \
            test       1 1 "Test command"                                \
            close      0 1 "Close a file, or extension windows"          \
            create     0 1 "Create a FITS file or an extension"          \
            display    0 1 "Display contents of an extension"            \
            export     0 1 "export FITS file HDU or as a text file"      \
            exporttable 0 1 "export FITS table as HDU or as a text file" \
            help       1 0 "Display XPA entry point for fv"              \
            open       1 1 "Open a FITS file"                            \
            opentool   0 1 "Open other tools, SkyView Catalogs, VizieR"  \
            pow        1 1 "Execute a pow command"                       \
            plot       1 1 "Execute a plot command"                       \
            preference 0 1 "Change the preference of fv"                 \
            quit       0 1 "Quit fv"                                     \
            save       0 1 "Save file"                                   \
            select     1 1 "Select an opened file for manipulation"      \
            sort       0 1 "Sort a table"                                \
            delete     0 1 "Delete a table row or columns"               \
            insert     0 1 "Insert a table row or columns"               \
            tcl        0 1 "Execute tcl code"                            \
            version    1 0 "Return fv version number"                    \
            minimize   1 1 "minimize the windows"                    \
            deiconify  1 1 "normalized the windows"                    \
            ] {
        catch { register $cmd $snd $rcv $hlp } err
      }

   }

   proc register { cmd snd rcv hlp } {
      variable xpaPt
      
      if { $snd } {
         set sndParam [list fvXPA::send $cmd "fillbuf=false"]
      } else {
         set sndParam [list "" "" ""]
      }
      if { $rcv } {
         set rcvParam [list fvXPA::recv $cmd "fillbuf=false"]
      } else {
         set rcvParam [list "" "" ""]
      }
      catch {eval xpacmdadd $xpaPt $cmd {$hlp} $sndParam $rcvParam} err
   }


   proc send { xpa client_data paramlist } {
      switch -exact $client_data {
         default {
            if { [namespace eval ::fvCmds info procs $client_data]!="" } {
               xpasetbuf $xpa [eval ::fvCmds::$client_data $paramlist] 
            } else {
               xpaerror $xpa "$client_data is an invalid command"
            }
         }
      }
      return 
   }

   proc recv { xpa client_data paramlist buf len } {
      switch -exact $client_data {
         pow {
            if { [llength $paramlist]==0 } {
               error "pow powCmd ?args ...?"
            }
            ::powXPA::recv $xpa [lindex $paramlist 0] \
                  [lrange $paramlist 1 end] $buf $len
         }
         tcl {
            set dchan [xparec $xpa datachan]
            set scrpt [read $dchan]
            close $dchan
            xpasetbuf $xpa [namespace eval ::fvCmds $scrpt]
         }
         default {
            if { [namespace eval ::fvCmds info procs $client_data]!="" } {
               if {[catch {eval ::fvCmds::$client_data $paramlist} err]} {
                  xpaerror $xpa $err
               }
               
            } else {
               xpaerror "$client_data is an invalid command"
            }
         }
      }
      return 
   }

}
