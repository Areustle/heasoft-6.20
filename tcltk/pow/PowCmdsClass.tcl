proc gPowCmdsClass { args } {
   return [uplevel #0 PowCmdsClass #auto $args]
}

itcl::class PowCmdsClass {

   constructor {} {}
   destructor {}

   public {
        method helpPage   { args }
        method binFactorTool { args }
        method regionTool { args }
        method getRegion  { args }
        method getXRange  { args }
        method regions    { args }
        method regionName { args }
        method setRegionFormat { args }
        method xrangeTool { args }
        method xranges    { args }
        method xrangeName { args }
        method add       { objType objName }
        method array     { dchan dName {bitpix "LIST"} {byteOrder ""} }
        method axes      { xscale yscale }
        method bounds    { args }
        method calculate { newData args }
        method colormap  { args }
        method close     { args }
        method create    { objType objNam args }
        method contour   { args }
        method cursor    { }
        method curve     { args }
        method delete    { args }
        method draw      { args }
        method graph     { args }
        method init      { ncolors colorMode }
        method position  { args }
        method refresh   { args }
        method remote    { args }
        method remove    { args }
        method scope     { {dx ""} {dy ""} }
        method select    { obj name }
        method size      { args }
        method version   { }
        method wcs       { obj wcs }
        method wcsHeader    { gn strlen {str} flag }
        method wcsHeaderCnt { gn cnt }
        method wcsLabel  { gn label name value }
        method wcsSetList   { gn {list} }
   }
}

itcl::body PowCmdsClass::destructor {} {
}

itcl::body PowCmdsClass::helpPage { args } {
     return [eval ::powCmds::helpPage $args]
}

itcl::body PowCmdsClass::setRegionFormat { args } {
     return [eval ::powCmds::setRegionFormat $args]
}

itcl::body PowCmdsClass::regionName { args } {
     return [eval ::powCmds::regionName $args]
}

itcl::body PowCmdsClass::regions { args } {
     return [eval ::powCmds::regions $args]
}

itcl::body PowCmdsClass::getRegion { args } {
     return [eval ::powCmds::getRegion $args]
}

itcl::body PowCmdsClass::getXRange { args } {
     return [eval ::powCmds::getXRange $args]
}

itcl::body PowCmdsClass::binFactorTool { args } {
     return [eval ::powCmds::binFactorTool $args]
}

itcl::body PowCmdsClass::regionTool { args } {
     return [eval ::powCmds::regionTool $args]
}

itcl::body PowCmdsClass::xrangeName { args } {
     return [eval ::powCmds::xrangeName $args]
}

itcl::body PowCmdsClass::xranges { args } {
     return [eval ::powCmds::xranges $args]
}

itcl::body PowCmdsClass::xrangeTool { args } {
     return [eval ::powCmds::xrangeTool $args]
}

itcl::body PowCmdsClass::add       { objType objName } {
     return [eval ::powCmds::add $objType $objName]
}

itcl::body PowCmdsClass::array     { dchan dName {bitpix "LIST"} {byteOrder ""} } {
     return [eval ::powCmds::array $dchan $dName $bitpix $byteOrder ]
}

itcl::body PowCmdsClass::axes      { xscale yscale } {
     return [eval ::powCmds::axes $xcale $ycale]
}

itcl::body PowCmdsClass::bounds    { args } {
     return [eval ::powCmds::bounds $args]
}

itcl::body PowCmdsClass::calculate { newData args } {
     return [eval ::powCmds::calculate $newData $args]
}

itcl::body PowCmdsClass::colormap  { args } {
     return [eval ::powCmds::colormap $args]
}

itcl::body PowCmdsClass::close     { args } {
     return [eval ::powCmds::close $args]
}

itcl::body PowCmdsClass::create    { objType objName {args} } {
     return [eval ::powCmds::create $objType $objName $args]
}

itcl::body PowCmdsClass::contour   { args } {
     return [eval ::powCmds::contour $args]
}

itcl::body PowCmdsClass::cursor    { } {
     return [eval ::powCmds::cursor]
}

itcl::body PowCmdsClass::curve     { args } {
     return [eval ::powCmds::curve $args]
}

itcl::body PowCmdsClass::delete    { args } {
     return [eval ::powCmds::delete $args]
}

itcl::body PowCmdsClass::draw      { args } {
     return [eval ::powCmds::draw $args]
}

itcl::body PowCmdsClass::graph     { args } {

     set errorFlag [ catch {
         set returnValue [eval ::powCmds::graph $args]
         eval ::powCmds::scope 0
     } err ]

     if { $errorFlag } {
        return $err
     }
     return $returnValue
}

itcl::body PowCmdsClass::init      { ncolors colorMode } {
     return [eval ::powCmds::init $ncolors $colorMode]
}

itcl::body PowCmdsClass::position  { args } {
     return [eval ::powCmds::position $args]
}

itcl::body PowCmdsClass::refresh   { args } {
     return [eval ::powCmds::refresh $args]
}

itcl::body PowCmdsClass::remote    { args } {
     return [eval ::powCmds::remote $args]
}

itcl::body PowCmdsClass::remove    { args } {
     return [eval ::powCmds::remove $args]
}

itcl::body PowCmdsClass::scope     { {dx ""} {dy ""} } {
     return [eval ::powCmds::scope $dx $dy]
}

itcl::body PowCmdsClass::select    { obj name } {
     return [eval ::powCmds::select $obj $name]
}

itcl::body PowCmdsClass::size      { args } {
     return [eval ::powCmds::size $args]
}

itcl::body PowCmdsClass::version   { } {
     return [eval ::powCmds::version]
}

itcl::body PowCmdsClass::wcs       { obj {wcs} } {
     set wcsList {}
     lappend wcsList $wcs
     return [eval ::powCmds::wcs $obj $wcsList]
}

itcl::body PowCmdsClass::wcsHeader { gn strlen {str} flag } {
     return [eval ::powCmds::wcsHeader $gn $strlen [list $str] $flag ]
}

itcl::body PowCmdsClass::wcsHeaderCnt { gn cnt } {
     return [eval ::powCmds::wcsHeaderCnt $gn $cnt]
}

itcl::body PowCmdsClass::wcsLabel { gn label name value } {
     return [eval ::powCmds::wcsLabel $gn $label $name $value]
}

itcl::body PowCmdsClass::wcsSetList { gn list } {
     return [eval ::powCmds::wcsSetList $gn $list]
}
