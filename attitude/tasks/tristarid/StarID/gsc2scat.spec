# partition summary
type => StarID::SearchCat
fields => ID,RA,DEC,MAGF,MAGJ,MAGV,MAGN,CLASS,DIST
data => GSC2
catalog/type => Indexed
catalog/n => 4

subtype => gsc2
envvar => GSC_PATH
location => http://archive.eso.org/skycat/servers/gsc-server

limit => 10000
