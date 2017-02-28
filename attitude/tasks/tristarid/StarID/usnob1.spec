# partition summary
type => StarID::SearchCat
fields => ID,RA,DEC,MAGB1,MAGR1,MAGB2,MAGR2,MAGN,PM,NI,SG,DIST
data => USNOB1
catalog/type => Indexed
catalog/n => 4

subtype => ub1
sort => m4
envvar => UB1_PATH
location => /ssdc/usnob1
# a USNO B1 server
#	location => http://tdc-www.harvard.edu/cgi-bin/scat
# another USNO server
#	location => http://archive.eso.org/skycat/servers/usnoa-server
limit => 5000
