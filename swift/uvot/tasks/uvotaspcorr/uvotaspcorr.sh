# $Source: /headas/headas/swift/uvot/tasks/uvotaspcorr/uvotaspcorr.sh,v $
# $Revision: 1.1 $
# $Date: 2006/01/19 18:56:05 $
#
#	find aspect corrections for many observations
#
# $Log: uvotaspcorr.sh,v $
# Revision 1.1  2006/01/19 18:56:05  rwiegand
# Task to simplify running uvotskycorr on observations.
#
# Revision 1.2  2005/11/14 15:01:19  wiegand
# Updated tool name.
#
# Revision 1.1  2005/08/29 17:37:31  wiegand
# Initial revision

for d in 0*; do
	echo "directory $d"
	uvotaspcorr input=$d output=out filter=ALL apply=no chatter=5
done
