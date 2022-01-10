#!/bin/csh
#
# Create a reservation
# -c     - create
# -t 6   - for 6 tasks (nodes)
# -s 3:00:00_12/12/06   - starttime = Dec 12, 2006 at 3:00 AM
# -e 3:00:00_12/12/07   - endtime   = Dec 12, 2007 at 3:00 AM
# -a USER==fdda-atec    - ACL
# -n GMOD-Hawaii
/opt/moab/bin/mrsvctl -c -t 6 -a USER==fdda-atec+ -s 3:00:00_12/12/06 -e 3:00:00_12/12/07 -n GMOD-Hawaii

# Modify a reservation:

# Add carson to the ACL of a reservation
#mrsvctl -m -a USER==carson+ RES_ID

#Add eschuler to the ACL of a reservation
#mrsvctl -m -a USER==eschuler+ RES_ID

#Remove eschuler from the ACL of the reservation
#mrsvctl -m -a USER-=eschuler RES_ID

#Release a reservation
#mrsvctl -r RES_ID
