SHELL = "/bin/csh"
MAILTO=""

#  ===== Output data sent to GAMEP ==========

15 * * * * /usr/bin/touch /home/x_fisherh/dm.cron
0,30 * * * * /home/x_fisherh/distrib/push2gamep.sh >& /home/x_fisherh/datlog/rsync2gamep.log
