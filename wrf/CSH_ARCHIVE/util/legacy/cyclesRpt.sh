#!/bin sh

# figure out where we are and set range specific variables 
#
this_system=`hostname -s`
case $this_system in
  4dwx-dpg-c1) RANGE_DIR="/data/cycles"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
   atc-c1) RANGE_DIR="/data/cycles/GMATC/ATC"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
  crtc-c2) RANGE_DIR="/data/cycles/GMCRTC/CRTC"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
  wsmr-c1) RANGE_DIR="/data/cycles/GMWSMR/WSMR"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
   ypg-c1) RANGE_DIR="/data/cycles/GMYPG/YPG"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
  dtra-c1) RANGE_DIR="/data/cycles/GREECE/GRM"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
    gs-c1) RANGE_DIR="/data/cycles/GMDPG/DPG"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
   dev-c1) RANGE_DIR="/data/cycles/VPGWS/GRM"; RINPUTS="eta wmo"; YINPUTS="acars raws";;
        *) echo "Unrecognized system $this_system"; exit 1;;
esac

# set up other basic variables
#
ymd=`date +%Y%m%d`
hhmm=`date +%H:%M`
year=`date +%Y`
month=`date +%m`
day=`date +%d`
now="${ymd}-${hhmm}"

input_status="GREEN"
system_status="GREEN"
disk_space="GREEN"
too_many_fdda_procs=300

# for checking inputs
#
base_input_dir=/data/input
input_dirs="acars eta raws wmo"

fddadir=$RANGE_DIR
cd $fddadir
ls -lt 200*/*MMOUTPUT_DOMAIN1.*_F | \
awk '{ print $9 }' | sed "s/\// /" | \
awk '{ print substr($1,1,8) }' | sort | uniq -c

for dir in $input_dirs
do
    chk_red=`echo $dir $RINPUTS | \
       awk '{ f=2; while (f <= NF) { if ($1 == $f) { print 1; exit }; f++ }
              print 0 }'`
    chk_yellow=`echo $dir $YINPUTS | \
       awk '{ f=2; while (f <= NF) { if ($1 == $f) { print 1; exit }; f++ }
              print 0 }'`
    if [ ! -d ${base_input_dir}/$dir ]; then
      if [ "$chk_red" == "1" -o "$chk_yellow" == "1" ]; then
         if [ "$input_status" != "RED" ]; then
            if [ "$chk_red" == "1" ]; then
                 input_status="RED"
            else
                 input_status="YELLOW"
            fi
         fi 
      fi
    else
      last_date=`ls -lt ${base_input_dir}/$dir | head -2 | grep -v total | \
      awk -v year=$year '
        BEGIN { mon["Jan"]="01"; mon["Feb"]="02"; mon["Mar"]="03"
                mon["Apr"]="04"; mon["May"]="05"; mon["Jun"]="06"
                mon["Jul"]="07"; mon["Aug"]="08"; mon["Sep"]="09"; dd=0 
                mon["Oct"]="10"; mon["Nov"]="11"; mon["Dec"]="12"; mm=0
                
        }
	{ if ($6 in mon) { mm=mon[$6] }
          if ($8 !~ /:/) { year=$8 }
	  dd=$7
        }
        END { printf("%04d%02d%02d", year, mm, dd) }'`
 
      if [ "$last_date" != "$ymd" ]; then
         if [ $chk_red -eq 1 -o $chk_yellow -eq 1 ]; then
            if [ "$input_status" != "RED" ]; then
               if [ $chk_red -eq 1 ]; then
                    input_status="RED"
               else
                    input_status="YELLOW"
               fi
            fi
         fi 
      fi
    fi
done

fddasys_procs=`ps -fu fddasys | wc -l | awk '{ print $1 }'`
if [ $fddasys_procs -gt $too_many_fdda_procs ]; then
   system_status="YELLOW"
fi
ds=`df | egrep "data|raid" | awk 'NR == 1 { print substr($5,1,length($5)-1) }'`
if [ $ds -gt 90 ]; then
   disk_space="YELLOW"
fi
if [ $ds -gt 95 ]; then
   disk_space="RED"
fi 

echo "$now Inputs: $input_status  System: $system_status  DiskSpace: $disk_space"
