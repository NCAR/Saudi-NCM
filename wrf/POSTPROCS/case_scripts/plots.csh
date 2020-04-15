#!/bin/csh
#
foreach f (`ls *DOMAIN1.GRM`)
echo $f
set date = `echo ${f} | cut -c 1-12`
echo $date
/raid/cycles/GMHURR/GRM/case/do_output_archive.pl /raid/cycles/GMHURR/GRM/case GMHURR $date /raid/cycles/GMHURR/GRM/case/0 final 2 GRM 
end
