#!/bin/csh

rm wrfout.file
ln -s $1 wrfout.file
set valid=`ls $1 | cut -c15-33`
set domain=`ls $1 | cut -c11-13`
echo $valid
echo $domain
rm input.file
cat input.1 | sed s/time/$valid/g > input.file
set output_valid=`ls $1 | cut -c15-27 | sed s/_//g | sed s/-//g`
rm WRFPRS.GrbF*
./unipost.exe < input.file
mv WRFPRS.GrbF* "wrf_$domain"_"$output_valid"00".grb"
mv WRFSRS.GrbF* "mob_$domain"_"$output_valid"00".grb"
