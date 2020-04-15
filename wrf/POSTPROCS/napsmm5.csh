#!/bin/csh
#
#
setenv NCARG_ROOT /opt/ncarg
setenv NCARG_LIB /opt/ncarg/lib

   set SndData = $1
if(-e input.dat) rm input.dat

cat > input.dat << EOF
1                           | ICHOICE (default=1, NAPS model run)
0                           | IOPT (default=0, computes dB contours)
.True.                      | PLOTIT (set to true to invoke plotting)
.True.                      | white_backgrnd (true if white background on plots)
4                           | DBPLOTOPT (1-5)
.True.                      | DOELV (true to use elevation data)
.True.                      | DOATT (true to use land attribute data)
5.                          | input_delpsi(azimuth increment(deg),default=5)
'./napstest/'               | METDIR  directory containing met data
'./napstest/'               | DATADIR data directrory
'./napstest/'               | OUTDIR  output directory
12                          | ISITE: blast site
1.                          | ZBAG: blast height above surface in meters
20.                         | WB: blast weight in pounds
0                           | IGUN: gun type (default=0, uniform blast)
'$SndData'                  | met_dsn: input meteorology file
EOF

cp -p /data/fddahome/cycle_code/POSTPROCS/NAPS/* napstest

/data/fddahome/cycle_code/EXECUTABLE_ARCHIVE/naps

echo 'Noise is plotted'
