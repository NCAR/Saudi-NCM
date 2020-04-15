#!/usr/bin/csh -f
set y_end = `echo $1 |cut -c1-4`
set m_end = `echo $1 |cut -c5-6`
set d_end = `echo $1 |cut -c7-8`
set h_end = `echo $1 |cut -c9-10`

set file = "$2"

ed $file << EOF > /dev/null
g/GEYY/s//$y_end/g
g/GEMM/s//$m_end/g
g/GEDD/s//$d_end/g
g/GEHH/s//$h_end/g
w
q
EOF
