#!/usr/bin/perl -w

${indir} = "/d1/pmefdda/data/cycles/GWRFDUST/GFS_WCTRL/";

${cymd} = `date -d \"0 day ago\" '+%C%y%m%d'`;
chomp(${cymd});
#${cycle} = "${cymd}"."18";
${cycle} = "${cymd}"."06";

${dir} = ${indir}.${cycle};
${outdir} = ${indir}."plots/"."${cycle}";
${workdir} = ${indir}."plots/";

if (! -d "${outdir}") {system ("mkdir -p ${outdir}");}

chdir ${workdir};

#${dom} = 1;

system ("rm -f filelist");
system ("ls -1 ${dir}/wrfout_d01*_WCTRL_P+FCST > filelist");

${fsname} = "filelist";
@files = `cat ${fsname}`;
$ii = 0;
foreach ${fs} (@files) {
chomp($fs);
if ($ii<10) {$ii = "0".$ii};
$ncl = "ncl 'file_in=\"${fs}\"' 'dom=1' 'indir=\"${indir}\"' 'cycle=\"${cycle}\"' 'nfcst=\"${ii}\"' plot_DUST_surface.ncl >& DUSTsurface_${cycle}.log";
print "$ncl\n";
system ("$ncl");
$ii = $ii + 1;
}
system ("mv d1_surfaceDUST*.png ${outdir}");
1;
