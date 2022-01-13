#!/usr/bin/perl -w

${indir} = "/lustre/project/k1206/Dust_realtime/WRF12Z/";

${cymd} = `date -d \"0 day ago\" '+%C%y%m%d'`;
chomp(${cymd});
#${cycle} = "${cymd}"."18";
#${cycle} = "${cymd}"."06";
${cycle} = "${cymd}"."12";

#${dir} = ${indir}.${cycle};
${dir} = ${indir};
#${outdir} = ${indir}."plots/"."${cycle}";
${outdir} = "/lustre/project/k1206/x_fisherh/distrib/GWPME/DUST_plots/"."${cycle}";
print "$outdir \n";
#${workdir} = ${indir}."plots/";
${workdir} = "/lustre/project/k1206/x_fisherh/distrib/GWPME/DUST_plots/";

if (! -d "${outdir}") {system ("mkdir -p ${outdir}");}

chdir ${workdir};
system ("pwd");

#${dom} = 1;

system ("rm -f filelist");
#system ("ls -1 ${dir}/wrfout_d01*_WCTRL_P+FCST > filelist");
system ("ls -1 ${dir}/wrfout_d02* > filelist");

${fsname} = "filelist";
@files = `cat ${fsname}`;
$ii = 0;
foreach ${fs} (@files) {
chomp($fs);
if ($ii<10) {$ii = "0".$ii};
$ncl = "ncl 'file_in=\"${fs}\"' 'dom=2' 'indir=\"${indir}\"' 'cycle=\"${cycle}\"' 'nfcst=\"${ii}\"' plot_DUST_all.ncl >& DUSTlog_${cycle}.log";
print "$ncl\n";
system ("$ncl");
$ii = $ii + 1;
}
system ("mv d2_DUSTall*.png ${outdir}");
1;
