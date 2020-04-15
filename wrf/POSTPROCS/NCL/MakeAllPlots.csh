#

##goto VEG
foreach Range ( "ATC" "DPG" "WSMR" "YPG" "CRTC")

  echo "running plots for Range: $Range"
  setenv RANGE $Range
  @ domain = 1
  @ last_domain = 4
  if ($Range == "YPG") @ last_domain = 3
  while ($domain <= $last_domain)

    ncl wrf_static_topo.ncl d=$domain
    set epsfile = WRF_topo_${Range}_domain${domain}.eps
    set giffile = WRF_topo_${Range}_domain${domain}.gif
    convert $epsfile /var/www/WRF_static/$RANGE/$giffile

    ncl wrf_static_landmask.ncl d=$domain
    set epsfile = WRF_landmask_${Range}_domain${domain}.eps
    set giffile = WRF_landmask_${Range}_domain${domain}.gif
    convert $epsfile /var/www/WRF_static/$RANGE/$giffile

    ncl wrf_static_config.ncl d=$domain
    set epsfile = WRF_config_${Range}_domain${domain}.eps
    set giffile = WRF_config_${Range}_domain${domain}.gif
    convert $epsfile /var/www/WRF_static/$RANGE/$giffile
    
    @ domain ++

  end
end 

ncl landuse_labelbar.ncl
convert landuse_label.eps landuse_label.gif

foreach Range ("DPG" "WSMR" "YPG" "CRTC")
  echo "running plots for Range: $Range"
  setenv RANGE $Range
  @ domain = 1
  @ last_domain = 4
  if ($Range == "YPG") @ last_domain = 3
  while ($domain <= $last_domain)

    ncl wrf_static_landuse.ncl d=$domain
    set epsfile = WRF_landuse_${Range}_domain${domain}.eps
    set giffile = WRF_landuse_${Range}_domain${domain}.gif

    convert -rotate -90 $epsfile aux.gif
    montage -geometry +0+0 aux.gif landuse_label.gif /var/www/WRF_static/$RANGE/$giffile
    rm -f $epsfile aux.gif
    @ domain ++
  end
end


