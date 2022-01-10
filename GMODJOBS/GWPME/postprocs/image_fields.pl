
# Define field names for this job configuration

# Note the change to square-brackets...

# This array is indexed by $domain-1 !!!
# be sure to include a setting for all of your domains!


@fields_d1 = [ 'T2m', 'RHsfc', 'sfcstrm', 'Pre1hrWnd', 'radar', 'cape', 'cin', 'MSLPWnd', 'T850Wnd', 'T700Wnd', 'T500Wnd', 'RH925Wnd', 'RH850Wnd', 'RH700Wnd', 'RH500Wnd', 'Vor700Z', 'Vor500Z', 'Wnd300Z', 'Wnd200Z', 'IRsat', 'slhflux', 'sshflux', 'soilt1', 'landuse', 'LandUseTbl', 'WRFgrids', 'xsloc', 'xsthw', 'xsrhw', 'xswWnd' ];

@fields_d2 = [ 'T2m', 'RHsfc', 'sfcstrm', 'Pre1hrWnd', 'radar', 'cape', 'cin', 'MSLPWnd', 'T850Wnd', 'T700Wnd', 'T500Wnd', 'RH925Wnd', 'RH850Wnd', 'RH700Wnd', 'RH500Wnd', 'Vor700Z', 'Vor500Z', 'Wnd300Z', 'Wnd200Z', 'IRsat', 'slhflux', 'sshflux', 'soilt1', 'landuse', 'LandUseTbl', 'WRFgrids', 'xsloc', 'xsthw', 'xsrhw', 'xswWnd', 'sndg01', 'sndg02', 'sndg03', 'sndg04', 'sndg05', 'sndg06', 'sndg07', 'sndg08', 'sndg09', 'sndg10', 'sndg11', 'sndg12', 'sndg13', 'sndg14'  ];

@fields_d3 = [ 'T2m', 'RHsfc', 'sfcstrm', 'Pre1hrWnd', 'radar', 'cape', 'cin', 'MSLPWnd', 'T850Wnd', 'T700Wnd', 'T500Wnd', 'RH925Wnd', 'RH850Wnd', 'RH700Wnd', 'RH500Wnd', 'Vor700Z', 'Vor500Z', 'Wnd300Z', 'Wnd200Z', 'IRsat', 'slhflux', 'sshflux', 'soilt1', 'landuse', 'LandUseTbl', 'WRFgrids', 'xsloc', 'xsthw', 'xsrhw', 'xswWnd', 'sndg01', 'sndg02', 'sndg03', 'sndg04', 'sndg05', 'sndg06', 'sndg07', 'sndg08', 'sndg09', 'sndg10', 'sndg11', 'sndg12', 'sndg13', 'sndg14', 'sndg15'  ];

@img_fields[0] = @fields_d1;
@img_fields[1] = @fields_d2;
@img_fields[2] = @fields_d3;

