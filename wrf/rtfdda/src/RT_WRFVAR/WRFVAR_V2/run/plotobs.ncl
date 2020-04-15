;===========================================================================
; plotobs.ncl Scatter plot from "_plotobs" 3DVAR output files per platform
; type.
;===========================================================================
load "$NCL/nclscripts/csm/gsn_code.ncl"
load "$NCL/nclscripts/csm/gsn_csm.ncl"
load "$NCL/nclscripts/csm/contributed.ncl"
undef ("plotobs")
;=============================================================================

function sub_string(s:string,i:integer,j:integer)
begin
  s_char = stringtochar(s)

  sub_str = s_char(i:j)

  return (chartostring(sub_str))
end
;=============================================================================

function sub_string_int(s:string,i:integer,j:integer)
begin
  s_char = stringtochar(s)

  sub_str = s_char(i:j)

  return (stringtointeger(chartostring(sub_str)))
end

;=============================================================================

function sub_string_float(s:string,i:integer,j:integer)
begin
  s_char = stringtochar(s)

  sub_str = s_char(i:j)

  return (stringtofloat(chartostring(sub_str)))
end
;=============================================================================
begin

   ;======================================================================
   ; 0. DEFINE THE INPUT FILE NAME, THE PLATFORM AND THE VARIABLE TO PLOT
   ;======================================================================

   ; Input file names, a * would catenate several files

    filenames = "obs_wrfqc_plotobs"
;   filenames = "obs_after_qc.plotobs" ; WRFQC
;   filenames = "fort.99_plotobs"      ; MM5QC

   ; Output file name: output will be named filename_AMDAR.ncgm, etc.

    filename = "obs_wrfqc"
;   filename = "obs_after_qc.plotobs" ; WRFQC
;   filename = "fort.99_plotobs"      ; MM5QC

   ; Observation type to plots (Arrays of 11 characters)

;  obs_types = (/"FM-35 TEMP "/)
   obs_types = (/"FM-12 AMDAR", "FM-12 PROF ", "FM-12 SATWI", "FM-12 SYNOP", \
                 "FM-13 SHIP ", "FM-15 METAR", "FM-16 SPECI", "FM-97 TAMDA", \
                 "FM-32 PILOT", "FM-32 TEMP ", "FM-35 TEMP ", "FM-35 MWVR ", \
                 "FM-37 TEMP ", "FM-97 MADIS", "FM-97 MDCRS", "FM-97 ACARS", \
                 "FM-97 ACAMD"/)

   ; Variable to plot (Array of 2 characters)

   obs_vars  = (/"U ","V ","T ","RH"/)  
;  obs_vars  = (/"T "/)  

   ;  Watch for unknown platforms and print

   unknown_platforms = False

   ; Files first line

   first_line = "(3X,A2,6X,I4,1X,I6,4X,A4,2X,A5,3X,A11,3X,L1,4X,3(G13.6,1X),2(F8.3,2X),I7)"

   ;======================================================================
   ; 1. Worksation and color map
   ;======================================================================

;  wks_type = "X11"
;  wks_type = "ps"
   wks_type = "ncgm"

   ;=====================
   ; 2.  PREPARE FOR READ
   ;=====================

   ;---------------------
   ; 2.1 Open ascii files
   ;---------------------

   num_types = dimsizes (obs_types)
   num_vars  = dimsizes (obs_vars)

   allfiles = systemfunc("ls "+filenames)
   numfiles = dimsizes(allfiles)

   fpr=systemfunc("cat " + filenames)
   dimvar = dimsizes(fpr)

   print("")
   print("Read "+dimvar+" lines of data across "+numfiles+" file(s)...")

   ;---------------------------------
   ; 2.3 Loop over the platform types
   ;---------------------------------

   ndata = 0
   wks_opened = False

   do num_type = 0, num_types-1 

      obs_type  = obs_types(num_type)
      obs_typec = stringtochar(obs_type)

      fileout = filename

      do i = 10, 6, 1
      if (obs_typec(i) .ne. " ") then
         fileout = fileout+"_"+chartostring(obs_typec(6:i))
         break
      end if
      end do

      fileout = fileout+chartostring(obs_typec(3:4))

   ;---------------------------------
   ; 2.4 Loop over the variable names
   ;---------------------------------

    do num_var  = 0, num_vars-1 

      obs_var  = obs_vars(num_var)

      print("")
      print ("Gathering data for "+obs_var+" from platform "+obs_type)

      namevar  = new ((/dimvar/),string)
      pres_mb  = new ((/dimvar/),integer)
      obs_num  = new ((/dimvar/),integer)
      hhmmss   = new ((/dimvar/),integer)
      id       = new ((/dimvar/),string)
      platform = new ((/dimvar/),string)
      is_sfc   = new ((/dimvar/),string)
      obs      = new ((/dimvar/),float)
      mdl      = new ((/dimvar/),float)
      err      = new ((/dimvar/),float)
      xmdl     = new ((/dimvar/),float)
      ymdl     = new ((/dimvar/),float)
      qcmdl    = new ((/dimvar/),integer)

      num_obs = stringtoint(fpr(4))

      ;==============
      ; 3.  READ DATA
      ;==============

      ;---------------------------------------
      ; 3.0 Initialization and begin read loop
      ;---------------------------------------

      nfile = 0  ; Number of file read
      line  = 0   ; Line index for input file.
      idx   = 0   ; Index for the data arrays.

      num_850 = 0
      num_500 = 0
      num_150 = 0
      num_0   = 0

      xhi_150 = 0.0
      bia_150 = 0.0
      rms_150 = 0.0
      xhi_500 = 0.0
      bia_500 = 0.0
      rms_500 = 0.0
      xhi_850 = 0.0
      rms_850 = 0.0
      bia_850 = 0.0

      total_num_obs = 0

      do while (line .lt. dimvar)

         ;------------------------------------------
         ; 3.1 Skip the very first line of each file
         ;------------------------------------------

         ; print ("Go back to "+(/fpr(line)/))
         if (fpr (line) .eq. first_line) then
             line = line + 1 ; Go to next line
             nfile = nfile + 1
             print ("----> Read data in "+allfiles(nfile-1)+" ("+nfile+"/"+numfiles+")")
         else
         if (fpr (line-1) .eq. first_line) then
             nfile = nfile + 1
             print ("----> Read data in "+allfiles(nfile-1)+" ("+nfile+"/"+numfiles+")")
         end if
         end if

         ;----------------------------
         ; 3.2 Date (ccyy-mm-dd_hh)
         ;----------------------------

         if (fpr (line) .eq. "Date") then
             line = line + 1 ; Skip date string
             ; print ("Date = "+(/fpr(line)/))
             date = fpr(line) ; read date ccyy-mm-dd_hh
             line = line + 1 ; Go to next line
         end if

         ;----------------------------
         ; 3.3 Number of observations
         ;----------------------------

         if (fpr (line) .eq. "Number of Observations") then
             line = line + 1 ; Skip string numobs
         end if

         ; print ("numobs = "+(/fpr(line)/))
         num_obs = stringtoint(fpr(line))
         line = line + 1 ; Go to next line
         total_num_obs = total_num_obs + num_obs

         ;------------------
         ; 3.4 Skip 2 lines
         ;------------------

         line = line + 2

         ;===============
         ; 4.  READ DATA
         ;===============

         ;-------------------------
         ; 4.1 Read numberobs data
         ;-------------------------

         do n = line,line+num_obs-1

            ;-------------------
            ; 4.2 Variable name
            ;-------------------

            s = fpr(n)

            namevar (idx) = sub_string(s,3,4)

            ;--------------------
            ; 4.3 Pressure level
            ;--------------------

            pres_mb (idx) = sub_string_int(s,10,14)

            ;-----------
            ; 4.4 Obs #
            ;-----------

            obs_num (idx) = sub_string_int(s,17,21)

            ;-----------------
            ; 4.5 Time hhmmss
            ;-----------------

            hhmmss (idx) =  sub_string_int(s,26,29)

            ;----------------
            ; 4.6 Station id
            ;----------------

            id (idx) = sub_string(s,32,36)

            ;-------------
            ; 4.7 WMO Code
            ;-------------

            platform (idx) = sub_string(s,40,50)

            ;-----------------
            ; 4.8 Surface flag
            ;-----------------

            is_sfc (idx) = sub_string(s,54,54)

            ;---------------
            ; 4.9 Obs value
            ;---------------

            obs (idx) = sub_string_float(s,59,67)

            ;------------------
            ; 4.10 Model value
            ;------------------

            mdl (idx) = sub_string_float (s,73,81)

            ;----------------
            ; 4.11 Obs error
            ;----------------

            err (idx) = sub_string_float(s,89,95)

            if (err (idx) .le. 0 .and. \
                platform(idx) .eq. obs_type .and. namevar(idx) .eq. obs_var)then

                 if (obs_var .eq. "RH") then
                     print ("      Obs error for obs "+obs_type+" "+obs_var+\
                                 " is 0, set to 20%")
                     err (idx) = 20.
                 else
                     print ("      Obs error for obs "+obs_type+" "+obs_var+\
                                 " is 0, skip it")
                     continue
                 end if
            end if

            ;------------------------
            ; 4.12 Model coordinates
            ;------------------------

            xmdl (idx) = sub_string_float(s,101,108)
            ymdl (idx) = sub_string_float(s,111,118)

            ;--------------
            ; 4.13 QC flag
            ;--------------

            qcmdl (idx) = sub_string_int(s,126,127)

            ;---------------------
            ; 4.14 Rename profiler
            ;---------------------

            idd = stringtochar(id(idx))
            if (platform(idx) .eq. "FM-12 SYNOP".and. is_sfc(idx) .eq. "F".and.\
                idd(1) .ne. "0" .and. idd(1) .ne. "1" .and. \
                idd(1) .ne. "2" .and. idd(1) .ne. "3" .and. \
                idd(1) .ne. "4" .and. idd(1) .ne. "5" .and. \
                idd(1) .ne. "6" .and. idd(1) .ne. "7" .and. \
                idd(1) .ne. "8" .and. idd(1) .ne. "9") then
                platform(idx) = "FM-12 PROF "
            end if
            delete (idd)

            ;------------------------------------------------------------
            ; 4.15 Copy data matching the requested platform and variable
            ;------------------------------------------------------------

            if ((platform(idx) .eq. obs_type) .and. (namevar(idx) .eq. obs_var)) then
               if (pres_mb(idx) .gt. 700) then
                  num_850 = num_850 + 1
               end if

               if ((pres_mb(idx) .gt. 300) .and. (pres_mb(idx) .le. 700)) then
                  num_500 = num_500 + 1
               end if

               if (pres_mb(idx) .le. 300) then
                  num_150 = num_150 + 1
               end if

            end if

            ;---------------------
            ; 4.16 Go to next line
            ;---------------------

            idx = idx + 1

            delete(s)

         end do

         line = line + num_obs

         ;--------------------------------------------
         ; 4.17 Skip the headers between the data sets
         ;--------------------------------------------

         if (line .lt. dimvar) then
            if (fpr(line) .eq. "Date") then 
               line = line + 3
            else
               line = line + 1
            end if
         end if 

      end do

      ;================================
      ; Create arrays to store the data
      ;================================
       
      if (num_850 .gt. 0) then
      data_850_x = new ((/7,num_850/),float)
      data_850_y = new ((/7,num_850/),float)
      end if

      if (num_500 .gt. 0) then
      data_500_x = new ((/7,num_500/),float)
      data_500_y = new ((/7,num_500/),float)
      end if

      if (num_150 .gt. 0) then
      data_150_x = new ((/7,num_150/),float)
      data_150_y = new ((/7,num_150/),float)
      end if

      ndata = ndata + num_850 + num_500 + num_150

      ;==========
      ; Sort data
      ;==========

      idx_850 = 0
      idx_500 = 0
      idx_150 = 0

      do I=0,total_num_obs-1
         if ((platform(I) .eq. obs_type) .and. (namevar(I) .eq. obs_var)) then

            if (qcmdl(I) .eq. 0) then
               q_idx = 1
            end if

            if ((qcmdl(I) .eq. 1) .or. (qcmdl(I) .eq. 2)) then
               q_idx = 2
            end if

            if ((qcmdl(I) .eq. 3) .or. (qcmdl(I) .eq. 4)) then
               q_idx = 3
            end if

            if ((qcmdl(I) .eq. 5) .or. (qcmdl(I) .eq. 6)) then
               q_idx = 4
            end if

            if ((qcmdl(I) .eq. 7) .or. (qcmdl(I) .eq. 8)) then
               q_idx = 5
            end if

            if ((qcmdl(I) .eq. 9) .or. (qcmdl(I) .eq. 10)) then
               q_idx = 6
            end if

            if ((qcmdl(I) .lt. 0) .or. (qcmdl(I) .gt. 10)) then
               print ("Unknown QC flag = "+sprinti("%i",qcmdl(I)))
               q_idx = 1
            end if

            if (pres_mb(I) .gt. 700) then

                xhi_850 = xhi_850 + ((obs(I)-mdl(I))/err(I))^2
                rms_850 = rms_850 + (obs(I)-mdl(I))^2
                bia_850 = bia_850 +  obs(I)-mdl(I)

                data_850_x(q_idx,idx_850) = obs(I)
                data_850_y(q_idx,idx_850) = mdl(I)

                idx_850 = idx_850 + 1
            end if

            if (pres_mb(I) .gt. 300) .and. (pres_mb(I) .le. 700) then

                xhi_500 = xhi_500 + ((obs(I)-mdl(I))/err(I))^2
                rms_500 = rms_500 + (obs(I)-mdl(I))^2
                bia_500 = bia_500 +  obs(I)-mdl(I)

                data_500_x(q_idx,idx_500) = obs(I)
                data_500_y(q_idx,idx_500) = mdl(I)

               idx_500 = idx_500 + 1
            end if

            if (pres_mb(I) .le. 300) then

                xhi_150 = xhi_150 + ((obs(I)-mdl(I))/err(I))^2
                rms_150 = rms_150 + (obs(I)-mdl(I))^2
                bia_150 = bia_150 +  obs(I)-mdl(I)

                data_150_x(q_idx,idx_150) = obs(I)
                data_150_y(q_idx,idx_150) = mdl(I)

                idx_150 = idx_150 + 1
            end if

         end if
      end do

      ;-----------------------------------------------------------------------
      ; T in C
      ;-----------------------------------------------------------------------

      if (num_850 .gt. 1 .and.  obs_var .EQ. "T ") then
         data_850_x = data_850_x - 273.15
         data_850_y = data_850_y - 273.15
      end if

      if (num_500 .gt. 1 .and.  obs_var .EQ. "T ") then
         data_500_x = data_500_x - 273.15
         data_500_y = data_500_y - 273.15
      end if

      if (num_150 .gt. 1 .and.  obs_var .EQ. "T ") then
         data_150_x = data_150_x - 273.15
         data_150_y = data_150_y - 273.15
      end if

      ;-----------------------------------------------------------------------
      ; Dynamic Data ranges
      ;-----------------------------------------------------------------------

      if (num_850 .gt. 1)
          min_data_850_x = min(data_850_x)
          max_data_850_x = max(data_850_x)
          min_data_850_y = min(data_850_y)
          max_data_850_y = max(data_850_y)
      end if
      if (num_500 .gt. 1)
          min_data_500_x = min(data_500_x)
          max_data_500_x = max(data_500_x)
          min_data_500_y = min(data_500_y)
          max_data_500_y = max(data_500_y)
      end if
      if (num_150 .gt. 1)
          min_data_150_x = min(data_150_x)
          max_data_150_x = max(data_150_x)
          min_data_150_y = min(data_150_y)
          max_data_150_y = max(data_150_y)
      end if

      ;-----------------------------------------------------------------------
      ; Fixed axes
      ;-----------------------------------------------------------------------

      if (num_850 .gt. 1 .and.  obs_var .EQ. "T ") then
          min_data_850_x =   0 + 1
          max_data_850_x =  40 - 1
          min_data_850_y =   0 + 1
          max_data_850_y =  40 - 1
      end if

      if (num_500 .gt. 1 .and.  obs_var .EQ. "T ") then
          min_data_500_x = -30 + 1
          max_data_500_x =  10 - 1
          min_data_500_y = -30 + 1
          max_data_500_y =  10 - 1
      end if

      if (num_150 .gt. 1 .and.  obs_var .EQ. "T ") then
          min_data_150_x = -70 + 1
          max_data_150_x = -30 - 1
          min_data_150_y = -70 + 1
          max_data_150_y = -30 - 1
      end if

      if (num_850 .gt. 1 .and.  (obs_var .EQ. "U " .or. obs_var .EQ. "V ")) then
          min_data_850_x = -10 + 1
          max_data_850_x =  30 - 1
          min_data_850_y = -10 + 1
          max_data_850_y =  30 - 1
      end if

      if (num_500 .gt. 1 .and.  (obs_var .EQ. "U " .or. obs_var .EQ. "V ")) then
          min_data_500_x = -10 + 1
          max_data_500_x =  30 - 1
          min_data_500_y = -10 + 1
          max_data_500_y =  30 - 1
      end if

      if (num_150 .gt. 1 .and.  (obs_var .EQ. "U " .or. obs_var .EQ. "V ")) then
          min_data_150_x = -10 + 1
          max_data_150_x =  30 - 1
          min_data_150_y = -10 + 1
          max_data_150_y =  30 - 1
      end if

      if (num_850 .gt. 1 .and.  obs_var .EQ. "RH") then
          min_data_850_x =   0 + 1
          max_data_850_x = 100 - 1
          min_data_850_y =   0 + 1
          max_data_850_y = 100 - 1
      end if

      if (num_500 .gt. 1 .and.  obs_var .EQ. "RH") then
          min_data_500_x =   0 + 1
          max_data_500_x = 100 - 1
          min_data_500_y =   0 + 1
          max_data_500_y = 100 - 1
      end if

      if (num_150 .gt. 1 .and.  obs_var .EQ. "RH") then
          min_data_150_x =   0 + 1
          max_data_150_x = 100 - 1
          min_data_150_y =   0 + 1
          max_data_150_y = 100 - 1
      end if

      ;-----------------------------------------------------------------------
      ; Create the diagonal line.
      ;-----------------------------------------------------------------------

      if (num_850 .gt. 1) then
      data_850_x(0,:) = fspan(min_data_850_x-1,max_data_850_x+1,num_850)
      data_850_y(0,:) = fspan(min_data_850_y-1,max_data_850_y+1,num_850)
      end if

      if (num_500 .gt. 1) then
      data_500_x(0,:) = fspan(min_data_500_x-1,max_data_500_x+1,num_500)
      data_500_y(0,:) = fspan(min_data_500_y-1,max_data_500_y+1,num_500)
      end if

      if (num_150 .gt. 1) then
      data_150_x(0,:) = fspan(min_data_150_x-1,max_data_150_x+1,num_150)
      data_150_y(0,:) = fspan(min_data_150_y-1,max_data_150_y+1,num_150)
      end if

      ;-----------------------------------------------------------------------
      ; Xhi diagnostics
      ;-----------------------------------------------------------------------

      if (num_150 .gt. 1) then
          xhi_150 = xhi_150/num_150
          bia_150 = bia_150/num_150
          rms_150 = sqrt (rms_150/num_150)
      end if
      if (num_500 .gt. 1) then
          xhi_500 = xhi_500/num_500
          bia_500 = bia_500/num_500
          rms_500 = sqrt (rms_500/num_500)
      end if
      if (num_850 .gt. 1) then
          xhi_850 = xhi_850/num_850
          bia_850 = bia_850/num_850
          rms_850 = sqrt (rms_850/num_850)
      end if

      ;-----------------------------------------------------------------------
      ; Print out
      ;-----------------------------------------------------------------------

      if (num_150 .gt. 1.0) then
      print ("----> Found "+sprinti("%4i ",num_150)+obs_var+" above  300hPa for "+obs_type)
      print ("      BIAS =  " +sprintf("%4.1f",bia_150)+ \
                  "  RMS = "  +sprintf("%4.1f",rms_150)+ \
                  "    XHI/"+num_150+" = "+sprintf("%4.1f",xhi_150))
      end if

      if (num_500 .gt. 1.0) then
      print ("----> Found "+sprinti("%4i ",num_500)+obs_var+" in 300-700hPa for "+obs_type)
      print ("      BIAS =  " +sprintf("%4.1f",bia_500)+ \
                  "  RMS = "  +sprintf("%4.1f",rms_500)+ \
                  "    XHI/"+num_500+" = "+sprintf("%4.1f",xhi_500))
      end if

      if (num_850 .gt. 1.0) then
      print ("----> Found "+sprinti("%4i ",num_850)+obs_var+" below  700hPa for "+obs_type)
      print ("      BIAS =  " +sprintf("%4.1f",bia_850)+ \
                  "  RMS = "  +sprintf("%4.1f",rms_850)+ \
                  "    XHI/"+num_850+" = "+sprintf("%4.1f",xhi_850))
      end if

      ;===============================
      ; 5.  Open window
      ;===============================

;     if (xhi_150 .gt. 0.01 .or. xhi_500 .gt. 0.01 .or. xhi_850 .gt. 0.01) then
      if (num_150 .gt. 1.0  .or. num_500 .gt. 1.0 .or. num_850 .gt. 1.0) then

          print ("----> Plotting data for "+obs_var+" in "+fileout+"."+wks_type)

          if (.not. wks_opened) then
              wks = gsn_open_wks (wks_type, fileout) ; open workstation
              cmap = (/"White","Black","Blue","Red","Green","Yellow","Cyan","Magenta"/)
              gsn_define_colormap(wks,cmap)
              wks_opened = True
          end if

      ;=======================
      ; 6.  Plotting resources
      ;=======================

      res   = True
      res@gsnMaximize  = True

      ;================================
      ; 7. Strings common to all plots
      ;================================

      res@tiMainString   = filenames
      res@tiMainFontHeightF  = .020

      res@tiXAxisString  = obs_var+" Observations"
      res@tiXAxisFontHeightF = .020

      res@tiYAxisString  = obs_var+" Model"
      res@tiYAxisFontHeightF = .020

      res@gsnLeftString       = obs_type             ; left title

;     res@gsnStringFontHeightF = .020
;     res@gsnFontHeightF = .020

      ;===================
      ; 8. Colors and size
      ;===================

      res@xyMarkLineModes = (/"Lines","Markers","Markers","Markers","Markers","Markers", "Markers"/)
      res@xyMarkers       = (/2, 2, 2, 2, 2, 2, 2/)
      res@xyMarkerColors  = (/"black","red","magenta","green","cyan","blue","black"/)

      res@xyMarkerThicknessF= 1.0
      res@xyMarkerSizeF       = 0.015

      ;============
      ; 9. Legends
      ;============

      res@pmLegendDisplayMode    = "Always"   ; Turn on drawing legend.
      res@pmLegendSide           = "Bottom"
      res@pmLegendZone           = 0                  ; Change the location
      res@pmLegendOrthogonalPosF = 0.18               ; of the legend

      res@pmLegendParallelPosF   = .30                ; move units right
      res@pmLegendOrthogonalPosF =  -0.38             ; move units up

      res@pmLegendWidthF         = 0.25               ; Change width and
      res@pmLegendHeightF        = 0.18               ; height of legend.

      res@pmLegendSide           = "Top"              ; Change location of
      res@lgPerimOn              = False              ; legend and turn off
                                                      ; the perimeter.
      res@lgLabelFontHeightF     = 0.0125

      res@xyExplicitLegendLabels = (/"", \
                                     "QC = 0","QC = 1-2","QC = 3-4" \
                                    ,"QC = 5-6","QC = 7-8","QC = 9-10"/)

      ;-----------------------------------------------------------------------
      ; Create the plot for the pressures below 700hPa.
      ;-----------------------------------------------------------------------

      if (num_850 .gt. 1) then

      ; Determine the x-axis values

      res@trXMinF        = min_data_850_x-1
      res@trXMaxF        = max_data_850_x+1

      ; Determine the y-axis values

      res@trYMinF        = min_data_850_y-1
      res@trYMaxF        = max_data_850_y+1

      res@tiYAxisString  = "Total: "+sprinti("%d",num_850)+" obs."
      res@tiXAxisString  = " ~C~"+obs_var+" "+obs_type+"    p > 750hPa"+\
        "~C~     "+"bias = "+sprintf("%4.1f",bia_850)+\
                  ", rms = "+sprintf("%4.1f",rms_850)

      plot  = gsn_csm_xy (wks,data_850_x,data_850_y,res)

      else
        print ("----> No "+obs_var+" data below 700hPa for "+obs_type)
      end if

      ;-----------------------------------------------------------------------
      ; Create the plot for the pressures between 300hPa and 700hPa
      ;-----------------------------------------------------------------------

      if (num_500 .gt. 1) then

      ; Determine the x-axis values

      res@trXMinF        = min_data_500_x-1
      res@trXMaxF        = max_data_500_x+1

      ; Determine the y-axis values

      res@trYMinF        = min_data_500_y-1
      res@trYMaxF        = max_data_500_y+1

      res@tiYAxisString  = "Total: "+sprinti("%d",num_500)+" obs."
      res@tiXAxisString  = " ~C~"+obs_var+" "+obs_type+"    300hPa < p <= 750hPa"+\
        "~C~     "+"bias = "+sprintf("%4.1f",bia_500)+\
                  ", rms = "+sprintf("%4.1f",rms_500)

      plot  = gsn_csm_xy (wks,data_500_x,data_500_y,res)

      else
        print ("----> No "+obs_var+" data between 300 and 700hPa for "+obs_type)
      end if

      ;-----------------------------------------------------------------------
      ; Create the plot for the pressures above 300hPa
      ;-----------------------------------------------------------------------

      if (num_150 .gt. 1) then

      ; Determine the x-axis values

      res@trXMinF        = min_data_150_x-1
      res@trXMaxF        = max_data_150_x+1

      ; Determine the y-axis values

      res@trYMinF        = min_data_150_y-1
      res@trYMaxF        = max_data_150_y+1

      res@tiXAxisString  = " ~C~"+obs_var+" "+obs_type+"    p <= 300hPa"+\
        "~C~     "+"bias = "+sprintf("%4.1f",bia_150)+\
                  ", rms = "+sprintf("%4.1f",rms_150)
      res@tiYAxisString  = "Total: "+sprinti("%d",num_150)+" obs."

      plot  = gsn_csm_xy (wks,data_150_x,data_150_y,res)

      else
        print ("----> No "+obs_var+" data above 300hPa for "+obs_type)
      end if

      else
        print ("----> No "+obs_var+" data for "+obs_type)
      end if

   ;========================================================================;
   ; 10 Clean up
   ;========================================================================;

      delete (namevar)
      delete (pres_mb)
      delete (obs_num)
      delete (hhmmss) 
      delete (id)    
      delete (platform)
      delete (is_sfc) 
      delete (obs)   
      delete (mdl)  
      delete (err)  
      delete (xmdl) 
      delete (ymdl) 
      delete (qcmdl)
      if (num_850 .gt. 0) then
      delete (data_850_x)
      delete (data_850_y)
      end if
      if (num_500 .gt. 0) then
      delete (data_500_x)
      delete (data_500_y)
      end if
      if (num_150 .gt. 0) then
      delete (data_150_x)
      delete (data_150_y)
      end if

    end do

    if (wks_opened) then
        delete (wks)
        delete (res)
        wks_opened = False
    end if

   end do

   print("")
   print("Plotted "+ndata+" obs. across "+sprinti("%3d",nfile)+" file(s).")
   print("")

   ;==========================
   ; 11. Print unknown stations
   ;==========================

   if (unknown_platforms) then

   print("Watch for unknown platforms...")

    l = 1
    line  = 0
    nfile = 0

    do while (line.lt. dimvar)

       s = fpr(line)

       if (s .eq. first_line) then
           l = 1
           nfile = nfile + 1
           print("Reading file "+allfiles(nfile-1)+" ("+nfile+"/"+numfiles+")")
       end if

       do n = 0, num_vars-1 

          if (sub_string(s,3,4) .eq. obs_vars(n)) then

              do nn = 0, num_types-1
                 if (sub_string(s,40,50) .eq. obs_types(nn)) then
                     break
                 end if
              end do

              if (nn .ge. num_types) then
                  print("---> Unknown platform "+sub_string(s,40,50)+" at line "+l)
              end if

              break

          end if

       end do

       line = line + 1
       l = l + 1

   end do

   print("")
   print ("The requested platforms were:")

   print((/obs_types/))

   print("")

   end if

   ;=====
   ; END
   ;=====

end
