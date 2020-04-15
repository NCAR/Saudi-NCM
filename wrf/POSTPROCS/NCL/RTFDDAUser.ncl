
undef ("read_map_ascii")

procedure read_map_ascii(fili, xlatmap, xlonmap, nptsline)

local data, total_pts, icount, lcount, numvals, numpts, \
      n_to_assign, igood, xlatmap, xlonmap

begin

;==========================================================================
; This procedure reads range boundary data (latitude and longitude) from an
; ascii file and returns lat,lon and the total number of points and the 
; number of points from each line.  The points are plotted as groups from
; each line to avoid cross-over in the plot.
;
; Input:
;   fili:  path and file name with range boundary data in ascii format
;
; Output:
;   xlatmap  : latitude points of range boundary
;   xlonmap  : longitude points of range boundary
;   nptsline : number of lat/lon points in each line of the ascii file
;   igood    : number of lat/longitude points of range boundary
;==========================================================================
;

   ;--------------------------------------------------------------------------
   ; Read all values from file and put into a giant 1D array to remove 
   ; dependence on file size
   ;--------------------------------------------------------------------------
   data   = asciiread (fili, -1, "float") 
   total_pts = dimsizes(data)
   print ("total_pts = " + total_pts)
   ;--------------------------------------------------------------------------
   ; Make two gigantic arrays for lat and lon points, fill with missing values
   ; to use then determining the number of lat/lon points that we have.
   ;--------------------------------------------------------------------------
   xlatmap  = new((/1000/),float)
   xlonmap  = new((/1000/),float)
   nptsline = new((/1000/),integer)
   xlatmap@_FillValue = -999           ; sets _FillValue to -999
   xlonmap@_FillValue = -999           ; sets _FillValue to -999
   nptsline@_FillValue = -999           ; sets _FillValue to -999
   xlatmap = -999
   xlonmap = -999
   nptsline = -999

   ;--------------------------------------------------------------------------
   ; first 7 values of some lines are basic info
   ;--------------------------------------------------------------------------
   icount = 0       ; Counts total number of points in the data array
   lcount = 0       ; Counts lat/lon points in the data array
   jcount = 0       ; Counts number of lines in the ascii file

   do while (icount .lt. total_pts)

;     print("icount = " + icount)
;     print("lcount = " + lcount)
      numvals = data(icount)     ; Number of lat + lon values in line
      nptsline(jcount) = floattoint(numvals)
;     xlatmin = data(icount+1)   ; Minimum lat in line (Not used)
;     xlatmax = data(icount+2)   ; Maximum lat in line (Not used)
;     xlonmin = data(icount+3)   ; Minimum lon in line (Not used)
;     xlonmax = data(icount+4)   ; Maximum lon in line (Not used)
      xlatmap(lcount) = data(icount+5)   ; First lat of boundary
      xlonmap(lcount) = data(icount+6)   ; First lon of boundary
      numpts = floattoint(numvals/2)
;     print("numvals = " + numvals)
;     print("xlatmap(lcount) = " + xlatmap(lcount))
;     print("xlonmap(lcount) = " + xlonmap(lcount))
;     print("npts = " + numpts)
      icount = icount + 7
      lcount = lcount + 1
   ;--------------------------------------------------------------------------
   ; Now assign the remainder of the points that were on the next line
   ;--------------------------------------------------------------------------
      n_to_assign = numpts - 1    ; Since we already have one point

      do i = lcount,lcount + n_to_assign -1
         xlatmap(i) = data(icount)
         xlonmap(i) = data(icount+1)
         icount = icount + 2
      end do

      lcount = lcount + n_to_assign
      jcount = jcount + 1

   end do

   igood = ind(.not.ismissing(xlatmap))  ; indices of non-missing values
   ngood = dimsizes(igood)

   print ("jcount = " + jcount)
   print ("Min = " + min(nptsline) + "Max = " + max(nptsline))
end
