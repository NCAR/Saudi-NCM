      PROGRAM dates
      IMPLICIT INTEGER (A-Z)

      READ *, cymdh1, cymdh2

      h1 = mod ( cymdh1 , 100 )
      h2 = mod ( cymdh2 , 100 )
      cymd1 = cymdh1 / 100
      cymd2 = cymdh2 / 100

      n = 0
10    if ( cymd1 .ge. cymd2 ) goto 20
      cymd1 = nxcymd ( cymd1, 1 )
      n = n + 1
      goto 10

20    if ( cymd1 .le. cymd2 ) goto 30
      cymd1 = nxcymd ( cymd1, -1 )
      n = n - 1
      goto 20

30    print *,  n*24 + ( h2 - h1 )
      STOP
      END
!
      function nxcymd ( ccyymmdd, delta )
      IMPLICIT INTEGER (A-Z)
      INTEGER mmday(12)
      DATA    mmday/31,28,31,30,31,30,31,31,30,31,30,31/

      ccyy = ccyymmdd / 10000
      mm   = mod ( ccyymmdd , 10000 ) / 100
      dd   = mod ( ccyymmdd , 100 )

      mmday(2) = 28
      IF ( MOD(ccyy,4) .EQ. 0 ) THEN
         IF     ( MOD(ccyy,400) .EQ. 0 ) THEN
            mmday(2) = 29
         ELSEIF ( MOD(ccyy,100) .NE. 0 ) THEN
            mmday(2) = 29
         ENDIF
      ENDIF

      dd = dd + delta
      IF ( dd .EQ. 0 ) THEN
         mm = mm - 1
         IF ( mm .EQ. 0 ) THEN
            mm = 12
            ccyy = ccyy - 1
         ENDIF
         dd = mmday(mm)
      ELSEIF ( dd .GT. mmday(mm) ) THEN
         dd = 1
         mm = mm + 1
         IF ( mm .GT. 12 ) THEN
            mm = 1
            ccyy = ccyy + 1
         ENDIF
      ENDIF
      nxcymd = ccyy*10000 + mm*100 + dd
      RETURN
      END
