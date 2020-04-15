FUNCTION add_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION subtract_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION multiply_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION divide_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0 ) THEN
      IF ( val1.EQ.0 ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0. ) THEN
      IF ( val1.EQ.0 ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0 ) THEN
      IF ( val1.EQ.0. ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0. ) THEN
      IF ( val1.EQ.0. ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION remainder_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = mod ( val1 , val2 )
   ENDIF
ENDFUNCTION

FUNCTION remainder_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = mod ( val1 , val2 )
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION minus_i ( val1 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = -val1
   ENDIF
ENDFUNCTION

FUNCTION minus_r ( val1 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = -val1
   ENDIF
ENDFUNCTION

