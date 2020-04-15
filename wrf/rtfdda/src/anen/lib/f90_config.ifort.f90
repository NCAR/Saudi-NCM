PLACE MARKER
2009-dec-13

ifort's iostat for "end of record" (unformatted???) is:

either 620 or 758

(Or maybe something else)

-------------------------------------

Might only be used by read_prcip_rdf.f90, but defer until then.

Current gfortran value will cause read_precip to bomb as
soon as the first case is encountered.

Almost any rain data set before december 2009 will trigger it.
