MODULE DA_Decoded_Module

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Setup_Structures
   USE DA_Grid_Definitions
   USE DA_ReadWrite_MM5

   IMPLICIT NONE

CONTAINS

#     include <DA_Decoded.inc>
#     include <DA_clean_decoded.inc>
#     include <DA_count_levels.inc>
#     include <DA_count_stations.inc>
#     include <DA_diagnostics.inc>
#     include <DA_duplicate.inc>
#     include <DA_error_levels.inc>
#     include <DA_fm_code.inc>
#     include <DA_fm_decoder.inc>
#     include <DA_func.inc>
#     include <DA_h_to_k.inc>
#     include <DA_interp1d.inc>
#     include <DA_interp2d.inc>
#     include <DA_interp3d.inc>
#     include <DA_iserr_wmo.inc>
#     include <DA_merge.inc>
#     include <DA_oberr_unit.inc>
#     include <DA_open_unit.inc>
#     include <DA_p_to_k.inc>
#     include <DA_qc_fdda.inc>
#     include <DA_read_decoded.inc>
#     include <DA_recoverhp.inc>
#     include <DA_sigma_intp.inc>
#     include <DA_sort.inc>
#     include <DA_sort_platform.inc>
#     include <DA_intp_wrf.inc>
#     include <DA_intp_err.inc>
#     include <DA_write_decoded.inc>
#     include <DA_x_to_j.inc>
#     include <DA_error_handler.inc>

END MODULE DA_Decoded_Module
