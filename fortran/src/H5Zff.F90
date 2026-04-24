!> @defgroup FH5Z Fortran Filter (H5Z) Interface
!!
!! @see H5Z, C-API
!!
!! @see @ref H5Z_UG, User Guide
!!

!> @ingroup FH5Z
!!
!! @brief This module contains Fortran interfaces for H5Z functions.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the LICENSE file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5Z function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5Z

  USE H5GLOBAL
  USE H5fortkit
  IMPLICIT NONE

CONTAINS

!>
!! \ingroup FH5Z
!!
!! \brief Unregisters specified filters.
!!
!! \param filter Filter; may have one of the following values:
!!               \li H5Z_FILTER_DEFLATE_F
!!               \li H5Z_FILTER_SZIP_F
!!               \li H5Z_FILTER_NBIT_F
!!               \li H5Z_FILTER_SCALEOFFSET_F
!!               \li H5Z_FILTER_SHUFFLE_F
!!               \li H5Z_FILTER_FLETCHER32_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Zunregister()
!!
  SUBROUTINE h5zunregister_f(filter, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5zunregister_c(filter) BIND(C,NAME='h5zunregister_c')
         INTEGER, INTENT(IN) :: filter
       END FUNCTION h5zunregister_c
    END INTERFACE
    hdferr = h5zunregister_c(filter)
  END SUBROUTINE h5zunregister_f

!>
!! \ingroup FH5Z
!!
!! \brief Queries if filter is available
!!
!! \param filter  Filter; may be one of the following:
!!                \li H5Z_FILTER_DEFLATE_F
!!                \li H5Z_FILTER_SZIP_F
!!                \li H5Z_FILTER_NBIT_F
!!                \li H5Z_FILTER_SCALEOFFSET_F
!!                \li H5Z_FILTER_SHUFFLE_F
!!                \li H5Z_FILTER_FLETCHER32_F
!! \param status  Flag; .TRUE. if filter is available, .FALSE. otherwise.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Zfilter_avail()
!!
  SUBROUTINE h5zfilter_avail_f(filter, status, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

    INTERFACE
       INTEGER FUNCTION h5zfilter_avail_c(filter, flag) BIND(C,NAME='h5zfilter_avail_c')
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: filter
         INTEGER :: flag
       END FUNCTION h5zfilter_avail_c
    END INTERFACE

    hdferr = h5zfilter_avail_c(filter, flag)
    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5zfilter_avail_f

!>
!! \ingroup FH5Z
!!
!! \brief Queries if filter has its encoder and/or decoder available.
!!
!! \param filter       Filter; may be one of the following:
!!                     \li H5Z_FILTER_DEFLATE_F
!!                     \li H5Z_FILTER_SZIP_F
!!                     \li H5Z_FILTER_NBIT_F
!!                     \li H5Z_FILTER_SCALEOFFSET_F
!!                     \li H5Z_FILTER_SHUFFLE_F
!!                     \li H5Z_FILTER_FLETCHER32_Ffilter
!! \param config_flags Flag, indicates if filter has its encoder and/or decoder available, possible values:
!!                     \li H5Z_FILTER_ENCODE_ENABLED_F
!!                     \li H5Z_FILTER_DECODE_ENABLED_F
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Zget_filter_info()
!!
  SUBROUTINE h5zget_filter_info_f(filter, config_flags, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: config_flags
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5zget_filter_info_c(filter, config_flags) BIND(C,NAME='h5zget_filter_info_c')
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: filter
         INTEGER, INTENT(OUT) :: config_flags
       END FUNCTION h5zget_filter_info_c
    END INTERFACE

    hdferr = h5zget_filter_info_c(filter, config_flags)

  END SUBROUTINE h5zget_filter_info_f

!>
!! \ingroup FH5Z
!!
!! \brief Retrieves a single named parameter from a filter parameter string.
!!
!! \param params    Full parameter string (e.g. "level=6,mode=fast").
!! \param key       Name of the parameter to retrieve.
!! \param value_buf Buffer to receive the value string.
!! \param buf_size  On entry: size of value_buf; on exit: length of value found.
!! \param hdferr    Returns > 0 if found, 0 if not found, -1 on error.
!!
!! See C API: @ref H5Zconfig_get_param()
!!
  SUBROUTINE h5zconfig_get_param_f(params, key, value_buf, buf_size, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)    :: params
    CHARACTER(LEN=*), INTENT(IN)    :: key
    CHARACTER(LEN=*), INTENT(OUT)   :: value_buf
    INTEGER(SIZE_T),  INTENT(INOUT) :: buf_size
    INTEGER,          INTENT(OUT)   :: hdferr

    CHARACTER(LEN=LEN_TRIM(params)+1,KIND=C_CHAR)      :: c_params
    CHARACTER(LEN=LEN_TRIM(key)+1,KIND=C_CHAR)         :: c_key
    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(value_buf)+1) :: c_valbuf
    INTEGER(SIZE_T)                                     :: c_bufsz

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zconfig_get_param(params_c, key_c, value_buf_c, buf_size_c) &
            BIND(C,NAME='H5Zconfig_get_param')
         IMPORT :: C_INT, C_CHAR, SIZE_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)    :: params_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)    :: key_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT)   :: value_buf_c
         INTEGER(SIZE_T),                      INTENT(INOUT) :: buf_size_c
       END FUNCTION H5Zconfig_get_param
    END INTERFACE

    c_params = TRIM(params)//C_NULL_CHAR
    c_key    = TRIM(key)//C_NULL_CHAR
    c_bufsz  = INT(LEN(value_buf), SIZE_T)
    hdferr   = INT(H5Zconfig_get_param(c_params, c_key, c_valbuf, c_bufsz))
    buf_size = c_bufsz
    IF (hdferr > 0) &
       CALL HD5c2fstring(value_buf, c_valbuf, LEN(value_buf, KIND=SIZE_T), &
                         LEN(value_buf, KIND=SIZE_T)+1_SIZE_T)
  END SUBROUTINE h5zconfig_get_param_f

END MODULE H5Z





