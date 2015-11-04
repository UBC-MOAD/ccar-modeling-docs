
Reading netCDF4 file in :kbd:`MY_TRC`
************************ 

This section describes the steps to read an user-defined nerCDF4 file during the time stepping of :kbd:`MY_TRC` in NEMO v3.4. This is tested on ORCA 2 degree configuration. 
Similar approaches can be done in ANHA4 cases.   

Edit the namelist
=================

Edit the namelist of :kbd:`MY_TRC`:

.. code-block:: bash
  
      cd $your_case/EXP00/
      vim namelist_my_trc

And here is an example:

.. note::

    The examples here and below are requiring a netCDF4 file in :kbd:`EXP00` which contains **nav_lat**, **nav_lon**, **time_counter** and **var_name**.
    The netCDF4 file should have the same longtitude/latitude dimensions as the file :kbd:`coordinate.nc` in your case. The time dimension should be
    "UNLIMITED" and **var_name** is the variable you want :kbd:`MY_TRC` to read. 

    The arrangment of the dimensions (For Python users) should be (TIME, LAT, LON). 


:file:`namelist_my_trc`::

 &namelist_section
 !,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
 !      ! file name ! frequency (hours) ! variable ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation !
 !      !           !  (if <0  months)  !   name   !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  !
 sn_var ='file_name',        -12        ,'var_name',    .false.   , .true. ,  'yearly' ,    ''    ,    ''
 cn_dir = './'
 /

*  **frequency** is the reading frequency of the variable in time dimension
*  **clim** is the flag of file batching ...
*  A section should be ended with :kbd:`/`


Edit :kbd:`MY_TRC` code
=======================

* Go to the :kbd:`MY_SRC` of your case;
* Copy :kbd:`trcini_my_trc.F90`, :kbd:`trcnam_my_trc.F90` and :kbd:`trcsms_my_trc.F90` from the NEMO script.

.. code-block:: bash
  
      cd ${your_case}/MY_SRC
      cp ${NEMO-CODE}/NEMOGCM/NEMO/TOP_SRC/MY_TRC/trc*_my_trc.F90 .

Add the following FORTRAN code blocks

In :kbd:`trcini_my_trc.F90`, change the following:

.. code-block:: fortran
 IMPLICIT NONE
 PRIVATE

 PUBLIC   trc_ini_my_trc   ! called by trcini.F90 module
 CONTAINS

 SUBROUTINE trc_ini_my_trc
   IF(trc_sms_my_trc_alloc() /= 0) THEN
      CALL ctl_stop('STOP', 'trc_ini_my_trc: unable to allocate MY_TRC arrays')
   ! Assign structure
   CALL fld_fill(sf_var, (/sn_var/), cn_dir, 'trc_ini_my_trc', 'docs', 'namelist_section')
 END SUBROUTINE trc_ini_my_trc

:file:`trcnam_my_trc.F90`::

 IMPLICIT NONE
 PRIVATE

 PUBLIC   trc_nam_my_trc   ! called by trcnam.F90 module

 CONTAINS

 SUBROUTINE trc_nam_my_trc
   INTEGER :: numnatl
   NAMELIST/namelist_section/ cn_dir, sn_var
   CALL ctl_opn(numnatl, 'namelist_my_trc', 'OLD', 'FORMATTED', 'SEQUENTIAL', 1, numout, .FALSE.)
   REWIND(numnatl)
   READ  (numnatl, namelist_section)
 END SUBROUTINE trc_nam_my_trc

In :kbd:`trcsms_my_trc.F90`, add the following:

.. code-block:: fortran
 IMPLICIT NONE
 PUBLIC

 PUBLIC   trc_sms_my_trc       ! called by trcsms.F90 module
 PUBLIC   trc_sms_my_trc_alloc ! called by trcini_my_trc.F90 module

 CHARACTER(len=100), PUBLIC :: cn_dir = './'    ! Root directorY
 TYPE(FLD_N) :: sn_var                          ! information about the file to be read
 REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: var   ! Array receives the value from netCDF
 TYPE(FLD), ALLOCATABLE, DIMENSION(:) :: sf_var ! structure variable (PUBLIC for TAM)

 CONTAINS

 SUBROUTINE trc_sms_my_trc( kt )
   INTEGER, INTENT(in) :: kt ! ocean e-step index
   INTEGER :: i, j
   IF(nn_timing == 1) CALL timing_start('trc_sms_my_trc')
   !
   CALL fld_read (kt, 1, sf_var)
   var(:, :) = sf_var(1)%fnow(:, :, 1)
   ! More code ...
 END SUBROUTINE trc_sms_my_trc


 INTEGER FUNCTION trc_sms_my_trc_alloc()
   INTEGER :: ierror
   ALLOCATE(var(jpi,jpj), STAT=trc_sms_my_trc_alloc)
   ALLOCATE(sf_var(1), STAT=ierror)
   IF(ierror > 0) THEN
      CALL ctl_stop('trc_sms_my_trc_alloc: unable to allocate');
      RETURN
   ENDIF
   ALLOCATE(sf_var(1)%fnow(jpi, jpj, 1))
   IF(trc_sms_my_trc_alloc /= 0) THEN
      CALL ctl_warn('trc_sms_my_trc_alloc : failed to allocat')
 END FUNCTION trc_sms_my_trc_alloc
 
 For 4 dimension variables (time dimension has been subtrackted by keyword "frequency" in the namelist): var(:, :, :) = sf_var(1)%fnow(:, :, :)




 




