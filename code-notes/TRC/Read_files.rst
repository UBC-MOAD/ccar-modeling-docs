
Reading netCDF4 file in :kbd:`MY_TRC`
************************************* 

This section describes the ways to read an user-defined nerCDF4 file during the time stepping of :kbd:`MY_TRC` in NEMO 3.4.

Through NEMO's existing file channel
====================================

By replacing the variable information in :kbd:`namelist/&namdta_dyn` to your own, the tracer model can read your files during the simulation. 
An example of it can be found in `here`_. 
`(this section needs more explainations)`

.. _here: http://ccar-modeling-documentation.readthedocs.io/en/latest/code-notes/TRC/Calculate_emps_ANHA4.html#use-namelist-namdta-dyn

By creating new namelist
========================


.. note::

    The examples here and below are requiring a netCDF4 file in :kbd:`EXP00` which contains **nav_lat**, **nav_lon**, **time_counter** and **var_name**.
    The netCDF4 file should have the same longtitude/latitude dimensions as the file :kbd:`coordinate.nc` in your case. The time dimension should be
    "UNLIMITED" and **var_name** is the variable you want :kbd:`MY_TRC` to read. 

    The arrangment of the dimensions (For Python users) should be (TIME, LAT, LON). 



Create :kbd:`namelist_my_trc`
-------------------------------------

In :kbd:`$your_case/EXP00/`, create the file :file:`namelist_my_trc`::

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


Edit :kbd:`MY_TRC` scripts
---------------------------

The structure and access information can be added in :kbd:`trcini_my_trc.F90` and :kbd:`trcnam_my_trc.F90`. When the variable is read,
it can be used in :kbd:`trcsms_my_trc.F90`.

In :kbd:`trcini_my_trc.F90`, assign the structure of your file by :kbd:`fld_fill`. 

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


In :kbd:`trcnam_my_trc.F90`, read the variable through :kbd:`ctl_opn`. The name of the namelist :kbd:`namelist_my_trc`
should be consistent with the one created in the section above.  

.. code-block:: fortran

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

:kbd:`trcsms_my_trc.F90` call the two scripts above and allocates the array.

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
 
For 4 dimension variables (time dimension has been subtrackted by keyword "frequency" in the namelist): var(:, :, :) = sf_var(1)%fnow(:, :, :).




 




