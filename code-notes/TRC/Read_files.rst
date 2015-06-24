
Reading files in :kbd:`MY_TRC`
************************ 

This section describes the steps to read an user-defined nerCDF file during the time stepping of :kbd:`MY_TRC` in NEMO v3.4.

Edit the namelist
===============================

Edit the namelist of :kbd:`MY_TRC`:

.. code-block:: bash
  
      cd $your_case/EXP00/
      vim namelist_my_trc

And here is an example:

.. note::

    The examples here and below are requiring a netCDF file in :kbd:`EXP00` which contains *nav_lat*, *nav_lon*, *time_counter* and *var_name*.
    The netCDF file should have the same longtitude/latitude dimensions as the file :kbd:`coordinate.nc` in your case. The time dimension should be
    "UNLIMITED" and *var_name* is the variable you want :kbd:`MY_TRC` to read. 

    The arrange of the dimensions (For Python users) should be (TIME, LAT, LON) 


:file:`namelist_my_trc`::

  &namelist_section
  !,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
  !              !     file name     ! frequency (hours) !    variable    ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation !
  !              !                   !  (if <0  months)  !      name      !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  !
   sn_var        =    'file_name'    ,        -12        ,   'var_name'   ,    .false.   , .true. ,  'yearly' ,    ''    ,    ''
   cn_dir        = './'
  /

*  *frequency* is -12 means the variable is stored as monthly mean;
*  *clim* is .true. with *yearly* means each month of the variable will repeat every year as a cycle
*  A section should end with :kbd:`/`


Edit MY_TRC code
================

* Go into the :kdb:`MY_SRC` of your case;
* Copy :kbd:`trcini_my_trc.F90`, :kbd:`trcnam_my_trc.F90` and :kbd:`trcsms_my_trc.F90` from the code base of :kbd:`MY_TRC`.

.. code-block:: bash
  
      cd $your_case/MY_SRC
      cp $NEMO-CODE/NEMOGCM/NEMO/TOP_SRC/MY_TRC/trc*_my_trc.F90 .

Add the following FORTRAN code blocks

:file:`trcini_my_trc.F90`::

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_my_trc   ! called by trcini.F90 module
   CONTAINS

   SUBROUTINE trc_ini_my_trc
      IF( trc_sms_my_trc_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_ini_my_trc: unable to allocate MY_TRC arrays' )
      ! Assign structure
      CALL fld_fill( sf_var   , (/ sn_var /)   , cn_dir, 'trc_ini_my_trc', 'documentation' , 'namelist_section' )
      ! sf_var ----- Structurfe defined at *sms* file
      ! sn_var ----- Variable info defined in the namelist_section
      IF( .NOT. ln_rsttr ) trn(:,:,:,jp_myt0:jp_myt1) = 0.

:file:`trcnam_my_trc.F90`::

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_my_trc   ! called by trcnam.F90 module

   CONTAINS

   SUBROUTINE trc_nam_my_trc
      INTEGER :: numnatl
      NAMELIST/namelist_section/ cn_dir, sn_var
      CALL ctl_opn( numnatl, 'namelist_my_trc', 'OLD', 'FORMATTED', 'SEQUENTIAL', 1, numout, .FALSE. )
      REWIND( numnatl )
      READ  ( numnatl, namelist_section )

:file:`trcsms_my_trc.F90`::

     IMPLICIT NONE
     PUBLIC

     PUBLIC   trc_sms_my_trc       ! called by trcsms.F90 module
     PUBLIC   trc_sms_my_trc_alloc ! called by trcini_my_trc.F90 module

     CHARACTER(len=100), PUBLIC :: cn_dir = './'    ! Root directory for location of river file
     TYPE(FLD_N) :: sn_var                          ! information about the Ba runoff file to be read
     REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: var   ! Array receives the value from netCDF
     TYPE(FLD), ALLOCATABLE, DIMENSION(:) :: sf_var ! structure variable (PUBLIC for TAM)

     CONTAINS

     SUBROUTINE trc_sms_my_trc( kt )
      INTEGER, INTENT(in) :: kt   ! ocean e-step index
      INTEGER :: i, j
      IF( nn_timing == 1 ) CALL timing_start('trc_sms_my_trc')
      !
      CALL fld_read ( kt, 1, sf_var  )
      IF(lwp) WRITE(numout,*) 'did the reading'
      var(:, :) = sf_var(1)%fnow(:, :, 1)




 




