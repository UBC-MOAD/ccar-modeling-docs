
Adding user-defined tracers
*************************** 

This section describes the way to build-up the tracer scheme in NEMO 3.4 based on the :kbd:`MY_TRC` module.

Edit :kbd:`MY_TRC` files
========================

The following files are useful when adding tracers into the model.

+--------------------------+-----------------------+----------------------------------------+
| Script                   | Location              | Functionality                          |
+==========================+=======================+========================================+
| :kbd:`par_my_trc.F90`    | :kbd:`TOP_SRC/MY_TRC` | Claim the number & indexing of tracers |
+--------------------------+-----------------------+----------------------------------------+
| :kbd:`trcsms_my_trc.F90` | :kbd:`TOP_SRC/MY_TRC` | Initialization & parameterization      |
+--------------------------+-----------------------+----------------------------------------+
| :kbd:`trcnxt.F90`        | :kbd:`TOP_SRC/MY_TRC` | Boundary conditions                    |
+--------------------------+-----------------------+----------------------------------------+

An ideal way to edit these files is copying them to :kbd:`MY_SRC` and without changing things in the original folder. 
Here some examples are provided, but the real editing depends on the type of tracer. The OPA Tracer Mannuel explained 
that :kbd:`MY_TRC` is designed for "passive tracers".

The example below sets user defined tracer as :kbd:`.TRUE.` and claimed two tracers with index :kbd:`jpmyt1` and :kbd:`jpmyt2`. So during the simulation,
the first tracer can be indexed as: :kbd:`trn(lon, lat, dep, jpmyt1)`. 

:file:`par_my_trc.F90`::

 ! Line 43
   !!---------------------------------------------------------------------
   !!   'key_my_trc'                     user defined tracers (MY_TRC)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_my_trc     = .TRUE.   !: PTS flag
 ! Line 48
 ! assign an index in trc arrays for each PTS prognostic variables
   INTEGER, PUBLIC, PARAMETER ::   jpmyt1 = jp_lm + 1 !: 1st MY_TRC tracer
   INTEGER, PUBLIC, PARAMETER ::   jpmyt2 = jp_lm + 2 !: 1st MY_TRC tracer

:kbd:`trcsms_my_trc.F90`
------------------------
   
:kbd:`trcsms_my_trc.F90` is an important file to work on. In the example below, :kbd:`kt` is the timestep, :kbd:`trn` is the storage of tracer value and :kbd:`tra`
is the "delta" term. `(this block needs more explainations)` 
 
:file:`trcsms_my_trc.F90`::

 ! Initialization
      IF( kt < 5 ) THEN
         WRITE(*, *) '~~~~ Initialization ~~~~'
         trn(:, :, :, jpmyt1) = your_ini_field(:, :, :)
      ENDIF
 ! Param time derivative
 tra(:, :, 1, jpmyt1) = tra(:, :, 1, jpmyt1) + delta
 ! Print surface tracer values to check
 WRITE(*, *) 'trn@Lena estuary', trn(600, 450, 1, jpmyt1)

The example below is the open boundary condition for our group, and another part of it is to preserve tracer values at the boundary to avoid 
the advection from those "zeros". A typical way is to hold values in the initial condition.

:file:`trcnxt.F90`::

 ! Inside the scope of SUBROUTINE trc_nxt( kt ) 
 ! Set grids below 60N latitude to zero (only simulates Arctic)
       DO jn = 1, jptra
         DO jk = 1, jpk
            WHERE (gphit < 60)
               tra(:, :, jk, jn) = 0.
            END WHERE
         ENDDO
       ENDDO

.. note::

    It is always necessary to read initial/boundary conditions and other physical fields when simulating the tracer, this part of information
    is in `here`_.
    
.. _here: http://ccar-modeling-documentation.readthedocs.io/en/latest/code-notes/TRC/Read_files.html
    

Edit I/O options`
=================

:kbd:`EXP00/iodef.xml` in general contains two parts: (1) the variable definition section and (2) the output section. User-defined tracers should be 
defined in (1), ideally the :kbd:`ptrc_T` group. The information of output frequency is in (2), varies from 1-day to 10-year.

:file:`iodef.xml`::

 <!-- In ptrc_T scope -->
 <group id="ptrc_T" axis_ref="deptht" grid_ref="grid_T">
       <!-- My Stupid Tracer -->
       <field id="T01" description="My tracer 01" unit="none" />
       <field id="T02" description="My tracer 02" unit="none" />
 </group>

The output definition locates at the bottom of the file.
 
.. code-block:: bash
 <!-- In the output definition scope -->
 <file_definition >
      <group id="5d" output_freq="432000" output_level="10" enabled=".TRUE.">
        <file id="5d_ptrc_T" name="auto" description="pisces sms variables" >
           <field ref="T01" /> <!-- output every 5-day -->
           <field ref="T02" />
        </file>
      </group>
      <group id="1m" output_freq="-1"     output_level="10" enabled=".TRUE.">
        <file id="1m_ptrc_T" name="auto" description="pisces sms variables" >
          <field ref="T01" /> <!-- output every month -->
          <field ref="T02" />
        </file>
      </group>
 </file_definition>

Edit :kbd:`namelist_top`
========================

:file:`namelist_top`::

 !                !    name   ! title of the field ! initial data ! initial data ! save   !
 !                !           !                    !  units       ! from file    ! or not ! 
 !                !           !                    !              ! or not       !        !
    sn_tracer(1)  = 'T01'     , 'My tracer 01'     ,  'none'      ,  .false.     ,  .true.
    sn_tracer(2)  = 'T02'     , 'My tracer 02'     ,  'none'      ,  .false.     ,  .true.
 /

The original :kbd:`namelist_top` contains many existing :kbd:`sn_tracer(#)` fields, these are prepared for PISCES and can be deleted.
:kbd:`namelist_top` can set-up the restart tracer files which is useful for long-term simulations  


