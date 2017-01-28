
Adding user-defined tracers
*************************** 

This section describes the way to build-up the tracer scheme in NEMO 3.4 based on the :kbd:`MY_TRC` module.

Edit :kbd:`MY_TRC` files
========================

The following files are useful when adding tracers into the model.

+--------------------------+----------------+----------------------------------+
| Script                   | Location       | Functionality                    |
+==========================+================+==================================+
| :kbd:`par_my_trc.F90`    | :kbd:`TOP_SRC` | Claim the number of tracers      |
+--------------------------+----------------+----------------------------------+
| :kbd:`trcsms_my_trc.F90` | :kbd:`TOP_SRC` | Initialization & parameterization|
+--------------------------+----------------+----------------------------------+
| :kbd:`trcnxt.F90`        | :kbd:`TOP_SRC` | Boundary conditions              |
+--------------------------+----------------+----------------------------------+

An ideal way to edit these files is copying them to :kbd:`MY_SRC` and without changing things in the original scripts. 

Here some examples are provided, but the real editing depends on the modelling goal.

:file:`par_my_trc.F90`::

 ! Line 43
   !!---------------------------------------------------------------------
   !!   'key_my_trc'                     user defined tracers (MY_TRC)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_my_trc     = .TRUE.   !: PTS flag
 ! Line 48
 ! assign an index in trc arrays for each PTS prognostic variables
   INTEGER, PUBLIC, PARAMETER ::   jpmyt1 = jp_lm + 1     !: 1st MY_TRC tracer
   INTEGER, PUBLIC, PARAMETER ::   jpmyt2 = jp_lm + 2     !: 1st MY_TRC tracer
 
The example above set user defined tracer as :kbd:`.TRUE.` and claimed two tracers with index :kbd:`jpmyt1` and :kbd:`jpmyt2`. So during the simulation,
the first tracer is indexed as: :kbd:`trn(lon, lat, dep, jpmyt1)`. :kbd:`MY_TRC` scripts explained that they were designed for "passive tracers".
 
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

:kbd:`trcsms_my_trc.F90` is an important file to work on. In the example above, :kbd:`kt` is the timestep, :kbd:`trn` is the storage of tracer value and :kbd:`tra`
is the "delta" term. (need more explainations) 
 
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

The above example is the open boundary condition for our group, and another part of it is to preserve tracer values at the boundary to avoid 
the advection from those "zeros". A typical way is to hold values in the initial condition.

.. note::

    It is always necessary to read initial/boundary conditions and other physical fields when simulating the tracer, this part of information
    is in `here`_
    
.. _here: http://ccar-modeling-documentation.readthedocs.io/en/latest/code-notes/TRC/Read_files.html
    
Edit I/O options and namelist
=============================

:kbd:`namelist_top` and :kbd:`iodef.xml` should be modified for exporting the simulated tracer values: 

:file:`iodef.xml`::

 <!-- In ptrc_T scope -->
 <group id="ptrc_T" axis_ref="deptht" grid_ref="grid_T">
       <!-- My Stupid Tracer -->
       <field id="T01"       description="My tracer 01"                     unit="none" />
       <field id="T02"       description="My tracer 02"                     unit="none" />
 </group>
 <!------------------------------------>
 <!-- In the output definition scope -->
 <file_definition >
      <group id="5d" output_freq="432000" output_level="10" enabled=".TRUE.">
        <file id="5d_ptrc_T" name="auto" description="pisces sms variables" >
           <field ref="T01"     />
           <field ref="T02"   />
        </file>
      </group>
      <group id="1m" output_freq="-1"     output_level="10" enabled=".TRUE.">
        <file id="1m_ptrc_T" name="auto" description="pisces sms variables" >
          <field ref="T01"      />
          <field ref='T02'    />
        </file>
      </group>
 </file_definition>

In :kbd:`iodef.xml`, tracer values are defined in :kbd:`ptrc_T` group, and the common export frequencies are 5-day and monthly. 

:file:`namelist_top`::

 !                !    name   ! title of the field ! initial data ! initial data ! save   !
 !                !           !                    !  units       ! from file    ! or not ! 
 !                !           !                    !              ! or not       !        !
    sn_tracer(1)  = 'T01'     , 'My tracer 01'     ,  'none'      ,  .false.     ,  .true.
    sn_tracer(2)  = 'T02'     , 'My tracer 02'     ,  'none'      ,  .false.     ,  .true.
 /

The original :kbd:`namelist_top` contains many existing :kbd:`sn_tracer(#)` fields, they were set-up for PISCES and can be deleted.   



