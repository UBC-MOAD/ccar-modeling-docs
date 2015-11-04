
Calculate emps with ANHA4 Output 
********************************** 

This section describes the steps to calculate evaporation minus precipitation (emps) in :kbd:`MY_TRC` offline simulation with ANHA4 forcing field in NEMO 3.4. The approaches were tested under ANHA4-EXH001 with surface ocean salt flux and sea surface salinity.   

The equation for emps is:

.. math::
    
 \mathrm{emp(:, :) = -\frac{iocesafl(:, :)*soce}{rday*(isssalin(:, :)+1.0E^{-16})}}

* **iocesafl** is the salt flux at ocean surface, defined in icemod files. 
* **isssalin** is Sea surface salinity, defined in icemod files.
* **soce** is 34.7, **rday** is 3600*4, they are constants. 

Use :kbd:`namelist/&namdta_dyn`
===============================

A simple way to read **iocesafl** and **isssalin** in the model is using the file channels in :kbd:`namelist/&namdta_dyn`. sn_emp can be used, and since ANHA4-EXH001 does not have "key_eiv", here we also choose sn_eiw.  

In :kbd:`namelist`, add the following

.. code-block:: bash

 !-----------------------------------------------------------------------
 &namdta_dyn        !   offline dynamics read in files                ("key_offline")
 !-----------------------------------------------------------------------
 !            !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation !
 !            !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  !
     sn_emp  = 'ANHA4-EXH001_icemod',    120    , 'iocesafl',    .true.    , .false.,   'fday'  , ''       , ''
     sn_eiw  = 'ANHA4-EXH001_icemod',    120    , 'isssalin',    .true.    , .false.,   'fday'  , ''       , ''
  /

Then copy :kbd:`datdyn.F90` from :kbd:`OFF_SRC` and on row 295, add the following:

.. code-block:: fortran

 emp(:, :) = -1.0*emp(:, :)*34.7/(3600.0*4*(aeiw(:, :)+1.0e-16))
 emps(:,:) = emp(:,:)
