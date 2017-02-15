
Calculate emps with ANHA4 Output 
********************************** 

This section describes the steps to calculate evaporation minus precipitation (emps) term in :kbd:`MY_TRC` with ANHA4 forcing fields.
This approache was tested with forcing files from ANHA4-EXH001 and EXH005.

 The equation for emps is [Schmitt, 1989]:

.. math::
    
 \mathrm{emps(:, :) = -\frac{iocesafl(:, :)*soce}{rday*(isssalin(:, :)+1.0E^{-16})}}

* :kbd:`iocesafl` is the salt flux at ocean surface, defined in icemod files. 
* :kbd:`isssalin` is Sea surface salinity, defined in icemod files.
* :kbd:`soce` is 34.7, :kbd:`rday` is 3600*24, they are constants. 

In NEMO 3.4, the "p" part of emps indlues both precipitation and sea-ice melt but does **not** take runoff into account.

In Kyle's research, emps was calculated for

* estimating the dilution of tracers (combined with runoff). 
* the parameterization of isotopic fractionation. By definition, net precipitation minus emps is the sea-ice melt water.

Use :kbd:`namelist/&namdta_dyn`
===============================

A simple way to read :kbd:`iocesafl` and :kbd:`isssalin` in the model is using the file channels in :kbd:`namelist/&namdta_dyn`. 
sn_emp can be used, and since ANHA4-EXH001/EXH005 does not have "key_eiv", here we also choose sn_eiw.  

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

 emp(:, :) = -1.0*emp(:, :)*34.7/(3600.0*24*(aeiw(:, :)+1.0e-16))
 emps(:,:) = emp(:,:)
 
 
**Reference**

 - R. W. Schmitt, P. S. Bogden, and C. E. Dorman. Evaporation minus precipitation and density fluxes for the North Atlantic. J. Phys. Oceanogr., 19(9):1208–1221, 1989.
