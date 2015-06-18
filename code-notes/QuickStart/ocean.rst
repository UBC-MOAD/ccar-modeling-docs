
:kbd:`ocean` 
===================

This section describes the steps to set up and test NEMO v3.4 on :kbd:`ocean` .

Create a Workspace
------------------

NEMO v3.4 requires

* bash installed
* perl installed
* svn installed
* FORTRAN90 compiler installed
* netcdf installed

FORTRAN compiler on ocean cluster is `G95`_.

The directory of netCDF library in :kbd:`ocean`  is:

.. code-block:: bash
  
      cd /usr/lib

.. _G95: http://www.g95.org/

Create a space for NEMO v3.4 code and file I/O

.. code-block:: bash
  
      mkdir -p /ocean/$NAME/GEOTRACES/

Access to NEMO v3.4 code 
------------------------

* Register in `NEMO Homepage`_.

* Follow the steps on `NEMO User Guide`_.

* Download NEMO v3.4. The package is "nemo_v3_4" or "dev_v3_4_STABLE_2012".

* One can also refers to a similar page on `Salishsea-MEOPAR`_.

* Windows users can get the code by using `TortoiseSVN`_ for a backup.

* The tree structure of directory can be viewed on `NEMO Quick Start Guide`_.

.. _NEMO Homepage: http://www.nemo-ocean.eu/
.. _NEMO User Guide: http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Advanced/Using-Subversion-svn/
.. _Salishsea-MEOPAR: http://salishsea-meopar-docs.readthedocs.org/en/latest/code-notes/dev-notes/nemo-3.4.html/
.. _TortoiseSVN: http://tortoisesvn.net/
.. _NEMO Quick Start Guide: http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Basics/NEMO-Quick-Start-Guide#eztoc1190_1_1

Change the permission of code directory
-
.. code-block:: bash
  
      cd /ocean/$NAME
      chmod -R a+x GEOTRACES

Compile the code
----------------

The arch file for NEMO

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACES/$CODEDIR/NEMOGCM/ARCH
      vim arch-ocean.fcm

:file:`arch-ocean.fcm`

  # generic gfortran compiler options for linux
  # NCDF_INC    netcdf include file
  # NCDF_LIB    netcdf library
  # FC          Fortran compiler command
  # FCFLAGS     Fortran compiler flags
  # FFLAGS      Fortran 77 compiler flags
  # LD          linker
  # LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
  # FPPFLAGS    pre-processing flags
  # AR          assembler
  # ARFLAGS     assembler flags
  # MK          make
  # USER_INC    additional include files for the compiler,  e.g. -I<include dir>
  # USER_LIB    additional libraries to pass to the linker, e.g. -l<library>
  # ============================================================================
  %NCDF_INC            -I/usr/include
  %NCDF_LIB            -L/usr/lib -lnetcdff
  %FC                  gfortran
  %FCFLAGS             -fdefault-real-8 -O3 -funroll-all-loops -fcray-pointer
  %FFLAGS              %FCFLAGS
  %LD                  gfortran
  %LDFLAGS
  %FPPFLAGS            -P -C -traditional
  %AR                  ar
  %ARFLAGS             -rs
  %MK                  make
  %USER_INC            %NCDF_INC
  %USER_LIB            %NCDF_LIB


 Testing a widely applied set-up in our team:
 
.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACES/$CODEDIR/NEMOGCM/CONFIG
      ./makenemo -m ocean -r ORCA2_OFF_PISCES -n case_name add_key "key_nosignedzero key_netcdf4"
      
Run the model
--------------

Download forcing files from `NEMO Homepage`_ and place all the files in:

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACES/$CODEDIR/NEMOGCM/CONFIG/$case_name/EXP00
      mv $forcing_file .

.. _NEMO Homepage: http://www.nemo-ocean.eu/

After changing NEMO's output in :kbd:`ocean` and other options in different namelists. We can run the model

.. code-block:: bash
  
      ./opa &
      
The export information is saved in `ocean.output`.
      
