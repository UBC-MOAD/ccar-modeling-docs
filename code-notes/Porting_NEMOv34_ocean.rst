.. _CCARModelingNEMO:

*******************
CCAR NEMO Modelling
*******************
 
Porting NEMO v3.4 on Ocean Cluster 
=================

Requirements for NEMO v3+
----------------

* bash installed
* perl installed
* svn installed
* FORTRAN90 compiler installed
* netcdf installed

FORTRAN compiler on ocean cluster is `G95`_.

netCDF library is in the root, you can find them by:

.. code-block:: bash
  
      cd /usr/lib

.. _G95: http://www.g95.org/

Create Working Directories
----------------

Create your working directories for NEMO v3.4

.. code-block:: bash
  
      mkdir -p /ocean/$NAME/GEOTRACER/

Access the Code
----------------

* Register in `NEMO Homepage`_.

* Follow the steps on `NEMO User Guide`_.

* Download NEMO v3.4. The package is "nemo_v3_4" or "dev_v3_4_STABLE_2012".

* One can also refers to a similar page on `Salishsea-MEOPAR`_.

* Windows users can also access the code by through `TortoiseSVN`_ and set a backup on your laptop.

* The structure of directory can be viewed on `NEMO Quick Start Guide`_.

.. _NEMO Homepage: http://www.nemo-ocean.eu/
.. _NEMO User Guide: http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Advanced/Using-Subversion-svn/
.. _Salishsea-MEOPAR: http://salishsea-meopar-docs.readthedocs.org/en/latest/code-notes/dev-notes/nemo-3.4.html/
.. _TortoiseSVN: http://tortoisesvn.net/
.. _NEMO Quick Start Guide: http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Basics/NEMO-Quick-Start-Guide#eztoc1190_1_1

Set Permissions
----------------

.. code-block:: bash
  
      cd /ocean/$NAME
      chmod -R a+x GEOTRACER

Add ARCH file
----------------

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/ARCH # nemo_v3_4 is the name of your NEMO code
      vim arch-ocean.fcm

New ARCH file: :file:`arch-ocean.fcm` could be::

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

Based on the new ARCH file, we add an new configure and build option.

Test Each Components of NEMO
----------------

(coming soon)

**GYRE**

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/CONFIG
      ./makenemo -m ocean -n test_GYRE -r GYRE add_key "key_nosignedzero key_netcdf4"

**LIM2**

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/CONFIG
      /makenemo -m ocean -r ORCA2_LIM -n test_LIM2 add_key "key_nosignedzero key_netcdf4"
      # del_key "key_mpp_mpi" is necessary for NEMO v3.6

**LIM3**

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/CONFIG
      /makenemo -m ocean -r ORCA2_LIM3 -n test_LIM3 add_key "key_nosignedzero key_netcdf4"

**PISCES**

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/CONFIG
      /makenemo -m ocean -r ORCA2_OFF_PISCES -n test_PISCES add_key "key_nosignedzero key_netcdf4"



