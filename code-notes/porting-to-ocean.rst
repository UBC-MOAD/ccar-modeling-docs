.. _CCARModelingNEMO:

*******************
CCAR NEMO Modelling
*******************
 
Notes on Porting NEMO v3.4 on Ocean Cluster 
=================

Requirements for NEMO v3+
----------------

* bash installed
* perl installed
* svn installed
* FORTRAN90 compiler installed
* netcdf installed

FORTRAN compiler on ocean cluster is G95[http://www.g95.org/downloads.shtml]. 
netCDF library is in the root, you can find them by:
  .. code-block:: bash
      cd /usr/lib


Create Working Directories
----------------

Create your working directories for NEMOv3.4

  .. code-block:: bash
      mkdir -p /ocean/$NAME/GEOTRACER/

Access the Code
----------------

* Register in NEMO's website[http://www.nemo-ocean.eu/].
* Follow the steps on NEMO's website[http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Advanced/Using-Subversion-svn].
* Download NEMO v3.4. the name could be "nemo_v3_4" or "dev_v3_4_STABLE_2012"
* One can also refers to a similar page on Salishsea teams[http://salishsea-meopar-docs.readthedocs.org/en/latest/code-notes/dev-notes/nemo-3.4.html].
* Windows users can also downloading the code through TortoiseSVN to make a backup on your laptop.
* The structure of directory can be viewed here(you need to login)[http://www.nemo-ocean.eu/Using-NEMO/User-Guides/Basics/NEMO-Quick-Start-Guide#eztoc1190_1_1]

Set Permissions
----------------

  .. code-block:: bash
      cd /ocean/$NAME
      chmod -R a+x GEOTRACER

Add ARCH file
----------------

  .. code-block:: bash
      cd /ocean/yingkai/GEOTRACER/nemo_v3_4/NEMOGCM/ARCH # nemo_v3_4 is the name of your NEMO code
      vim arch-ocean.fcm

The new ARCH :file:`arch-ocean.fcm` could be:

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

  %NCDF_INC            **-I/usr/include**
  %NCDF_LIB            **-L/usr/lib -lnetcdff**
  %FC                  gfortran
  %FCFLAGS             -fdefault-real-8 -O3 -funroll-all-loops -fcray-pointer
  %FFLAGS              %FCFLAGS
  %LD                  gfortran
  %LDFLAGS
  %FPPFLAGS            -P -C -traditional
  %AR                  ar
  %ARFLAGS             -rs
  %MK                  **make**
  %USER_INC            %NCDF_INC
  %USER_LIB            %NCDF_LIB

Test Each Components of NEMO
----------------
(coming soon)
**GYRE**

**LIM2**

**LIM3**

**PISCES**


FAQ & Notes for NEMO bugs
=================

**Building failed on limrhg.F90**

You may see the message like:
  .. code-block:: bash
      make: *** No rule to make target `limrhg.o', needed by `limdyn_2.o'.  Stop.
      
When you are building a ORCA2_LIM, ORCA2_LIM3 or ORCA2_LIM_PISCES. 
This is because limrhg.F90 in LIM3 cannot link successfully to LIM2.

Quirks

Link limrhg.F90 manually:
  .. code-block:: bash
      cd /ocean/yingkai/GEOTRACER/nemo_v3_4/NEMOGCM/NEMO/LIM_SRC_2
      rm -rf limrhg.F90 # delete the file
      ln -s ../LIM_SRC_3/limrhg.F90 # link to limrhg.F90 in LIM_SRC_3
      

External Link
=================
(coming soon)




