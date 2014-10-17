## FAQ & Notes for NEMO bugs

### Building failed on limrhg.F90

You may see the message like:

.. code-block:: bash
  
      make: *** No rule to make target `limrhg.o', needed by `limdyn_2.o'.  Stop.
      
When you are building a ORCA2_LIM, ORCA2_LIM3 or ORCA2_LIM_PISCES. 
This is because limrhg.F90 in LIM3 cannot link successfully to LIM2.

**Quirks**

Link limrhg.F90 manually:

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/NEMO/LIM_SRC_2
      rm -rf limrhg.F90 # delete the file
      ln -s ../LIM_SRC_3/limrhg.F90 # link to limrhg.F90 in LIM_SRC_3
	  
You can also directly copy and replace the limrhg.F90 script on NEMO/LIM_SRC2:

.. code-block:: bash
  
      cd /ocean/$NAME/GEOTRACER/nemo_v3_4/NEMOGCM/NEMO/LIM_SRC_2
      cp -rf ../LIM_SRC_3/limrhg.F90 limrhg.F90 # copy in force
 

### FORTRAN runtime error for line 214 on nemogcm.f90

You may see the message like:

.. code-block:: bash
  
      fortran runtime error, line 214 on nemogcm.f90

The line 214 on nemogcm.f90 is:

.. code-block:: fortran
  
      READ(numnam, namctl)
	  
When you are running nearly ORCA2_LIM, ORCA2_LIM3 or ORCA2_LIM_PISCES.
This is because namelist, namelist_lim, namelist_lim3 in the 
configuration directory do not link successfully to the file source in NEMOGCM/NEMO/..
there are something like "link ../namelist" not the real namelist.

**Quirks**

Copy and replace the namelist, namelist_lim, namelist_lim3.


	  

