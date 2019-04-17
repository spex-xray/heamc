# =========================
# CMakeLists.txt file for
# HEAMC project
# =========================
cmake_minimum_required (VERSION 2.6)
project (heamc)
enable_language(Fortran)

set(ROUTINES mod_heamc.f90 spexlines.f90 spexpower.f90
             spexpion.f90  
             out_csd.f90   out_lines.f90 out_power.f90
	     out_level.f90 nei_cont.f90  plas.f90 
	     pi_csd.f90    pi_lines.f90  pi_abs.f90 
	     pi_hc.f90 )
	     
add_executable(heamc heamc.f90 ${ROUTINES})	     
