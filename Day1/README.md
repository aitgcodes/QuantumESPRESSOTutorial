Switch into the Codes directory
  cd Codes
Compile each of the files linking to lapack and blas libraries
  gfortran pwell.f90 dysev.f -o pwell.x -llapack -lblas
  gfortran helium_hf_radial.f90 -o helium_hf_radial.x -llapack -lblas
  gfortran h2_hf_gauss.f90 -o h2_hf_gauss.x -llapack -lblas
