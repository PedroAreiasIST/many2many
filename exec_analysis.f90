  INCLUDE 'mkl_pardiso.f90'
  PROGRAM simplasnew
    USE  MKL_PARDISO
    CALL ssimplas(.TRUE.)
    
  END PROGRAM simplasnew
