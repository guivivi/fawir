.onLoad <- function(libname, pkgname) {
  # Restrict threading globally
  Sys.setenv(OMP_NUM_THREADS = 1)
  Sys.setenv(MKL_NUM_THREADS = 1)
  Sys.setenv(OPENBLAS_NUM_THREADS = 1)
  Sys.setenv(R_INSTALL_NCPUS = 1)
  
  # Set RcppParallel to single-threaded
  RcppParallel::setThreadOptions(numThreads = 1)
}
