# Keep every resume snapshot this run writes (live ShinySessions write
# them) out of the developer's real user cache.
withr::local_envvar(
  R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = testthat::teardown_env()),
  .local_envir = testthat::teardown_env()
)
snapshot_store_reset()
withr::defer(snapshot_store_reset(), envir = testthat::teardown_env())
