

# For a POSIXct vector (indicating time and date), get the time of day within
# the given date, in units of hours since midnight.
getTimeOfDay <- function(t) difftime(t, as.POSIXct(trunc(t, "days")), units = "hours")


# Cache the Result of `f` on Disk
#
# This function wraps a function `f`, producing a new function that is cached.
# The cached function will write its result to a file
#   "`cache.path`/cached_<hash>.rds".
# When it is called again, it tries to load its result from this file.
#
# The `<hash>` part is determined by the arguments of the function, so if the
# function is called with different arguments, a different file is created /
# read.
#
# The function arguments of the cached version of `f` need to be *named* for
# every call. While it is possible to avoid this requirement, I wanted to keep
# this function as simple and understandable as possible. You should be able to
# read and understand this function!
#
# This function tries to handle RNG as well as possible:
# - if `f` does not use RNG, caching always works
# - if `f` does use RNG, then cache is only used when
#   1. the .Random.seed was set beforehand
#   2. the .Random.seed is the same every time the function is called
#   This means you have to `set.seed()` at some point before calling a cached
#   function, but it saves you from most bad surprises regarding
#   reproducibility.
#
# If you did not care about handling RNG properly, you could consider using the
# "memoise" package.
#
# Arguments:
# - f (`function`): The function to wrap
# - cache.path (`character(1)`): The directory in which to store hashed files.
#   This directory is created if it does not exist.
# Return value: The return value of f, possibly cached.
#
# Examples:
# estimatePi <- function(n) {
#   mean(colSums(matrix(runif(n)^2, nrow = 2)) < 1) * 4
# }
# estimatePiCached <- cacheFunktionOnDisk(estimatePi, tempdir(), "estimatePi")
#
# set.seed(1)
# estimatePi(1e8)  # takes a few seconds
# set.seed(1)
# estimatePiCached(1e8)  # takes a few seconds
# set.seed(1)
# estimatePiCached(1e8)  # returns instantly
#
# General note: The code below "licensed" "CC0", you can use it in your own
# projects in any form you like, although I cannot guarantee that it is free
# from errors.
# <https://creativecommons.org/publicdomain/zero/1.0/>
cacheFunctionOnDisk <- function(f, cache.path) {
  assertFunction(f)
  assertString(cache.path)
  dir.create(cache.path, showWarnings = FALSE, recursive = TRUE)

  function(...) {

    args <- list(...)
    assertList(args, names = "unique")

    # we sort the names in the argument list; this way,
    # f(a = 1, b = 2) and f(b = 2, a = 1) are interpreted as the same call.
    args <- args[sort(names(args))]
    # The way to do this without requiring passing named arguments would be to
    # use `match.call()`.

    # create a unique hash of the *content* of the arguments.
    # We only keep the first 16 characters, that should be enough to avoid
    # collisions. (If we had 1e7 different arguments, the probability to have at
    # least one collision would be ~ (1 - exp(-1e7 ^ 2 / (2 * 16^16))) ~ 3e-6
    hash <- substr(digest::digest(args, "blake3"), 1, 16)
    cachefile <- file.path(cache.path, sprintf("cached_%s.rds", hash))

    seedstate.before <- get0(".Random.seed")
    if (file.exists(cachefile)) {
      tryCatch({
        # carefully open the file, if it exists. If the file is damaged, this
        # fails, and we overwrite it later.
        cached <- readRDS(cachefile)
        if (cached$used.rng) {
          if (!identical(seedstate.before, cached$seedstate.before)) {
            warning("cacheFunctionOnDisk cache hit but RNG state was different -> not using cache.")
            stop()
          }
          if (!is.null(cached$seedstate.after)) {
            set.seed(1)
            .Random.seed <<- cached$seedstate.after
          }
        }
        return(cached$result)
      }, error = function(e) NULL)
    }
    result <- do.call(f, args, quote = TRUE, envir = parent.frame())
    cachefile.tmp <- paste0(cachefile, ".tmp")

    seedstate.after <- get0(".Random.seed")

    used.rng <- !identical(seedstate.after, seedstate.before)
    # refuse to save if function used RNG but had no seed set before.
    if (is.null(seedstate.after) != is.null(seedstate.before)) {
      warning("cacheFunctionOnDisk refusing to cache function that used RNG but had no seed set before.")
    } else {
      # if saving fails, we want to clean up after ourselves
      on.exit(suppressWarnings(file.remove(cachefile.tmp)))
      # by writing to a temporary file first and then moving that to the actual
      # destination file, we avoid keeping "broken" files in case saveRDS()
      # crashes for some reason, or if two R-processes write something at the
      # same time.

      saveRDS(list(
          result = result,
          used.rng = used.rng,
          seedstate.before = seedstate.before,
          seedstate.after = seedstate.after
        ), cachefile.tmp)
      file.rename(cachefile.tmp, cachefile)
      if (used.rng) {
        # if seedstate.after is NULL we should not be here
        stopifnot(!is.null(seedstate.after))
        # reset RNG state that is not captured by `.Random.seed`
        set.seed(1)
        .Random.seed <<- seedstate.after
      }
    }

    result
  }
}
