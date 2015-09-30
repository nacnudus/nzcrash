# nzcrash 0.0.1

* The following vehicle codes are now parsed and no longer appear in `vehicles$vehicle` as `NA`:

    ```
    E = pedestrian
    K = skateboard, in-line skater etc
    Q = equestrian
    H = wheeled pedestrian (wheelchairs etc)
    O = other or unknown.
    ```

* `inst/extdata/raw2clean.R` has been fixed to work with the latest version of
  `purrr`.
* Stray pdf files have been removed from the R directory.
* Some typos have been corrected in the vignette and README.
