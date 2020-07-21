# mgsub 1.7.2
* There was a warning surfacing in newer versions of R around how certain functions were linked to in documentation. While not technically a bug, our CICD pipeline is setup to report those as errors. To ensure full compliance with CRAN Standards, the linking was updated.
* Very minor code refactor to apply linting standards to enable easier contribution, removed some superfulous lines.

# mgsub 1.7.1

* Added RNG Version control to the tests to support future versions of R. There are no functional changes to the way the package runs, this is only to ensure the testing environment continues to work as expected.

# mgsub 1.7

* Added the mgsub_censor function which enables fast, simultaneous censoring of patterns
* Trimmed unncessary logic and helper functions

# mgsub 1.6

* New method employed leveraging gregexpr to reduce the amount of computation time spend identifying matches which slightly improves performance.
* Added 'escapes' from the normal processing if there's only one match to replace to further improve performance.

# mgsub 1.5

* Removed NSE evalutaion, deprecating the named list (dictionary) method
* Added support for vector inputs
* Added NA handling
* Slightly improved vectorized performance
* Updated license (MIT)

# mgsub 1.0.1

* Added checks for named input to mgsub
* Added mgsub_alt to support alternative function parameterization (name is temporary)

# mgsub 1.0

* Initial release to CRAN
