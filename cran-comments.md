## CRAN comments — IFFuzzyStats 0.1.0

### Test environments

* Local: Windows 11 x64, R 4.5.3 — `R CMD check --as-cran`: 0 errors,
  0 warnings, 2 NOTEs (see below).
* `R CMD check` (without `--as-cran`): 0 errors, 0 warnings, 0 NOTEs.

### NOTEs

* "New submission" — expected: this is the first CRAN release of the
  package (name `IFFuzzyStats` was chosen after confirming no
  case-insensitive conflict on CRAN).
* "unable to verify current time" / "checking for future file
  timestamps ... NOTE" — environment artifact of the offline build
  machine (no network time available to the checker); no file in the
  tarball carries a future timestamp (`R CMD check` without
  `--as-cran` reports no timestamp NOTE). Nothing to fix package-side.

### Re-submission notes

* None yet — initial submission.
