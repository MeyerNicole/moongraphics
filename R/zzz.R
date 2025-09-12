.onAttach <- function(libname, pkgname) {

  package_version <- "1.0.0"

  packageStartupMessage(
    "── Attaching moongraphics ─────────────────────────────────────────────────────────── ", "\n",
    paste("✔", "moongraphics",package_version), "\n",
    "   by Nicole Meyer"
  )

}
