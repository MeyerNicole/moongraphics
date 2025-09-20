.onAttach <- function(libname, pkgname) {

  version <- utils::packageDescription(pkgname)$Version

  cli::cli_alert_success("{pkgname} {version} attached successfully. \n Dev: MeyerNicole @ github")

}
