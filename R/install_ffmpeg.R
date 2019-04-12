
#' Install \code{ffmpeg} from a command line installer
#'
#' @param force Should \code{ffmpeg} be installed
#' if found in the path already?
#' @note The default installer for Linux is \code{apt-get},
#' which may not be appropriate for your distribution and is not run
#' using \code{sudo}
#'
#' @return Either \code{NA} if not found after install or the path
#' to the installation.
#' @export
#'
#' @examples
#' install_ffmpeg()
install_ffmpeg = function(force = FALSE) {

  ffmpeg = c(Sys.getenv("ffmpeg"),
             Sys.which("ffmpeg"))
  ffmpeg = ffmpeg[ !(ffmpeg %in% "") ]
  ffmpeg = ffmpeg[1]

  if (is.na(ffmpeg) || force) {
    os = sys_type()
    cmd = switch(
      os,
      macos = "brew",
      linux = "apt-get",
      windows = "choco")
    if (is.null(cmd)) {
      stop("OS system not determined - please install at command line")
    }
    cmd = paste(cmd, "install -y ffmpeg")
    res = system(cmd)
    if (res != 0) {
      warning("Non-zero exit status in ffmpeg installation")
    }
  }
  ffmpeg = c(Sys.getenv("ffmpeg"),
             Sys.which("ffmpeg"))
  ffmpeg = ffmpeg[ !(ffmpeg %in% "") ]
  ffmpeg = ffmpeg[1]
  return(ffmpeg)
}
