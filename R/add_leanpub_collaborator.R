
#' Add Leanpub as Collaborator
#'
#' @param course_dir directory with course materials or a `course_check`
#' object
#' @param auth_token Provide a personal access token (PAT) from
#'   <https://github.com/settings/tokens>. If `NULL`, will use the logic
#'   described in \code{gh::gh_whoami()} to look for a token stored in an environment
#'   variable. Use [usethis::browse_github_pat()] to help set up your PAT.
#'
#' @return Answer from the API as a `gh_response` object,
#' which is also a `list.` Failed requests will generate an `R` error.
#'
#' @export
#' @md
add_leanpub_collaborator = function(
  course_dir = ".",
  auth_token = NULL) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    stop("gh package required for add_leanpub_collaborator")
  }
  if (inherits(course_dir, "course_check")) {
    course_dir = course_dir$course_dir
  }
  if (inherits(course_dir, "structure_check")) {
    course_dir = course_dir$course_dir
  }

  repo = git2r::repository(course_dir)
  remote = git2r::remote_url(repo, remote = "origin")
  url_remote = remote[1]
  owner = basename(dirname(url_remote))
  remote = basename(url_remote)
  repo = sub("[.]git$", "", remote)

  null_replace = function(a, b) {
    if (!is.null(a)) a else b
  }
  gh_token = function() {
    token <- Sys.getenv("GITHUB_PAT", "")
    if (token == "")
      Sys.getenv("GITHUB_TOKEN", "")
    else token
  }
  auth_token <- null_replace(auth_token, gh_token())

  res = add_gh_collaborator(owner = owner,
                            repo = repo,
                            collaborator = "leanpub",
                            auth_token = auth_token)
  res
}
