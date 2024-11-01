#' @title Get the help text
#'
#' @description
#' Similar to \code{help()}, but this function will obtain the text of .Rd file
#'
#' @param func_name a \code{\link{name}} or \code{\link{character}} string specifying the function for which help is sought.
#' @param package a name or character vector giving the packages to look into for documentation, or \code{NULL}
#'
#' @return The plantext of the .Rd file
#'
#' @examples
#' cat(get_help_text(sapply))          # Without quotes
#' cat(get_help_text("sapply"))        # Can use as a string
#' cat(get_help_text(sapply, base))   # Specify package using name
#' cat(get_help_text("sapply", "base"))  # Use string to specify package
#' #cat(get_help_text(sapply, stats))   # Specify package using name, but wrong package
#' #cat(get_help_text("sapply", "stats"))  # Use string to specify package, but wrong package
#' @export
get_help_text <- function(func_name, package = NULL) {
  # Check if func_name is a name; if so, convert to string
  func_name <- as.character(substitute(func_name))

  # If package is provided, check if it's a name and convert to string
  if (!missing(package)) {
    package <- as.character(substitute(package))
  } else {
    # If package is not provided, automatically find the package
    func_obj <- tryCatch(get(func_name), error = function(e) NULL)
    if (is.null(func_obj)) {
      stop("Function not found: ", func_name)
    }
    package <- sub("^package:", "", find(func_name)[1])
  }

  # Get the help file path for the function
  help_path <- as.character(help((func_name), (package)))

  # Check if help_path is valid
  if (is.null(help_path) || length(help_path) == 0) {
    stop("Help file not found. Check the function name and package name: ", func_name, " in package: ", package)
  }

  # Extract the Rd file name from the path
  rd_name <- sub(".*/(.*)$", "\\1.Rd", help_path)

  # Check if rd_name exists in the Rd files of the package
  rd_files <- tools::Rd_db(package)
  if (!rd_name %in% names(rd_files)) {
    stop("Corresponding .Rd file not found for ", func_name, " in package ", package)
  }

  # Retrieve the content of the Rd file and return it as text
  rd_content <- rd_files[[rd_name]]
  rd_content_with_escape <- gsub("%", "\\\\%", as.character(rd_content))

  return(rd_content_with_escape)
}

