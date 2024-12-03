#' @import chatgpt
#' @importFrom utils file.edit find help
#'
#' @title HelpGPT
#'
#' @description
#' Utilized OpenAI’s API to translate .Rd documentation files into any language.
#'
#' @param func_name a \code{\link{name}} or \code{\link{character}} string specifying the function for which help is sought.
#' @param package a name or character vector giving the packages to look into for documentation, or \code{NULL}.
#' @param language a character for specify language. This will be inserted in the pre-specified prompt.
#' @param prompt Pre-specified prompt for chatGPT. See details below. User can also use their own prompt character.
#' @param add.prompt Additional prompt for chatGPT. Will be added after \code{prompt}.
#' @param roll Roll of chatGPT. See \code{\link{reset_chat_session}} in package \code{\link{chatgpt}}.
#' @param api.key a character, your OpenAI api key.
#' @param model ID of the model to use. Suggest to use \code{"gpt-4o"}. See https://platform.openai.com/docs/models.
#'
#' @details
#' For \code{prompt=1}, the prompt is:
#'
#' "Translate the following .Rd file text into formal, polished <TargetLanguage>, making sure to maintain a professional tone. Do not add any comment. Ensure no changes are made to any \{ or \} characters, including preserving sequences of multiple \} (like \}\}). Do not alter any LaTeX commands or formatting commands (such as \verb{\\item{}{}}, \verb{\\code{}}, \verb{\\emph{}}, etc.). Each \verb{\\item} should retain its original structure and indentation, and nested items should remain within their original scope. Additionally, avoid translating any R code or variable names."
#'
#' For \code{roll=1}, the system roll is:
#'
#' "You are a helpful assistant familiar with R and Latex."
#'
#' @examples
#'
#' \dontrun{
#' Sys.setenv(OPENAI_API_KEY = "Your OpenAI api key")
#' HelpGPT(filter, stats, language = "Spanish")
#' HelpGPT("sapply", prompt = "Your own prompt")
#' }
#'
#' @export

HelpGPT = function(func_name,
                   package = NULL,
                   language = NULL,
                   prompt = 1,
                   add.prompt = NULL,
                   roll = 1,
                   api.key = Sys.getenv("OPENAI_API_KEY"),
                   model = c("gpt-4o", "gpt-4o-mini", "gpt-3.5-turbo")){

  if(is.numeric(prompt) && is.null(language)){
    stop("When using the default prompt, you must specify a language to translate into.")
  }

  Sys.setenv(OPENAI_API_KEY = api.key,
             OPENAI_MODEL = model[1])

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
  message("Generating .Rd file...\n")
  rd_content <- rd_files[[rd_name]]
  rd_content_with_escape <- gsub("%", "\\\\%", as.character(rd_content))

  # Get the text of .Rd file
  temp_rd_file <- tempfile(fileext = ".Rd")
  message(paste("Save .Rd file to", temp_rd_file,"\n"))
  writeLines(as.character(rd_content_with_escape), sep = "", temp_rd_file)
  cat("\n", file = temp_rd_file, append = TRUE)

  # Transfer character array to one character (paragraph)
  txt_arr <- readLines(temp_rd_file)
  #cat(txt_arr, sep="\n")
  txt_para <- paste(txt_arr, collapse = "\n")
  txt_para = gsub("}}", "} }", txt_para)
  #cat(txt_para)

  # Create final prompt
  if(prompt==1){
    prompt = "Translate the following .Rd file text into formal, polished <TargetLanguage>, making sure to maintain a professional tone. Do not add any comment. Ensure no changes are made to any { or } characters, including preserving sequences of multiple } (like }}). Do not alter any LaTeX commands or formatting commands (such as \\item{}{}, \\code{}, \\emph{}, etc.). Each \\item should retain its original structure and indentation, and nested items should remain within their original scope. Additionally, avoid translating any R code or variable names.\n"
  }
  prompt = gsub('<TargetLanguage>', language, prompt)

  final_prompt = paste0(prompt, add.prompt, txt_para)
  #cat(final_prompt)

  # Ask GPT
  if(roll==1){
    roll = "You are a helpful assistant familiar with R and Latex."
  }
  chatgpt::reset_chat_session(system_role = roll)
  translate_text <- chatgpt::ask_chatgpt(final_prompt)

  # Write translate .Rd file
  temp_rd_file_trans <- tempfile(fileext = ".Rd")
  message(paste("Save translate .Rd file to", temp_rd_file_trans, "\n"))
  writeLines(translate_text, sep = "\n", temp_rd_file_trans)
  file.edit(temp_rd_file_trans) # For modification

  # Write to .html and show in help panel
  output_html <- tempfile(fileext = ".html")
  tools::Rd2HTML(temp_rd_file_trans, out = output_html)

  rstudioapi::viewer(output_html) # Show in Viewer panel
  #browseURL(output_html) # Show in browser
  #help("your_custom_documentation", package = "your_package") # Should show in Help panel?
}
