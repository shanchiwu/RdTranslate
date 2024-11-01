#' @import chatgpt
#' @importFrom utils file.edit find help
#'
#' @title Help
#'
#' @param help.txt text from \code{get_help_text}
#' @param language a character for specify language. This will be inserted in the prompt
#' @param prompt prompt for chatGPT. See details below.
#' @param add.prompt Additional prompt for chatGPT. Will be added after \code{prompt}
#' @param roll Roll of chatGPT. See \code{\link{reset_chat_session}}
#' @param api.key OpenAI api key
#'
#' @details
#' For \code{prompt=1}, the prompt is "Translate the following .Rd file text into _target_language_. Ensure no changes are made to any \{ or \} characters
#'
#' For \code{roll=1}, the system roll is "You are a helpful assistant familiar with R and Latex."
#'
#' @examples
#' #Sys.setenv(OPENAI_API_KEY = "Your OpenAI api key")
#' #txt = get_help_text(filter)
#' #Help(txt, language = "Chinese")
#'
#' @export

Help = function(help.txt,
                language,
                prompt=1,
                add.prompt=NULL,
                roll=1,
                api.key=Sys.getenv("OPENAI_API_KEY")){

  Sys.setenv(OPENAI_API_KEY = api.key)

  # Get the text of .Rd file
  message("Generating .Rd file...\n")
  txt = help.txt
  #txt = get_help_text(func_name)
  #cat(txt)

  temp_rd_file <- tempfile(fileext = ".Rd")
  message(paste("Save .Rd file to",temp_rd_file,"\n"))
  writeLines(as.character(txt), sep = "", temp_rd_file)
  cat("\n", file = temp_rd_file, append = TRUE)

  # Transfer character array to one character (paragraph)
  txt_arr <- readLines(temp_rd_file)
  #cat(txt_arr, sep="\n")
  txt_para <- paste(txt_arr, collapse = "\n")
  txt_para = gsub("}}", "} }", txt_para)
  #cat(txt_para)

  # Create final prompt
  if(prompt==1){
    prompt = "Translate the following .Rd file text into _target_language_. Ensure no changes are made to any { or } characters, including preserving sequences of multiple } (like }}). Do not alter any LaTeX commands or formatting commands (such as \\item{}{}, \\code{}, \\emph{}, etc.). Each \\item should retain its original structure and indentation, and nested items should remain within their original scope. Additionally, avoid translating any R code or variable names.\n"
  }
  prompt = gsub('_target_language_', language, prompt)

  final_prompt = paste0(prompt, add.prompt, txt_para)
  #cat(final_prompt)

  # Ask GPT
  if(roll==1){
    roll = "You are a helpful assistant familiar with R and Latex."
  }
  chatgpt::reset_chat_session(system_role = roll)
  translate_text = chatgpt::ask_chatgpt(final_prompt)

  # Write translate .Rd file
  temp_rd_file_trans <<- tempfile(fileext = ".Rd")
  message(paste("Save translate .Rd file to",temp_rd_file_trans,"\n"))
  writeLines(translate_text, sep = "\n",temp_rd_file_trans)
  file.edit(temp_rd_file_trans) # For modification

  # Write to .html and show in help panel
  output_html <- tempfile(fileext = ".html")
  tools::Rd2HTML(temp_rd_file_trans, out = output_html)

  rstudioapi::viewer(output_html) # Show in Viewer panel
  #browseURL(output_html) # Show in browser
  #help("your_custom_documentation", package = "your_package") # Should show in Help panel?
}
