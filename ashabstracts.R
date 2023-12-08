library(rvest)
library(httr)
library(jsonlite) #ended up not using as a json object because i never built the web interface. next year!
library(cleanNLP)
library(reticulate)
library(tidyverse)
library(progress)

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = 20000,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)

use_python("/Users/feinj/mambaforge/bin/python")
cnlp_init_spacy(model="en_core_web_sm")


title_extract <- function(title){
  title_e <- strsplit(title, "\n")[[1]][3]
}

datetime <- function(datetime){
  datetime2 <- list()
  datetime2[[1]] <- paste0(strsplit(datetime, ",")[[1]][2],strsplit(datetime, ",")[[1]][3])
  datetime2[[1]] <- trimws(datetime2[[1]])
  if(grepl("2023:", datetime)==T){
    datetime2[[2]] <- strsplit(datetime, "2023:")[[1]][2]
  } else{
    datetime2[[2]] <- paste0(strsplit(datetime, "[,]")[[1]][4])
  }
  datetime2[[2]] <- trimws(datetime2[[2]])

  date <- as.Date(datetime2[[1]], "%B %d %Y")
  s_time <- strsplit(datetime2[[2]], "-")[[1]][1]
  e_time <- strsplit(datetime2[[2]], "-")[[1]][2]

  return(list("date"=date, "s_time"=s_time, "e_time"=e_time))

}

authors_split <- function(authors){
  authors <- gsub("[^A-Za-z, ]", "", authors)
  authors2 <- strsplit(authors, ",")
  authors2 <- sapply(authors2, trimws)
  authors2 <- authors2[which(!(authors2 %in% c("MD", "M.D.", "PhD", "Ph.D.", "D.O.", "M.S.", "DO", "MS", "BA", "MA", "BS")))]
  authors3 <- cnlp_annotate(authors2, verbose=F)
  authors3 <- authors3$entity %>%
    filter(entity_type=="PERSON")
  return(authors3)
}

session <- function(content){
  s <- gregexpr("Session:", content)[[1]][1]
  e <- gregexpr("Hematology Disease Topics & Pathways", content)[[1]][1]
  return(trimws(str_sub(content, start=s+8, end=e-1)))
}

details <- list()
scrape_abstract_details <- function(url) {
  response <- httr::GET(url)

  # Check for a 404 error or other unsuccessful status
  if (httr::status_code(response) != 200) {
    warning(paste("Failed to access URL:", url, "with status code:", httr::status_code(response)))
    return(NULL)
  }

  page <- read_html(url)
  if(is.na(html_text(html_node(page, "span.number")))){return(NULL)}

  # Extract details from various div classes
  details$session <- session(html_text(html_node(page, ".content")))
  details$abstract_number <- html_text(html_node(page, "span.number"))
  details$date_time <- datetime(html_text(html_node(page, ".datetime.header")))
  details$title <- title_extract(html_text(html_node(page, "h2.subtitle")))
  details$authors <- authors_split(html_text(html_node(page, ".paperauthors")))
  details$abstract <- html_text(html_node(page, ".abstract"))
  details$url <- url
  details$complete_content <- html_text(html_node(page, ".content"))

  return(details)
}
abstractl <- list()

#these numbers are a guess based on what I was able to find randomly sampling the ASH conference website.
#may be missing some.
for(abstract in 17000:190000){
  url <- paste0('https://ash.confex.com/ash/2023/webprogram/Paper', abstract, '.html')
  abstract_details <- scrape_abstract_details(url)
  if(!is.null(abstract_details)) {

    abstractl <- c(abstractl, list(abstract_details))
  }
  Sys.sleep(2)
  n <- n+1
  pb$tick()
}

# Save as a backup
save(abstractl, file="abstracts.RData")



extract_last_name <- function(name) {
  # Assuming last name is the last word in the name string
  last_word <- tail(strsplit(name, " ")[[1]], n = 1)
  return(last_word)
}

# Reordering
abstracts_ordered <- abstractl %>%
  # Create a sorting data frame
  map_dfr(~tibble(
    abstract = list(.x),
    session_name = .x$session,
    start_time = .x$date_time$date,
    first_author_last_name = extract_last_name(.x$authors$entity[1])
  ), .id = "id") %>%
  # Arrange by session, time, and author's last name
  arrange(session_name, start_time, first_author_last_name) %>%
  # Extract the ordered abstracts
  pull(abstract)

escape_latex_chars <- function(text) {
  # The list of special LaTeX characters
  special_chars <- c('%', '$', '&', '#', '_', '{', '}', '^', '~')


  # Escape each special character
  for (char in special_chars) {
    text <- gsub(char, paste0('\\', char), text, fixed = TRUE)
  }
  text <- gsub("\n+", "\\", text, perl = TRUE)
  text <- gsub("\r+", "\\", text, perl = TRUE)
  text <- gsub("\t+", "\\", text, perl = TRUE)

  # Handling backslashes (which are used as escape characters in R and regex)
  # Replace single backslash with double backslashes for LaTeX
  # In R, each backslash must be escaped with another backslash
  # Therefore, to replace one backslash with two, four backslashes are used on the right side of the replacement
  return(text)
}

escape_latex_greek_chars <- function(text) {
  greek_replacements <- c(
    'Γ' = '\\Gamma', 'Δ' = '\\Delta', 'Θ' = '\\Theta', 'Λ' = '\\Lambda',
    'Ξ' = '\\Xi', 'Π' = '\\Pi', 'Σ' = '\\Sigma', 'Φ' = '\\Phi',
    'Ψ' = '\\Psi', 'Ω' = '\\Omega',
    'α' = '\\alpha', 'β' = '\\beta', 'γ' = '\\gamma', 'δ' = '\\delta', 'ε' = '\\epsilon',
    'ζ' = '\\zeta', 'η' = '\\eta', 'θ' = '\\theta', 'ι' = '\\iota', 'κ' = '\\kappa',
    'λ' = '\\lambda', 'μ' = '\\mu', 'ν' = '\\nu', 'ξ' = '\\xi', 'ο' = '\\omicron',
    'π' = '\\pi', 'ρ' = '\\rho', 'σ' = '\\sigma', 'ς' = '\\varsigma', 'τ' = '\\tau',
    'υ' = '\\upsilon', 'φ' = '\\phi', 'χ' = '\\chi', 'ψ' = '\\psi', 'ω' = '\\omega',
    'ϑ' = '\\vartheta', 'ϕ' = '\\varphi', 'ϵ' = '\\varepsilon', 'ϰ' = '\\varkappa',
    'ϱ' = '\\varrho', 'ϖ' = '\\varpi'
  )

  for (char in names(greek_replacements)) {
    replacement <- paste0("$", greek_replacements[[char]], "$")
    text <- gsub(char, replacement, text, fixed = TRUE)

  }

  return(text)
}
process_text_for_latex <- function(text) {
  # Replace newline characters (\n) with LaTeX's newline command
  text <- gsub("\n", " \\par ", text, fixed = TRUE)

  # [Your existing character replacement code here, such as escape_latex_greek_chars]

  return(text)
}
trim_special_chars <- function(text) {
  # Remove \r, \n, \t from the beginning of the string
  text <- gsub("^[\r\n\t]+", "", text)

  # Remove \r, \n, \t from the end of the string
  text <- gsub("[\r\n\t]+$", "", text)

  return(text)
}

latex_content <- ""
for (abstract in abstractl) {
  abstract_text <- process_text_for_latex(trim_special_chars(abstract$abstract))
  abstract_text <- escape_latex_chars(abstract_text)
  abstract_text <- escape_latex_greek_chars(abstract_text)
  abstract_text <- gsub("&", "\\&", abstract_text, fixed = TRUE)

  title_text <- escape_latex_chars(abstract$title)
  title_text <- escape_latex_greek_chars(title_text)

  latex_content <- paste0(latex_content,
                          "\\textcolor{darkred}{\\section*{", abstract$abstract_number,".", title_text, "}}\n",
                          "\\Large{\\textbf{Session:} ", abstract$session, "}\\hfill\n\n",
                          "\\Large{\\textbf{Date:} ", format(abstract$date_time$date, "%A, %B %d, %Y"), " ", abstract$date_time$s_time, "}\n\n",
                          "\\large{\\textbf{Authors:} ", paste(abstract$authors$entity, collapse = ", "), "}\n\n",
                          abstract_text, "\n\n",
                          "\\href{", abstract$url, "}{\\underline{\\textcolor{darkblue}{", abstract$url, "}}}\n",
                          "\\newpage\n")
}

latex_document <- paste0(
  "\\documentclass{article}\n",
  "\\usepackage[utf8]{inputenc}\n",
  "\\usepackage{hyperref}\n",
  "\\usepackage{geometry}\n",
  "\\usepackage{setspace}\n",
  "\\usepackage{xcolor}\n",
  "\\usepackage{graphicx}\n",
  "\\hypersetup{\n",
  "  colorlinks=true,\n",
  "  linkcolor=darkblue,\n",
  "  filecolor=darkblue,\n",
  "  urlcolor=darkblue,\n",
  "  pdftitle={Title},\n",
  "  }\n",
  "\\definecolor{darkred}{RGB}{139, 0, 0}\n",
  "\\definecolor{darkblue}{RGB}{0, 0, 139}\n",
  "\\geometry{a4paper, margin=1in}\n",
  "\\setlength{\\parindent}{0pt}\n",
  "\\setlength{\\parskip}{12pt}\n",
  "\\renewcommand{\\familydefault}{\\sfdefault}\n",
  "\\linespread{1.2}\n",
  "\\begin{document}\n",
  "\\begin{titlepage}\n,",
  "\\centering\n",
  "\\vspace*{\\stretch{1}}\n",
  "\\includegraphics[width=\\textwidth]{dallecover.png}\n",
  "\\vspace*{\\stretch{1}}\n",
  "\\end{titlepage}\n",
  latex_content,
  "\\end{document}"
)


large_string <- latex_document  # Replace with your actual string

# Step 1: Replace already escaped '&' (i.e., '\&') with a placeholder
temp_placeholder <- "TEMP_PLACEHOLDER"  # Make sure this placeholder does not already occur in the string
large_string <- gsub("\\\\&", temp_placeholder, large_string)

# Step 2: Replace unescaped '&' with '\&'
large_string <- gsub("&", "\\\\&", large_string)

# Step 3: Restore the placeholders to '\&'
large_string <- gsub(temp_placeholder, "\\\\&", large_string)

writeLines(large_string, "abstracts1.tex")

## i couldn't get it to work with xelatex for some reason
## so I just ran this in the terminal:
## pdflatex -interaction=nonstopmode abstracts1.tex
## which I could call programmatically here but haven't currently.
