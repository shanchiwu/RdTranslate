# RdTranslate: Automatically Translate R Documentation Using ChatGPT

## Overview:

`RdTranslate` is an R package designed to streamline the translation of R documentation files (`.Rd`) into various languages, leveraging the power of ChatGPT's natural language processing capabilities. This tool is ideal for developers and educators aiming to make their R packages more accessible to non-English-speaking audiences, thereby broadening the reach and usability of their work.

* Advantages:
    * __Time-Saving__: Eliminates the need for manual translation.
    * __High Quality__: Delivers translations with contextual understanding, reducing ambiguity and errors.
    * __Cost-Effective__: Provides a budget-friendly alternative to professional translation services.

* Limitations:
    * __Translation Context__: While ChatGPT is powerful, nuanced technical terms may require manual review.
    * __Internet Dependency__: Requires an internet connection to access ChatGPT's API.
    
    
## Installation:
The package can be downloaded from github: 

```r
devtools::install_github("shanchiwu/RdTranslate")
```

Load the package:

```r
library(RdTranslate)
```

Example:

```r
Sys.setenv(OPENAI_API_KEY = "Your OpenAI api key")

HelpGPT(filter, stats, language = "Spanish")HelpGPT("sapply", prompt = "Your own prompt")
```
