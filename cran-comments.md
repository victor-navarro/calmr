## Comments

### Third submission

>Thanks, we see:
Found the following (possibly) invalid file URI:
     URI: www.victornavarro.org/calmr
       From: README.md
Pls specify the URL completely and start with the protocol, e.g. https://...
Please fix and resubmit.

Fixed to include the protocol. Apologies for the inconvenience.


### Second submission (calmr 0.6.0 -> 0.6.1)

Thank you very much for the feedback and my apologies for the oversights. See my comments below.

>If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in
quotes: "Title")

There are no references to be included in the description at the moment.

>Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
-> CalmrDesign-methods.Rd: \value
      CalmrFit-methods.Rd: \value
      CalmrResult-methods.Rd: \value

I have revised the documentation for these documents and added the return values as requested. I have also revised the rest of the documentation and made it consistent throughout.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.