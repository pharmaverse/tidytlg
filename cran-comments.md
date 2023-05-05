## tidytlg 0.1.0 Resubmission

### Resubmission Actions:

Per a Review by Victoria Wimmer dated May 3, 2023:

- Package DESCRIPTION was updated

### Testing

Tested on Ubuntu Jammy, GitHub Action, and RHub.

Two notes found:

Possibly misspelled words in DESCRIPTION: TLG (43:52) TLGs (2:15)
Tidyverse (2:30, 43:63)

Found the following (possibly) invalid URLs: URL:
<https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf>
From: man/tidytlg.Rd Status: 403 Message: Forbidden

TLG is an industry specific word refering to tables, listings, and
figure(s).

The link returning a 403 is correct, however cloudflare does not like
the user-agent request being sent from a script so its sending a Captcha
challenge and giving a 403. I am able to access the site without issue
interactively.
