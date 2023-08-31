## tidytlg 0.1.2

### Testing

Tested on Ubuntu Jammy, GitHub Action, and RHub.

One notes found:

Found the following (possibly) invalid URLs: URL:
<https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf>
From: man/tidytlg.Rd Status: 403 Message: Forbidden

TLG is an industry specific word refering to tables, listings, and
figure(s).

The link returning a 403 is correct, however cloudflare does not like
the user-agent request being sent from a script so its sending a Captcha
challenge and giving a 403. I am able to access the site without issue
interactively.
