# Requests to DataSuite

An app asks DataSuite, the application running it, for a few things it cannot do itself: open the chat with a prompt,
print a page to PDF (and draw a PDF's pages as pictures), install packages it needs only now and then. They all go
through one function, `ds_host_request(action, args, wait = FALSE)` (`R/kit-host.R`), which sends the request one of
two ways:

| Channel | When | How |
| --- | --- | --- |
| **Jovian's host channel** | DataSuite started the app with `CDSUITE_HOST_UI=1`, and the app runs in a Jovian R kernel (Elara) whose hera has `host_notify()` and `host_ask()` (Jovian 0.2.3 and later) | `hera::host_notify("datasuite.<action>", args)`, or `hera::host_ask("datasuite.<action>", args)` to wait for the answer. DataSuite sees a Jovian Session `'ui'` event `{ method, params, reply? }`. |
| **A line on the R session's output** | anything else (an older DataSuite or kernel, plain R) | `DATASUITE_HOST_REQUEST <json>` written with `message()` (stderr, not buffered), the JSON an object `{ "action": <action>, ...args }`. An answer is written by DataSuite to the file named in the request's `done`. |

`CDSUITE_HOST_UI=1` is DataSuite's promise that it answers the `datasuite.*` methods below: the app only uses the host
channel when it is set, so an older DataSuite keeps getting the line it reads today. The callers decide whether the
app runs in DataSuite at all, as before: `CDSUITE_SHINY_ID` set (the chat, the packages), `CDSUITE_PRINT=1`
(printing). hera is not a dependency of the package (it is not on CRAN); it is looked up when it is there.

A notification hera could not send (`host_notify()` gives `FALSE`: not in a kernel after all) goes on the line instead.

## The requests

`args` is a JSON object. Arrays are always arrays (`ds_host_request()` is given them as lists, so one element stays an
array). Paths are absolute, with forward slashes, in the R session's temporary folder.

| Action (method) | Sent | Args | What DataSuite does | Answer |
| --- | --- | --- | --- | --- |
| `openChat` (`datasuite.openChat`) | notification | `query`: text, at most 1000 characters | Opens the chat with `query` in the input, for the user to finish or send. Nothing is sent. (`shinyAppAiBridge.ts`) | none |
| `installPackages` (`datasuite.installPackages`) | notification | `packages`: array of R package names (today the Bayesian model's: `bayescoveragemodel`, `bayescoveragedeploy`) | Installs them from the app's repos (valid R package names, at most 20), showing the progress, then offers to restart the app. (`shinyAppPackages.contribution.ts`) | none |
| `print` (`datasuite.print`) | question (waits) | `pdf`: the PDF to write, or the existing PDF when there is no `html`; `html` (optional): the printable page, a file; `pages` (optional): a folder to draw the PDF's pages into as `page_001.png`, `page_002.png`, ...; `dpi`: the pages' resolution | Prints `html` to `pdf` (Chromium's printing) and/or draws `pdf`'s pages into `pages` (pdf.js), within its time limit. (`shinyAppPrint.contribution.ts`, `nativeHostMainService.printHtmlToPdf`) | `{ "ok": true, "pages": <number of pages drawn> }`, or `{ "ok": false, "error": <message> }` |

On the line, `print` also carries `done`: the file DataSuite writes the answer to (the same JSON). R reads it every
0.1 s, for at most 120 s. On the host channel there is no `done`: the answer is `host_ask()`'s reply, and R waits
until it comes -- **DataSuite must always answer a `datasuite.print`**, also when it fails or takes too long
(`{ "ok": false, "error": ... }`). R reads the answer as JSON (lists); no answer at all is the same as "did not print
in time" (an error in the app, which then prints its own way where it can).

Callers in this package: the Reports page's PDF and page pictures (`.ds_print()`, `R/report-print.R`, used by
`.ds_host_pdf()`, `.ds_host_pages()`, `report_final_pages()` and `export_report(format = "pdf")` without Word or
LibreOffice), the Ask AI buttons (`.ai_host_request("openChat", ...)`, `R/kit-ai-bridge.R`). In cd2030.core:
`cd_request_bayes_packages()` (`installPackages`).

## What DataSuite implements for the host channel

1. Start the app's R session with `CDSUITE_HOST_UI=1` (beside `CDSUITE_SHINY_ID` and `CDSUITE_PRINT=1`) once it
   handles the methods below; leave it unset otherwise.
2. Listen to the Jovian Session's `'ui'` events of the app's session, matched to its Shiny tab by session id as the
   line requests are today. Ignore methods that do not start with `datasuite.`, and requests from a session that is not
   an app open in a Shiny tab (answering a question with `{ "ok": false, "error": "..." }` rather than not at all).
3. `datasuite.openChat` and `datasuite.installPackages`: do what the line requests do today, with `params` in place
   of the line's fields (same names, same checks: the query's length, valid package names).
4. `datasuite.print`: run `printHtmlToPdf({ html, pdf, pages, dpi })` without `done`, then `reply()` with
   `{ ok: true, pages }` or `{ ok: false, error }` -- always, within the print time limit.
5. Keep reading the `DATASUITE_HOST_REQUEST` lines: apps on older kernels, or run without `CDSUITE_HOST_UI`, still
   send them.
