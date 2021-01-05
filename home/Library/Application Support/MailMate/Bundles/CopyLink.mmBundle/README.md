# MailMate Copy Message Link Command #

Copies a `message://` link to email currently selected in MailMate to the clipboard with the email subject as the title.

The link is copied in multiple formats, with the email subject as the link text:

|               Format               |              UTI              |
|------------------------------------|-------------------------------|
| URL                                | `public.url`                  |
| Rich text                          | `public.rtf`                  |
| HTML                               | `public.html`                 |
| Plain text (`message://` URI only) | `public.utf8-plain-text`      |

There are 2 defined commands:

1. `Copy Email Link` (Keyboard shortcut: `^⇧+C`) — Copy a link to the selected message in the above formats.
2. `Copy Markdown Email Link` — As above, but the plain text link will be a Markdown link: `[<subject>](<message://url>)`.

To install the bundle, download the repo, remove `-master` from the folder name, and drop it in your `~/Library/Application Support/MailMate/Bundles` directory.
