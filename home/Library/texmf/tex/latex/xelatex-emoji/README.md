# xelatex-emoji
Emoji support for XeLaTeX documents

## Installation
Clone this repo into your [`texmf` folder](https://en.wikibooks.org/wiki/LaTeX/Installing_Extra_Packages) or to the project folder. Clone emoji images named after the utf8 code - I recommend [EmojiOne](https://github.com/Ranks/emojione).

Note: Tex doesn't natively support SVG, so if you download the vector emojis, you can convert them to PDFs with the following command on Mac and Linux: (assumes you have installed the librsvg2-bin package in Linux)

```bash
# For Mac OS X:
$ brew install librsvg

# For Linux:
$ sudo apt-get install librsvg2-bin

# Then, run:
$ rsvg-convert -f pdf -o <emoji-UTF-code>.pdf <emoji-UTF-code>.svg
```

If you don't want to hand convert massive emoji SVGs, run the ```bin/convert_svgs_to_pdfs``` command (included this repo) in the folder where the SVGs are:
```bash
bin/convert_svgs_to_pdfs ./*.svg
```

## Usage
Use the `xelatexemoji` package. Enjoy :)

## Example layout
```tex
\documentclass{article}

\usepackage{xltxtra}
\usepackage{xelatexemoji}

\setmainfont{Ubuntu}

\begin{document}

  Hello, 🌎. Lorem 😃 😄 😅 😆 ipsum dolor.

\end{document}
```

yields (using the great [EmojiOne](https://github.com/Ranks/emojione) images)

![example result](example/example.png)

### Image path

By default, xelatex-emoji expects the images to be in `images/utf8code.extension`. The package supports the same image formats (extensions) as XeLaTeX.

For example: If you insert the emoji 👌 (code `1F44C`) in your document, then the translation will work if the file `images/1F44C.pdf` or `images/1F44C.png` exist. If both exist, then XeLaTeX will use the “better” version `images/1F44C.pdf`.

You can change the emoji image path and extension by creating your own `\xelatexemojipath` command.

```tex
\newcommand{\xelatexemojipath}[1]{mycustompath/#1.pdf}
```

## Development

To generate the flag codes:

```
ruby bin/generate_flags.rb > xelatexemoji-flags.sty
```
