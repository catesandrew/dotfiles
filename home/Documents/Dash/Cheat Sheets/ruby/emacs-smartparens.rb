# coding: utf-8

cheatsheet do
  title 'Emacs: SmartParens'
  docset_file_name '../Emacs-SmartParens'
  keyword 'sp'
  introduction "Automatic insertion, wrapping and paredit-like navigation with user defined pairs."

  style '
body {
  font-family: "Helvetica Neue", Helvetica, "Segoe UI", Arial, freesans, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  color: #333;
  font-size: 16px;
  line-height: 1.6;
  color: inherit;
  background-color: inherit;
  margin: inherit;
}

a {
  background-color: transparent;
}

a:active,
a:hover {
  outline: 0;
}

strong {
  font-weight: bold;
}

h1 {
  font-size: 2em;
  margin: 0.67em 0;
}

img {
  border: 0;
}

hr {
  box-sizing: content-box;
  height: 0;
}

pre {
  overflow: auto;
}

code,
kbd,
pre {
  font-family: monospace, monospace;
  font-size: 1em;
}

input {
  color: inherit;
  font: inherit;
  margin: 0;
}

html input[disabled] {
  cursor: default;
}

input {
  line-height: normal;
}

input[type="checkbox"] {
  box-sizing: border-box;
  padding: 0;
}

table {
  border-collapse: collapse;
  border-spacing: 0;
}

td,
th {
  padding: 0;
}

* {
  box-sizing: border-box;
}

input {
  font: 13px / 1.4 Helvetica, arial, nimbussansl, liberationsans, freesans, clean, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
}

a {
  color: #4078c0;
  text-decoration: none;
}

a:hover,
a:active {
  text-decoration: underline;
}

hr {
  height: 0;
  margin: 15px 0;
  overflow: hidden;
  background: transparent;
  border: 0;
  border-bottom: 1px solid #ddd;
}

hr:before {
  display: table;
  content: "";
}

hr:after {
  display: table;
  clear: both;
  content: "";
}

h1,
h2,
h3,
h4,
h5,
h6 {
  margin-top: 15px;
  margin-bottom: 15px;
  line-height: 1.1;
}

h1 {
  font-size: 30px;
  font-weight: inherit;
}

h2 {
  font-size: 21px;
}

h3 {
  font-size: 16px;
}

h4 {
  font-size: 14px;
}

h5 {
  font-size: 12px;
}

h6 {
  font-size: 11px;
}

blockquote {
  margin: 0;
}

ul,
ol {
  padding: 0;
  margin-top: 0;
  margin-bottom: 0;
}

ol ol,
ul ol {
  list-style-type: lower-roman;
}

ul ul ol,
ul ol ol,
ol ul ol,
ol ol ol {
  list-style-type: lower-alpha;
}

dd {
  margin-left: 0;
}

code {
  font-family: Consolas, "Liberation Mono", Menlo, Courier, monospace;
  font-size: 12px;
}

pre {
  margin-top: 0;
  margin-bottom: 0;
  font: 12px Consolas, "Liberation Mono", Menlo, Courier, monospace;
}

.select::-ms-expand {
  opacity: 0;
}

body:before {
  display: table;
  content: "";
}

body:after {
  display: table;
  clear: both;
  content: "";
}

body>*:first-child {
  margin-top: 0 !important;
}

body>*:last-child {
  margin-bottom: 0 !important;
}

a:not([href]) {
  color: inherit;
  text-decoration: none;
}

.anchor {
  display: inline-block;
}

.anchor:focus {
  outline: none;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  margin-top: 1em;
  margin-bottom: 16px;
  font-weight: bold;
  line-height: 1.4;
}

h1:hover .anchor,
h2:hover .anchor,
h3:hover .anchor,
h4:hover .anchor,
h5:hover .anchor,
h6:hover .anchor {
  text-decoration: none;
}

h1 {
  padding-bottom: 0.3em;
  font-size: 2.25em;
  line-height: 1.2;
  border-bottom: 1px solid #eee;
}

h1 .anchor {
  line-height: 1;
}

h2 {
  padding-bottom: 0.3em;
  font-size: 1.75em;
  line-height: 1.225;
  border-bottom: 1px solid #eee;
}

h2 .anchor {
  line-height: 1;
}

h3 {
  font-size: 1.5em;
  line-height: 1.43;
}

h3 .anchor {
  line-height: 1.2;
}

h4 {
  font-size: 1.25em;
}

h4 .anchor {
  line-height: 1.2;
}

h5 {
  font-size: 1em;
}

h5 .anchor {
  line-height: 1.1;
}

h6 {
  font-size: 1em;
  color: #777;
}

h6 .anchor {
  line-height: 1.1;
}

p,
blockquote,
ul,
ol,
dl,
table,
pre {
  /* !important is needed here because Hotmail/Outlook.com uses !important to
     kill the margin in <p>. We need this to win. */
  margin-left: 0 !important;
  margin-right: 0 !important;
  margin-top: 0 !important;
  margin-bottom: 16px !important;
}

hr {
  height: 4px;
  padding: 0;
  margin: 16px 0;
  background-color: #e7e7e7;
  border: 0 none;
}

ul,
ol {
  padding-left: 2em;
}

ul ul,
ul ol,
ol ol,
ol ul {
  margin-top: 0;
  margin-bottom: 0;
}

li>p {
  margin-top: 16px;
}

dl {
  padding: 0;
}

dl dt {
  padding: 0;
  margin-top: 16px;
  font-size: 1em;
  font-style: italic;
  font-weight: bold;
}

dl dd {
  padding: 0 16px;
  margin-bottom: 16px;
}

blockquote {
  padding: 0 15px;
  color: #777;
  border-left: 4px solid #ddd;
}

blockquote>:first-child {
  margin-top: 0;
}

blockquote>:last-child {
  margin-bottom: 0;
}

table {
  background-color: inherit;
  border-collapse: inherit;
  display: block;
  width: 100%;
  overflow: auto;
  word-break: normal;
  word-break: keep-all;
}

table td {
  padding: inherit;
  border-left: inherit;
}

table th {
  font-weight: bold;
  border-left: inherit;
}

table th,
table td {
  padding: 6px 13px;
  border: 1px solid #ddd;
}

table tr {
  background-color: #fff;
  border-top: 1px solid #ccc;
  border-bottom: inherit;
}

table tr:nth-child(2n) {
  background-color: #f8f8f8;
}

table th {
  background: inherit;
  padding: inherit;
  text-align: inherit;
  border-bottom: inherit;
  font-weight: bold;
}

table th:nth-child(1),
table td:nth-child(1) {
  border-left: none;
}


img {
  max-width: 100%;
  box-sizing: content-box;
  background-color: #fff;
}

.task-list-item {
  list-style-type: none;
}

.task-list-item+.task-list-item {
  margin-top: 3px;
}

.task-list-item input {
  margin: 0 0.35em 0.25em -1.6em;
  vertical-align: middle;
}

:checked+.radio-label {
  z-index: 1;
  position: relative;
  border-color: #4078c0;
}

kbd {
  display: inline-block;
  padding: 3px 5px;
  font: 11px Consolas, "Liberation Mono", Menlo, Courier, monospace;
  line-height: 10px;
  color: #555;
  vertical-align: middle;
  background-color: #fcfcfc;
  border: solid 1px #ccc;
  border-bottom-color: #bbb;
  border-radius: 3px;
  box-shadow: inset 0 -1px 0 #bbb;
}

code {
  border: inherit;
  padding: 0;
  padding-top: 0.2em;
  padding-bottom: 0.2em;
  margin: 0;
  font-size: 85%;
  background-color: rgba(0,0,0,0.04);
  border-radius: 3px;
  white-space: pre-wrap;
}

code:before,
code:after {
  letter-spacing: -0.2em;
  content: " ";
}

pre>code {
  padding: 0;
  margin: 0;
  font-size: 100%;
  word-break: normal;
  white-space: pre;
  background: transparent;
  border: 0;
}

.highlight {
  margin-bottom: 16px;
  background-color: inherit;
  border: inherit;
  padding: inherit;
  border-radius: inherit;
  margin-right: inherit;
  word-wrap: inherit;
}

.highlight pre,
pre {
  padding: 16px;
  overflow: auto;
  font-size: 85%;
  line-height: 1.45;
  border-radius: 3px;
  margin-top: inherit;
  margin-bottom: inherit;
}

.highlight pre {
  margin-bottom: 0;
  word-break: normal;
}

pre {
  word-wrap: normal;
}

pre code {
  display: inline;
  max-width: initial;
  padding: 0;
  margin: 0;
  overflow: initial;
  line-height: inherit;
  word-wrap: normal;
  background-color: transparent;
  border: 0;
}

pre code:before,
pre code:after {
  content: normal;
}

.highlight, .ace_editor {
  background-color: #0F0F0F !important;
  color: #FFFFFF !important;
}

.pl-c, .pl-c span     { color: #9933CC !important; font-style: italic !important; } /* comment */
.pl-c1                { color: #339999 !important; } /* constant */
.pl-e                 { color: #FF6600 !important; } /* entity */
.pl-en                { color: #FFFFFF !important; } /* entity.name */
.pl-ent               { color: #FFCC00 !important; } /* entity.name.tag */
.pl-k                 { color: #FF6600 !important; } /* keyword */
.pl-mb                { color: #66FF00 !important; font-weight: bold !important; } /* markup.bold */
.pl-mdh               { color: #FF6600 !important; } /* meta.diff.header */
.pl-mdr               { color: #FF6600 !important; } /* meta.diff.range */
.pl-mh                { color: #99CC99 !important; } /* markup.heading */
.pl-mh .pl-en         { color: #99CC99 !important; font-weight: bold !important; } /* markup.heading entity.name */
.pl-mi                { color: #FF6600 !important; font-style: italic !important; } /* markup.italic */
.pl-ml                { color: #FF6600 !important; } /* markup.list */
.pl-mm                { color: #FF6600 !important; } /* meta.module-reference */
.pl-mo                { color: #FF6600 !important; } /* meta.output */
.pl-mp                { color: #FF6600 !important; } /* meta.property-name */
.pl-mq                { color: #9933CC !important; } /* markup.quote */
.pl-mr                { color: #FF6600 !important; } /* meta.require */
.pl-ms                { color: #FF6600 !important; } /* meta.selector */
.pl-pds               { color: #66FF00 !important; } /* punctuation.definition.string */
.pl-s                 { color: #FFCC00 !important; } /* storage */
.pl-s1                { color: #66FF00 !important; } /* string */
.pl-s1 .pl-pse .pl-s2 { color: #66FF00 !important; } /* string punctuation.section.embedded source */
.pl-s1 .pl-s2         { color: #339999 !important; } /* string source */
.pl-s1 .pl-v          { color: #FF6600 !important; } /* string variable */
.pl-s3                { color: #FFCC00 !important; } /* support */
.pl-sc                { color: #FFFFFF !important; } /* support.class */
.pl-smi               { color: #339999 !important; } /* storage.modifier.import */
.pl-smp               { color: #339999 !important; } /* storage.modifier.package */
.pl-sr                { color: #44B4CC !important; } /* string.regexp */
.pl-sr .pl-cce        { color: #99CC99 !important; } /* constant.character.escape */
.pl-sr .pl-sra        { color: #44B4CC !important; } /* string.regexp string.regexp.arbitrary-repetition */
.pl-sr .pl-sre        { color: #44B4CC !important; } /* string.regexp source.ruby.embedded */
.pl-src               { color: #66FF00 !important; } /* string.regexp.character-class */
.pl-st                { color: #FF6600 !important; } /* support.type */
.pl-stj               { color: #339999 !important; } /* storage.type.java */
.pl-sv                { color: #66FF00 !important; } /* support.variable */
.pl-v                 { color: #FFCC00 !important; } /* variable */
.pl-vo                { color: #339999 !important; } /* variable.other */
.pl-vpf               { color: #FFFFFF !important; } /* variable.parameter.function */

/* Diff - Example: https://gist.github.com/silverwind/904159f1e71e2e626375 */
.pl-mi1               { color: #FFFFFF !important; background: rgba(0,64,0,.5) !important; } /* markup.inserted */
.pl-mdht              { color: #FFFFFF !important; background: rgba(0,64,0,.5) !important; } /* meta.diff.header.to-file */
.pl-md                { color: #FFFFFF !important; background: rgba(64,0,0,.5) !important; } /* markup.deleted */
.pl-mdhf              { color: #FFFFFF !important; background: rgba(64,0,0,.5) !important; } /* meta.diff.header.from-file */
.pl-id                { color: #CCFF33 !important; background: #000000 !important; } /* invalid.deprecated */
.pl-ii                { color: #CCFF33 !important; background: #000000 !important; } /* invalid.illegal */

/* language tweaks */
.highlight-source-css .pl-c1   { color: #99CC99 !important; } /* constant */
.highlight-source-css .pl-ent  { color: #339999 !important; } /* entity.name.tag */
.highlight-source-css .pl-s3   { color: #FFFFFF !important; } /* support */
.highlight-source-css .pl-sc   { color: #339999 !important; } /* support.class */
.highlight-text-html-basic .pl-ent { color: #FF6600 !important; } /* entity.name.tag */
.highlight-text-html-basic .pl-e   { color: #99CC99 !important; } /* entity */
.highlight-source-js .pl-vpf   { color: #FFCC00 !important; } /* variable.parameter.function */
.highlight-text-html-php .pl-s    { color: #FF6600 !important; } /* storage */
.highlight-text-html-php .pl-s3   { color: #FF6600 !important; } /* support */
.highlight-text-html-php .pl-vo   { color: #FFCC00 !important; } /* variable.other */
.highlight-source-c\+\+ .pl-s3   { color: #FFFFFF !important; } /* support */
  '

  category do
    id 'Navigation Shortcuts'

entry do
  name 'sp-forward-sexp'
  command 'C-M-f'
  notes <<-END
Move forward across one balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

```

|(foo bar baz)   -> (foo bar baz)|

(|foo bar baz)   -> (foo| bar baz)

(|foo bar baz)   -> (foo bar| baz) ;; 2

(foo (bar baz|)) -> (foo (bar baz)|)
```

  END
end

entry do
  name 'sp-backward-sexp'
  command 'C-M-b'
  notes <<-END
Move backward across one balanced expression (sexp).

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

```
(foo bar baz)|   -> |(foo bar baz)

(foo| bar baz)   -> (|foo bar baz)

(foo bar| baz)   -> (|foo bar baz) ;; 2

(|(foo bar) baz) -> ((|foo bar) baz)
```
  END
end

entry do
  name 'sp-down-sexp'
  command 'C-M-d'
  notes <<-END
Move forward down one level of sexp.

If the point is inside sexp and there is no down expression to
descend to, jump to the beginning of current one.  If moving
backwards, jump to end of current one.

Examples:

```
|foo (bar (baz quux)) -> foo (|bar (baz quux))

|foo (bar (baz quux)) -> foo (bar (|baz quux)) ;; 2

|foo (bar (baz (quux) blab)) -> foo (bar (baz (|quux) blab)) ;; `C-u`

(foo (bar baz) |quux) -> (|foo (bar baz) quux)

(blab foo |(bar baz) quux) -> (|blab foo (bar baz) quux) ;; `C-u` `C-u`
```
END
end

entry do
  name 'sp-backward-down-sexp'
  command 'C-M-a'
  notes <<-END
Move backward down one level of sexp.

If the point is inside sexp and there is no down expression to
descend to, jump to the end of current one.  If moving forward,
jump to beginning of current one.

Examples:

```
foo (bar (baz quux))| -> foo (bar (baz quux)|)

(bar (baz quux)) foo| -> (bar (baz quux|)) foo ;; 2

foo (bar (baz (quux) blab))| -> foo (bar (baz (quux|) blab)) ;; `C-u`

(foo| (bar baz) quux) -> (foo (bar baz) quux|)

(foo (bar baz) |quux blab) -> (foo (bar baz) quux blab|) ;; `C-u` `C-u`
```
END
end

entry do
  name 'sp-beginning-of-sexp'
  command 'C-S-d'
  notes <<-END
Jump to beginning of the sexp the point is in.

The beginning is the point after the opening delimiter.

With no argument, this is the same as calling
`C-u` `C-u` `sp-down-sexp`

Examples:

```common-lisp
  (foo (bar baz) quux (blab glob)) ;; =>
;;                   ¯
  (foo (bar baz) quux (blab glob))
;; ¯

  (foo (bar baz) quux (blab glob)) ;; =>
;;             ¯
  (foo (bar baz) quux (blab glob))
;;      ¯

  (foo) (bar) (baz quux) ;; => `C-u` `3`
;; ¯
  (foo) (bar) (baz quux)
;;             ¯

  (foo bar) (baz) (quux) ;; => `C-u` `-3`
;;                     ¯
  (foo bar) (baz) (quux)
;; ¯

  ((foo bar) (baz quux) blab) ;; => `C-u`
;;                ¯
  ((foo bar) (baz quux) blab)
;; ¯
```

If you want to move point to the start of the string, use `C-S-d`:

```common-lisp
  (let [x "foo bar baz ... blah"]) ;; =>
;;                         ¯
  (let [x "foo bar baz ... blah"])
;;         ¯
```

  END
end

entry do
  name 'sp-end-of-sexp'
  command 'C-S-a'
  notes <<-END
Jump to end of the sexp the point is in.

The end is the point before the closing delimiter.

With no argument, this is the same as calling
`C-u` `C-u` `sp-backward-down-sexp`.

Examples:

```common-lisp
  (foo (bar baz) quux (blab glob)) ;; =>
;;     ¯
  (foo (bar baz) quux (blab glob))
;;                               ¯

  (foo (bar baz) quux (blab glob)) ;; =>
;;      ¯
  (foo (bar baz) quux (blab glob))
;;             ¯

  (foo) (bar) (baz quux) ;; =>
;; ¯
  (foo) (bar) (baz quux) ;; `C-u` `3`
;;                     ¯

  (foo bar) (baz) (quux) ;; =>
;;                     ¯
  (foo bar) (baz) (quux) ;; `C-u` `-3`
;;        ¯

  ((foo bar) (baz quux) blab) ;; =>
;;      ¯
  ((foo bar) (baz quux) blab) ;; `C-u`
;;                          ¯
```

If you want to move point to the end of the string, use `C-S-a`:

```common-lisp
  (let [x "foo bar baz blah"]) ;; =>
;;                     ¯
  (let [x "foo bar baz blah"])
;;                         ¯
```
  END
end

entry do
  name 'sp-up-sexp'
  command 'C-M-e'
  notes "
Move forward out of one level of parentheses.

If `sp-navigate-close-if-unbalanced' is non-nil, close the
unbalanced expressions automatically.

Examples:

```
(foo |(bar baz) quux blab) -> (foo (bar baz) quux blab)|

(foo (bar |baz) quux blab) -> (foo (bar baz) quux blab)| ;; 2

(foo bar |baz              -> (foo bar baz)| ;; re-indent the expression
 )

(foo  |(bar baz)           -> (foo)| (bar baz) ;; close unbalanced expr.
```
"
end

entry do
  name 'sp-backward-up-sexp'
  command 'C-M-u'
  notes "
Move backward out of one level of parentheses.

Examples:

```
(foo (bar baz) quux| blab) -> |(foo (bar baz) quux blab)

(foo (bar |baz) quux blab) -> |(foo (bar baz) quux blab) ;; 2

(                  -> |(foo bar baz)
  foo |bar baz)
```
  "
end

entry do
  name 'sp-next-sexp'
  command 'C-M-n'
  notes "
Move forward to the beginning of next balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

```
((foo) |bar (baz quux)) -> ((foo) bar |(baz quux))

((foo) bar |(baz quux)) -> |((foo) bar (baz quux))
```
  "
end

entry do
  name 'sp-previous-sexp'
  command 'C-M-p'
  notes "
Move backward to the end of previous balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

```
((foo) bar| (baz quux)) -> ((foo)| bar (baz quux))

((foo)| bar (baz quux)) -> ((foo) bar (baz quux))|
```
  "
end

entry do
  name 'sp-kill-sexp'
  command 'C-M-k'
  notes "
Kill the balanced expression following point.

If point is inside an expression and there is no following
expression, kill the topmost enclosing expression.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

With ARG being raw prefix `C-u`, kill all the expressions from
point up until the end of current list.  With raw prefix [negative-argument] `C-u`,
kill all the expressions from beginning of current list up until
point.  If point is inside a symbol, this is also killed.  If
there is no expression after/before the point, just delete the
whitespace up until the closing/opening delimiter.

With ARG being raw prefix `C-u` `C-u`, kill current list (the list
point is inside).

With ARG numeric prefix 0 (zero) kill the insides of the current
list, that is everything from after the opening delimiter to
before the closing delimiter.

If ARG is nil, default to 1 (kill single expression forward)

With `sp-navigate-consider-symbols', symbols and strings are also
considered balanced expressions.

Examples:

```
(foo |(abc) bar)  -> (foo | bar) ;; nil, defaults to 1

(foo (bar) | baz) -> |           ;; 2

(foo |(bar) baz)  -> |           ;; `C-u` `C-u`

(1 |2 3 4 5 6)    -> (1|)        ;; `C-u`

(1 |2 3 4 5 6)    -> (1 | 5 6)   ;; 3

(1 2 3 4 5| 6)    -> (1 2 3 | 6) ;; -2

(1 2 3 4| 5 6)    -> (|5 6)      ;; - `C-u`

(1 2 |   )        -> (1 2|)      ;; `C-u`, kill useless whitespace

(1 2 3 |4 5 6)    -> (|)         ;; 0
```

Note: prefix argument is shown after the example. Assumes `sp-navigate-consider-symbols' equal to t.
  "
end

entry do
  name 'sp-copy-sexp'
  command 'C-M-w'
  notes "
Copy the following ARG expressions to the kill-ring.

This is exactly like calling `sp-kill-sexp' with second argument
t.  All the special prefix arguments work the same way.
  "
end

entry do
  name 'sp-transpose-sexp'
  command ''
  notes "
Transpose the expressions around point.

The operation will move the point after the transposed block, so
the next transpose will \"drag\" it forward.

Examples:

```
foo |bar baz     -> bar foo| baz

foo |bar baz     -> bar baz foo| ;; 2

(foo) |(bar baz) -> (bar baz) (foo)|

(foo bar)        ->    (baz quux)   ;; keeps the formatting
  |(baz quux)            |(foo bar)

foo bar baz|     -> foo baz| bar ;; -1
```
  "
end

entry do
  name 'sp-unwrap-sexp'
  command 'M-<delete>'
  notes "
Unwrap the following expression.

Return the information about the just unwrapped expression.  Note
that this structure does not represent a valid expression in the
buffer.

Examples:

```
|(foo bar baz)     -> |foo bar baz

(foo bar| baz)     -> foo bar| baz

|(foo) (bar) (baz) -> |(foo) bar (baz) ;; 2
```
  "
end

entry do
  name 'sp-backward-unwrap-sexp'
  command 'M-<backspace>'
  notes "
Unwrap the previous expression.

Examples:

```
(foo bar baz)|     -> foo bar baz|

(foo bar)| (baz)   -> foo bar| (baz)

(foo) (bar) (baz)| -> foo (bar) (baz) ;; 3
```
  "
end

entry do
  name 'sp-forward-slurp-sexp'
  command 'C-<right>'
  notes "
Add sexp following the current list in it by moving the closing delimiter.

If the current list is the last in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or end of file).

If both the current expression and the expression to be slurped
are strings, they are joined together.

See also `sp-slurp-hybrid-sexp' which is similar but handles
C-style syntax better.

Examples:

```
(foo |bar) baz        -> (foo |bar baz)

[(foo |bar)] baz      -> [(foo |bar) baz]

[(foo |bar) baz]      -> [(foo |bar baz)]

((|foo) bar baz quux) -> ((|foo bar baz quux)) ;; with `C-u`

\"foo| bar\" \"baz quux\" -> \"foo| bar baz quux\"
```
  "
end

entry do
  name 'sp-forward-barf-sexp'
  command 'C-<left>'
  notes "
Remove the last sexp in the current list by moving the closing delimiter.

If the current list is empty, do nothing.

Examples: (prefix arg in comment)

```
(foo bar| baz)   -> (foo bar|) baz   ;; nil (defaults to 1)

(foo| [bar baz]) -> (foo|) [bar baz] ;; 1

(1 2 3| 4 5 6)   -> (1 2 3|) 4 5 6   ;; `C-u` (or numeric prefix 3)

(foo bar| baz)   -> foo (bar| baz)   ;; -1
```
  "
end

entry do
  name 'sp-backward-slurp-sexp'
  command 'C-M-<left>'
  notes "
Add the sexp preceding the current list in it by moving the opening delimiter.

If the current list is the first in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or beginning of file).

If both the current expression and the expression to be slurped
are strings, they are joined together.

Examples:

```
foo (bar| baz)        -> (foo bar| baz)

foo [(bar| baz)]      -> [foo (bar| baz)]

[foo (bar| baz)]      -> [(foo bar| baz)]

(foo bar baz (|quux)) -> ((foo bar baz |quux)) ;; with `C-u`

\"foo bar\" \"baz |quux\" -> \"foo bar baz |quux\"
```
  "
end

entry do
  name 'sp-backward-barf-sexp'
  command 'C-M-<right>'
  notes "
This is exactly like calling `sp-forward-barf-sexp' with minus ARG.
In other words, instead of contracting the closing pair, the
opening pair is contracted.  For more information, see the
documentation of `sp-forward-barf-sexp'.

Examples:

```
(foo bar| baz) -> foo (bar| baz)

([foo bar] |baz) -> [foo bar] (|baz)

(1 2 3 |4 5 6) -> 1 2 3 (|4 5 6) ;; `C-u` (or 3)
```
  "
end

entry do
  name 'sp-splice-sexp'
  command 'M-D'
  notes "
Unwrap the current list.

Examples:

```
(foo (bar| baz) quux) -> (foo bar| baz quux)

(foo |(bar baz) quux) -> foo |(bar baz) quux

(foo (bar| baz) quux) -> foo (bar| baz) quux ;; 2
```
  "
end

entry do
  name 'sp-splice-sexp-killing-forward'
  command 'C-M-<delete>'
  notes "
Unwrap the current list and kill all the expressions between
the point and the end of this list.

Examples:

```
(a (b c| d e) f) -> (a b c| f)

(+ (x |y z) w)   -> (+ x| w)
```

Note that to kill only the content and not the enclosing
delimiters you can use `C-u` `sp-kill-sexp`.
See `sp-kill-sexp' for more information.
  "
end

entry do
  name 'sp-splice-sexp-killing-backward'
  command 'C-M-<backspace>'
  notes "
Unwrap the current list and kill all the expressions
between start of this list and the point.

Examples:

```
(foo (let ((x 5)) |(sqrt n)) bar)  -> (foo |(sqrt n) bar)

(when ok|                             |(perform-operation-1)
  (perform-operation-1)            ->  (perform-operation-2)
  (perform-operation-2))

(save-excursion                    -> |(awesome-stuff-happens) ;; 2
  (unless (test)
    |(awesome-stuff-happens)))
```

Note that to kill only the content and not the enclosing
delimiters you can use `C-u` `sp-backward-kill-sexp`.
See `sp-backward-kill-sexp' for more information.

  "
end

entry do
  name 'sp-splice-sexp-killing-around'
  command 'C-S-<backspace>'
  notes "
Unwrap the current list and kill everything inside except next expression.

Note that the behaviour with the prefix argument seems to be
reversed.  This is because the backward variant is much more
common and hence deserve shorter binding.

If ARG is raw prefix argument `C-u` `C-u` raise the expression the point
is inside of.  This is the same as `sp-backward-up-sexp' followed by
`sp-splice-sexp-killing-around'.

Examples:

```
(a b |(c d) e f)      -> |(c d)     ;; with arg = 1

(a b |c d e f)        -> |c d       ;; with arg = 2

(- (car x) |a 3)      -> (car x)|   ;; with arg = -1

(foo (bar |baz) quux) -> |(bar baz) ;; with arg = `C-u` `C-u`
```
  "
end

entry do
  name 'sp-select-next-thing-exchange'
  command 'C-]'
  notes "
Just like `sp-select-next-thing' but run `exchange-point-and-mark' afterwards.
  "
end

entry do
  name 'sp-select-next-thing'
  command 'C-M-]'
  notes <<-END
Set active region over next thing as recognized by `sp-get-thing'.

If POINT is non-nil, it is assumed it's a point inside the buffer
from which the selection extends, either forward or backward,
depending on the value of ARG.

If the currently active region contains a balanced expression,
following invocation of `sp-select-next-thing' will select the
inside of this expression. Therefore calling this function
twice with no active region will select the inside of the next
expression.

If the point is right in front of the expression any potential
prefix is ignored.  For example, '|(foo) would only select (foo)
and not include ' in the selection.  If you wish to also select
the prefix, you have to move the point backwards.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.
  END
end

entry do
  name 'sp-mark-sexp'
  command 'C-M-SPC'
  notes <<-END
Set mark ARG balanced expressions from point.
The place mark goes is the same place `sp-forward-sexp` would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG sexps after the ones already marked.
This command assumes point is not in a string or comment.
  END
end

entry do
  name 'sp-forward-symbol'
  command 'M-F'
  notes <<-END
Move point to the next position that is the end of a symbol.

A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.  Current
symbol only extend to the possible opening or closing delimiter
as defined by `sp-add-pair' even if part of this delimiter
would match \"symbol\" syntax classes.

Examples:

```
|foo bar baz          -> foo| bar baz

|foo (bar (baz))      -> foo (bar| (baz)) ;; 2

|foo (bar (baz) quux) -> foo (bar (baz) quux|) ;; 4

```
  END
end

entry do
  name 'sp-backward-symbol'
  command 'M-B'
  notes <<-END
Move point to the next position that is the beginning of a symbol.

A symbol is any sequence of characters that are in either the word
constituent or symbol constituent syntax class.  Current symbol only
extend to the possible opening or closing delimiter as defined by
`sp-add-pair' even if part of this delimiter would match \"symbol\"
syntax classes.

Examples:

```
foo bar| baz            -> foo |bar baz

((foo bar) baz)|        -> ((foo |bar) baz) ;; 2

(quux ((foo) bar) baz)| -> (|quux ((foo) bar) baz) ;; 4
```
    END
end

  end


  notes <<-END
    * Created by [andrew](https://github.com/catesandrew).
  END

end
