# coding: utf-8
cheatsheet do
  title 'DefaultKeyBinding'
  docset_file_name 'DefaultKeyBinding'
  keyword 'keybinding'

  introduction "DefaultKeyBinding.dict file for Mac OS X, created by [Brett Terpstra](http://brettterpstra.com) and based heavily on work done by [Lri](http://www.cs.helsinki.fi/u/lranta/keybindings/). Please note that these bindings won't work in all applications: TextWrangler and TextMate, for example, override these with their own settings.
See Lri's [gists](https://gist.github.com/Lri) and [website](http://www.cs.helsinki.fi/u/lranta/) for more coding madness.

@ is ⌘, ~ is ⌥, ^ is Control, and $ is ⇧

<b>Documentation</b> <i>(last updated 05/27/2015.)</i>

*Grouped items begin with the groups shortcut (if exists), followed by a subgroup (if exists) followed by the keys specified.*"

  category do
    id 'General Commands'
    entry do
      name 'replace yank: command with yankAndSelect for use with the kill ring'
      command '^y'
      notes <<-END
        ( defaults write -g NSTextKillRingSize -string 6)
      END
    end
    entry do
      name 'uppercase word'
      command '^⇧u'
      notes <<-END

      END
    end
    entry do
      name 'lowercase word'
      command '^⌥u'
      notes <<-END

      END
    end
    entry do
      name 'titlecase word'
      command '^⇧t'
      notes <<-END

      END
    end
    entry do
      name 'uppercase current paragraph'
      command '^⌥⇧u'
      notes <<-END

      END
    end
    entry do
      name 'titlecase paragraph'
      command '^⌥t'
      notes <<-END

      END
    end
    entry do
      name 'delete word before cursor'
      command '^w'
      notes <<-END

      END
    end
    entry do
      name 'select word'
      command '⌥w'
      notes <<-END

      END
    end
    entry do
      name 'select word backward and modify selection'
      command '⌥⇧w'
      notes <<-END

      END
    end
    entry do
      name 'select entire line/paragraph'
      command '⌥⇧s'
      notes <<-END
If you want to make sure you get everything, including the last line break, use
`⌥⇧s`
      END
    end
    entry do
      name 'select from beginning of paragrah to last character'
      command '⌥s'
      notes <<-END
You can select a paragraph by hitting `⌘←`, using the up arrow to get to the
beginning, then hitting `⌘⇧→` and using the down arrow to get to the end. That’s
horrible.

You can use `^a` to jump straight to the beginning of the paragraph instead of
the edge of the screen. Then hit `^⇧e` to select to the end of the actual
paragraph instantly. That’s much better.

If you’re me, though, you want to do that in a single stroke. This keybinding
lets you select the current line/paragraph with `⌥s`. If you want to make sure
you get everything, including the last line break, use `⌥⇧s`.

      END
    end
    entry do
      name 'select paragraph excluding leading/trailing whitespace (same as ^$@\UF701)'
      command '^⌥⇧s'
      notes <<-END

      END
    end
    entry do
      name 'delete line/paragraph'
      command '⌥d'
      notes <<-END

      END
    end
    entry do
      name 'copy paragraph'
      command '⌥y'
      notes <<-END

      END
    end
    entry do
      name 'cut paragraph'
      command '⌥x'
      notes <<-END

      END
    end
    entry do
      name 'paste paragraph below'
      command '⌥p'
      notes <<-END

      END
    end
    entry do
      name 'paste paragraph above'
      command '⌥⇧p'
      notes <<-END

      END
    end
    entry do
      name 'select to beginning of paragraph and copy'
      command '^⇧a'
      notes <<-END

      END
    end
    entry do
      name 'select to end of paragraph and copy'
      command '^⇧e'
      notes <<-END

      END
    end
    entry do
      name 'cut to beginning of paragraph'
      command '⌥q'
      notes <<-END

      END
    end
    entry do
      name 'cut to end of paragraph'
      command '⌥k'
      notes <<-END

      END
    end
    entry do
      name 'blank line after current'
      command '⌥o'
      notes <<-END

      END
    end
    entry do
      name 'blank line before current'
      command '⌥⇧o'
      notes <<-END

      END
    end
    entry do
      name 'move line up'
      command '^⌘k'
      notes <<-END

      END
    end
    entry do
      name 'move line down'
      command '^⌘j'
      notes <<-END

      END
    end
    entry do
      name 'indent line'
      command '^⌘l'
      notes <<-END

      END
    end
    entry do
      name 'outdent line (one tab or char)'
      command '^⌘h'
      notes <<-END

      END
    end
    entry do
      name 'move line up'
      command '^⌘↑'
      notes <<-END
This one is especially handy when working with lists in Markdown, but is good
for arranging paragraphs or lines of text anywhere. With `^⌘↑` and `^⌘↓` you can
move the current line of text above or below whatever precedes or follows it.

Note: this will balk if you try to move into an area with no newline where it’s
trying to go. Thus, the last item in a list that’s the last thing on the page is
a little problematic. Beyond that minor snag, this is something I use daily.
      END
    end
    entry do
      name 'move line down'
      command '^⌘↓'
      notes <<-END

      END
    end
    entry do
      name 'indent line'
      command '^⌘→'
      notes <<-END

      END
    end
    entry do
      name 'outdent line (one tab or char)'
      command '^⌘←'
      notes <<-END

      END
    end
    entry do
      name 'Full outdent - Deletes all leading space of line/paragraph (updated)'
      command '^⇧⌘←'
      notes <<-END
        ( Control-shift-command-left arrow)
      END
    end
    entry do
      name 'Delete trailing space'
      command '^⇧⌘→'
      notes <<-END

      END
    end
    entry do
      name 'Delete leading and trailing whitespace for paragraph'
      command '^⌘⇧↑'
      notes <<-END

      END
    end
    entry do
      name 'Select paragraph without leading or trailing whitespace'
      command '^⌘⇧↓'
      notes <<-END

      END
    end
    entry do
      name 'modify selection up by paragraph (Control Option Shift Up)'
      command '^⌥⇧↑'
      notes <<-END

      END
    end
    entry do
      name 'modify selection down by paragraph (Control Option Shift Down)'
      command '^⌥⇧↓'
      notes <<-END

      END
    end
    entry do
      name 'modify selection left by word'
      command '^⌥⇧←'
      notes <<-END

      END
    end
    entry do
      name 'modify selection right by word'
      command '^⌥⇧→'
      notes <<-END

      END
    end
    entry do
      name 'Move to first Alphanumeric character of line (new)'
      command '⌘⌥^←'
      notes <<-END

      END
    end
    entry do
      name 'Move to first non-whitespace character of line (new)'
      command '⌘⌥←'
      notes <<-END

      END
    end
    entry do
      name 'Select to first character of line with leading space (new)'
      command '⌘⌥⇧←'
      notes <<-END

      END
    end
    entry do
      name 'Move to last non-whitespace character of paragraph (new)'
      command '⌥⌘→'
      notes <<-END

      END
    end
    entry do
      name 'Move to end of paragraph and delete trailing whitespace (new)'
      command '^⌥→'
      notes <<-END

      END
    end
    entry do
      name 'TextMate Command-Return (Command Enter)'
      command '⌘↩'
      notes <<-END
One of the keybindings that I miss the most when I’m outside of a good code
editor is `⌘↩`. In most editors (and first in TextMate to the best of my
knowledge), hitting this combination will insert a new line after the current
paragraph and jump you to it, regardless of where the cursor is in the
paragraph.

I also use one with `⌘⇧↩` that will do the same, but above the current paragraph.
      END
    end
    entry do
      name 'Insert blank line above paragraph (Command Shift Enter)'
      command '⌘⇧↩'
      notes <<-END

      END
    end
    entry do
      name 'hyphenate next space and move to next word'
      command '⌘⌥⇧-'
      notes <<-END
> this will kill non alphanumeric symbols and punctuation, use only on *words*

If you blog at all, you’re used to creating slugs (hyphenated words). If you do
anything with file naming and prefer to keep spaces out of your titles, you’ve
probably done this as well. This keystroke (`⌘⌥⇧-`) lets you add a hyphen between
the next two words from the cursor, then advance so that repeated keystrokes
continue to hyphenate.
      END
    end
    entry do
      name 'hyphenate next space and move to next word'
      command '⌘⌥_'
      notes <<-END
        ( this will kill non alphanumeric symbols and punctuation, use only on *words*)
      END
    end
    entry do
      name 'set bookmark - save your spot in a text file'
      command '⌥1'
      notes <<-END
With two simple commands you can have a pair of keyboard shortcuts that will
store the cursor position in your text so you can go and making an edit or check
a reference elsewhere in the document, then jump right back to where you were. I
bind these to `⌥1` and `⌥2`.
      END
    end
    entry do
      name 'jump to bookmark - save your spot in a text file'
      command '⌥2'
      notes <<-END

      END
    end
    entry do
      name 'Continue a list item with indentation and include the same delimiter'
      command '⌥⌘↩'
      notes <<-END
        ( Command Option Enter)
      END
    end
    entry do
      name 'remove one tab (or character) from start of line (outdent)'
      command '⇧⇥'
      notes <<-END
        ( Shift Tab)
      END
    end
    entry do
      name 'bold selection (Markdown)'
      command '⌘⌥b'
      notes <<-END

      END
    end
    entry do
      name 'italicize selection (Markdown)'
      command '⌘⌥i'
      notes <<-END

      END
    end
    entry do
      name 'increase markdown header level'
      command '⌘⌥='
      notes <<-END

      END
    end
    entry do
      name 'decrease markdown header level'
      command '⌘⌥-'
      notes <<-END

      END
    end
    entry do
      name 'increase blockquote header level'
      command '⌘⌥>'
      notes <<-END

      END
    end
    entry do
      name 'decrease blockquote level'
      command '⌘⌥<'
      notes <<-END

      END
    end
    entry do
      name 'Make selected text into paired HTML tag. Allows attributes, only dupes first word into closing tag (caveat: overwrites your pasteboard)'
      command '^<'
      notes <<-END

      END
    end
    entry do
      name 'repeat character before cursor'
      command '⌥r'
      notes <<-END

      END
    end
    entry do
      name 'Forward delete to end of paragraph'
      command '⌘⇧⌦'
      notes <<-END

      END
    end
    entry do
      name 'Delete to beginning of paragraph'
      command '⌘⇧⌫'
      notes <<-END

      END
    end
    entry do
      name 'Right mouse click (useless, doesn\'t maintain cursor position)'
      command '⌘⌥7'
      notes <<-END

      END
    end
    entry do
      name 'Real, honest-to-goodnes Save As...'
      command '⌘⌥⇧s'
      notes <<-END

      END
    end
  end


  category do
    id 'Commenting commands (^⌘c)'
    entry do
      name 'comment with "//"'
      command '^⌘c  /'
      notes <<-END

      END
    end
    entry do
      name 'comment with "#"'
      command '^⌘c  \\'
      notes <<-END

      END
    end
    entry do
      name 'HTML commenting'
      command '^⌘c  !'
      notes <<-END

      END
    end
    entry do
      name 'Css Commenting'
      command '^⌘c  *'
      notes <<-END

      END
    end
  end


  category do
    id 'Multi-stroke Markdown commands (^⌘w)'
    entry do
      name 'force carriage return in text field'
      command '^⌘w  ␍'
      notes <<-END

      END
    end
    entry do
      name 'force tab in text field'
      command '^⌘w  ⇥'
      notes <<-END

      END
    end
    entry do
      name 'insert reference link `[selection][[cursor]]`'
      command '^⌘w  \'
      notes <<-END

      END
    end
    entry do
      name 'insert reference `[selection]: [cursor]`'
      command '^⌘w  \'
      notes <<-END

      END
    end
    entry do
      name 'Unordered list item with'
      command '^⌘w  +'
      notes <<-END

      END
    end
    entry do
      name 'Unordered list item with -'
      command '^⌘w  -'
      notes <<-END

      END
    end
    entry do
      name 'Unordered list item with *'
      command '^⌘w  *'
      notes <<-END

      END
    end
    entry do
      name 'convert current numbered list item to bullet, handles indentation'
      command '^⌘w  8'
      notes <<-END

      END
    end
    entry do
      name 'convert current bullet list item to numbered'
      command '^⌘w  1'
      notes <<-END

      END
    end
  end

  category do
    id 'Headlines (removes leading whitespace after inserting hashmarks) (h)'
    entry do
      name '#'
      command '^⌘w h 1'
      notes <<-END

      END
    end
    entry do
      name '##'
      command '^⌘w h 2'
      notes <<-END

      END
    end
    entry do
      name '###'
      command '^⌘w h 3'
      notes <<-END

      END
    end
    entry do
      name '####'
      command '^⌘w h 4'
      notes <<-END

      END
    end
    entry do
      name '#####'
      command '^⌘w h 5'
      notes <<-END

      END
    end
    entry do
      name '######'
      command '^⌘w h 6'
      notes <<-END

      END
    end
  end

  category do
    id 'Markdown link (l)'
    entry do
      name 'create a link for selected text, cursor between () `[selected text]([cursor])`'
      command '^⌘w l t'
      notes <<-END
        ( links without selected text first, these can produce a mess using multiple clipboards make a text selection before you run them)
      END
    end
    entry do
      name 'create a link for selected text, inserting clipboard as url `[[cursor]selected text](clipboard contents)`'
      command '^⌘w l c'
      notes <<-END

      END
    end
  end

  category do
    id 'Link as image (i)'
    entry do
      name 'same as lt, but with image syntax `![selected text]([cursor])`'
      command '^⌘w i t'
      notes <<-END

      END
    end
    entry do
      name 'same as lc, but with image syntax `![selected text](clipboard)`'
      command '^⌘w i c'
      notes <<-END

      END
    end
  end

  category do
    id 'Reference links (:)'
    entry do
      name 'create a reference from selected text'
      command '^⌘w : t'
      notes <<-END

      END
    end
    entry do
      name 'create a reference from selected text, clipboard as url'
      command '^⌘w : c'
      notes <<-END

      END
    end
  end


  category do
    id 'HTML commands (^⌘e)'
    entry do
      name '="[cursor]"'
      command '^⌘e  ='
      notes <<-END

      END
    end
    entry do
      name 'entity &[cursor];'
      command '^⌘e  e'
      notes <<-END

      END
    end
    entry do
      name 'http://'
      command '^⌘e  /'
      notes <<-END

      END
    end
    entry do
      name 'Make previous word into paired HTML tag'
      command '^⌘e  t'
      notes <<-END

      END
    end
  end

  category do
    id 'HTML Links (a)'
    entry do
      name 'Insert HTML link for selected text, leave cursor in the href with "http://" selected'
      command '^⌘e a t'
      notes <<-END

      END
    end
    entry do
      name 'Insert HTML link with clipboard as href'
      command '^⌘e a c'
      notes <<-END

      END
    end
  end

  category do
    id 'HTML Image (i)'
    entry do
      name 'Insert image tag, any selected text is alt text, leave cursor in src attribute'
      command '^⌘e i t'
      notes <<-END

      END
    end
    entry do
      name 'Insert image tag, clipboard as src, any selected text as alt, leave cursor at beginning of alt attribute'
      command '^⌘e i c'
      notes <<-END

      END
    end
  end

  category do
    id 'Surround commands (^⌘s)'
    entry do
      name 'wrap () with spaces'
      command '^⌘s  ('
      notes <<-END

      END
    end
    entry do
      name 'wrap () no spaces'
      command '^⌘s  )'
      notes <<-END

      END
    end
    entry do
      name 'wrap [] with spaces'
      command '^⌘s  \'
      notes <<-END

      END
    end
    entry do
      name 'wrap [] no spaces'
      command '^⌘s  \'
      notes <<-END

      END
    end
    entry do
      name 'wrap {} with spaces'
      command '^⌘s  {'
      notes <<-END

      END
    end
    entry do
      name 'wrap {} no spaces'
      command '^⌘s  }'
      notes <<-END

      END
    end
    entry do
      name 'wrap <> with spaces'
      command '^⌘s  <'
      notes <<-END

      END
    end
    entry do
      name 'wrap <> no spaces'
      command '^⌘s  >'
      notes <<-END

      END
    end
    entry do
      name 'wrap single quotes'
      command '^⌘s  \''
      notes <<-END

      END
    end
    entry do
      name 'wrap backticks'
      command '^⌘s  `'
      notes <<-END

      END
    end
    entry do
      name 'wrap double quotes'
      command '^⌘s  "'
      notes <<-END

      END
    end
  end

  notes <<-END
    * Created by [ttscoff](https://github.com/ttscoff/KeyBindings).
  END

end
