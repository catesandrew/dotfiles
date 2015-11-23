# coding: utf-8
cheatsheet do
  title 'Chromium-VIM'
  docset_file_name 'Chromium-VIM'
  keyword 'cvim'

  introduction "Chromium VIM"

  category do
    id 'Keybindings'

    entry do
        name 'center page to current search match (bottom)'
        command 'zb'
        notes <<-'END'
```
centerMatchB
```
    END
    end
    entry do
        name 'center page to current search match (top)'
        command 'zt'
        notes "
```
centerMatchT
```"
    end
    entry do
        name 'center page to current search match (middle)'
        command 'zz'
        notes "
```
centerMatchH
```"
    end
    entry do
        name 'go to the last focused input box by gi'
        command 'gI'
        notes "
```
goToLastInput
```"
    end
    entry do
        name 'go to first input box'
        command 'gi'
        notes "
```
goToInput
```"
    end
    entry do
        name 'reset the scroll focus to the main page'
        command '#'
        notes "
```
resetScrollFocus
```"
    end
    entry do
        name 'scroll to the right of the page'
        command '$'
        notes "
```
scrollToRight
```"
    end
    entry do
        name 'scroll to the left of the page'
        command '0'
        notes "
```
scrollToLeft
```"
    end
    entry do
        name 'scroll to the bottom of the page'
        command 'G'
        notes "
```
scrollToBottom
```"
    end
    entry do
        name 'scroll to the top of the page'
        command 'gg'
        notes "
```
scrollToTop
```"
    end
    entry do
        name 'scroll full-page up'
        command 'unmapped'
        notes "
```
scrollFullPageUp
```"
    end
    entry do
        name 'scroll half-page up'
        command 'u,e'
        notes "
```
scrollPageUp
```"
    end
    entry do
        name 'scroll full-page down'
        command 'unmapped'
        notes "
```
scrollFullPageDown
```"
    end
    entry do
        name 'scroll half-page down'
        command 'd'
        notes "
```
scrollPageDown
```"
    end
    entry do
        name 'scroll right'
        command 'l'
        notes "
```
scrollRight
```"
    end
    entry do
        name 'scroll left'
        command 'h'
        notes "
```
scrollLeft
```"
    end
    entry do
        name 'scroll up'
        command 'k,w'
        notes "
```
scrollUp
```"
    end
    entry do
        name 'scroll down'
        command 'j,s'
        notes "
```
scrollDown
```"
    end
  end

  category do
      id 'Link Hints'

      entry do
          name 'change the link hint focus'
          command ';'
      end

      entry do
          name 'reverse image search (google images)'
          command 'gr'
          notes "
```
reverseImage
```"
      end
      entry do
          name 'copy URL from link to clipboard'
          command 'gy'
          notes "
```
yankUrl
```"
      end
      entry do
          name 'yank multiple links (open the list of links with P)'
          command 'my'
          notes "
```
multiYankUrl
```"
      end
      entry do
          name 'reverse image search multiple links'
          command 'mr'
          notes "
```
multiReverseImage
```"
      end
      entry do
          name 'call a code block with the link as the first argument'
          command 'unmapped'
          notes "
```
createScriptHint(<FUNCTION_NAME>)
```"
      end
      entry do
          name 'edit text with external editor'
          command 'unmapped'
          notes "
```
createEditHint
```"
      end
      entry do
          name 'open multiple links'
          command 'mf'
          notes "
```
createMultiHint
```"
      end
      entry do
          name 'trigger a unhover event (mouseout + mouseleave)'
          command 'Q'
          notes "
```
createUnhoverHint
```"
      end
      entry do
          name 'trigger a hover event (mouseover + mouseenter)'
          command 'q'
          notes "
```
createHoverHint
```"
      end
      entry do
          name 'repeat last hint command'
          command 'A'
          notes "
```
openLastHint
```"
      end
      entry do
          name 'open link in new window'
          command 'W'
          notes "
```
createHintWindow
```"
      end
      entry do
          name 'open link in new tab (active)'
          command 'unmapped'
          notes "
```
createActiveTabbedHint
```"
      end
      entry do
          name 'open link in new tab'
          command 'F'
          notes "
```
createTabbedHint
```"
      end
      entry do
          name 'open link in current tab'
          command 'f'
          notes "
```
createHint
```"
      end

      entry do
          name 'open link in current tab'
          command 'f'
          notes "
```
createHint
```"
      end
  end


  category do
      id 'QuickMarks'

      entry do
          name 'open quickmark <*> in a new tab <N> times'
          command 'gn<*>'
          notes "
```
openQuickMarkTabbed
```"
      end
      entry do
          name 'open quickmark <*> in the current tab'
          command 'go<*>'
          notes "
```
openQuickMark
```"
      end
      entry do
          name 'create quickmark <*>'
          command 'M<*>'
          notes "
```
addQuickMark
```"
      end
  end

  category do
      id 'Miscellaneous'

      entry do
          name 'increment the first number in the URL path'
          command 'g+'
          notes "
```
incrementURLPath
```"
      end
      entry do
          name 'decrement the first number in the URL path'
          command 'g-'
          notes "
```
e.g www.example.com/5 => www.example.com/4, decrementURLPath
```"
      end
      entry do
          name 'close all browser windows'
          command 'unmapped'
          notes "
```
quitChrome
```"
      end
      entry do
          name 'create or toggle a bookmark for the current URL'
          command '<C-b>'
          notes "
```
createBookmark
```"
      end
      entry do
          name 'go to the view-source:// page for the current Url'
          command 'gs'
          notes "
```
:viewsource!
```"
      end
      entry do
          name 'go to to the base URL'
          command 'gU'
          notes "
```
goToRootUrl
```"
      end
      entry do
          name 'go up one path in the URL'
          command 'gu'
          notes "
```
goUpUrl
```"
      end
      entry do
          name 'stop all tabs from loading'
          command 'gQ'
          notes "
```
cancelAllWebRequests
```"
      end
      entry do
          name 'stop the current tab from loading'
          command 'gq'
          notes "
```
cancelWebRequest
```"
      end
      entry do
          name 'go to the root frame'
          command 'gF'
          notes "
```
rootFrame
```"
      end
      entry do
          name 'cycle through iframes'
          command 'gf'
          notes "
```
nextFrame
```"
      end
      entry do
          name 'hide the download shelf'
          command 'gj'
          notes "
```
hideDownloadsShelf
```"
      end
      entry do
          name 'open the clipboard selection in a new tab'
          command 'P'
          notes "
```
openPasteTab
```"
      end
      entry do
          name 'open the clipboard selection'
          command 'p'
          notes "
```
openPaste
```"
      end
      entry do
          name 'search through bookmarks'
          command 'b'
          notes "
```
:bookmarks
```"
      end
      entry do
          name 'copy the currently matched text from find mode (if any)'
          command 'yh'
          notes "
```
yankHighlight
```"
      end
      entry do
          name 'copy the URLs in the current window'
          command 'ya'
          notes "
```
yankWindowUrls
```"
      end
      entry do
          name 'copy the URL of the current frame to the clipboard'
          command 'yY'
          notes "
```
yankRootUrl
```"
      end
      entry do
          name 'copy the URL of the current page to the clipboard'
          command 'yy'
          notes "
```
yankDocumentUrl
```"
      end
      entry do
          name 'alias to :chrome://downloads<CR>'
          command 'gd'
          notes "
```
:chrome://downloads<CR>
```"
      end
      entry do
          name 'toggle image zoom (same as clicking the image on image-only pages)'
          command 'z<Enter>'
          notes "
```
toggleImageZoom
```"
      end
      entry do
          name 'zoom page to original size'
          command 'z0'
          notes "
```
zoomOrig
```"
      end
      entry do
          name 'zoom page out'
          command 'zo'
          notes "
```
zoomPageOut
```"
      end
      entry do
          name 'zoom page in'
          command 'zi'
          notes "
```
zoomPageIn
```"
      end
      entry do
          name 'reload all tabs but current'
          command 'cr'
          notes "
```
reloadAllButCurrent
```"
      end
      entry do
          name 'reload all tabs'
          command 'none'
          notes "
  ```
  reloadAllTabs
```"
      end
      entry do
          name 'go to mark <*>'
          command '\'<*>'
          notes "
```
goToMark
```"
      end
      entry do
          name 'go to last scroll position'
          command '\'\''
          notes "
```
lastScrollPosition
```"
      end
      entry do
          name 'create mark <*>'
          command ';<*>'
          notes "
  ```
  setMark
```"
      end
      entry do
          name 'reload the current tab + local cache'
          command 'gR'
          notes "
  ```
  reloadTabUncached
```"
      end
      entry do
          name 'reload the current tab'
          command 'r'
          notes "
  ```
  reloadTab
```"
      end
      entry do
          name 'enter insert mode (escape to exit)'
          command 'i'
          notes "
  ```
  insertMode
```"
      end
      entry do
          name 'restart Google Chrome'
          command 'zr'
          notes "
```
:chrome://restart<CR>
```"
      end
      entry do
          name 'pass <N> keys through to the current page'
          command '<N> unmapped'
          notes "
```
passKeys
```"
      end
      entry do
          name 'scroll <N> percent down the page'
          command '<N>g%'
          notes "
```
percentScroll
```"
      end
      entry do
          name 'search through browser history'
          command 'I'
          notes "
```
:history
```"
      end
      entry do
          name 'open link search bar '
          command 'unmapped'
          notes "
```
same as pressing `/?`, openLinkSearchBar
```"
      end
      entry do
          name 'open search bar (reverse search)'
          command '?'
          notes "
```
openSearchBarReverse
```"
      end
      entry do
          name 'open search bar'
          command '/'
          notes "
```
openSearchBar
```"
      end
      entry do
          name 'open command bar'
          command ':'
          notes "
```
openCommandBar
```"
      end
      entry do
          name 'repeat the last command'
          command '.'
          notes "
```
repeatCommand
```"
      end
      entry do
          name 'alias to ":tabnew google "'
          command 'a'
          notes "
```
:tabnew google
```"
      end

  end


  category do
    id 'Tab Navigation'

    entry do
        name 'toggle the focus between the last used tabs'
        command '<C-6>'
        notes "
```
lastUsedTab
```"
    end
    entry do
        name 'pin/unpin the current tab'
        command 'gp'
        notes "
```
pinTab
```"
    end
    entry do
        name 'click the "back" link on the page'
        command '[['
        notes "
```
previousMatchPattern
```"
    end
    entry do
        name 'click the "next" link on the page'
        command ']]'
        notes "
```
nextMatchPattern
```"
    end
    entry do
        name 'move current tab right'
        command '>'
        notes "
```
moveTabRight
```"
    end
    entry do
        name 'move current tab left'
        command '<'
        notes "
```
moveTabLeft
```"
    end
    entry do
        name 'search for another active tab'
        command 'B'
        notes "
```
:buffer
```"
    end
    entry do
        name 'go forward'
        command 'L,D'
        notes "
```
goForward
```"
    end
    entry do
        name 'go back'
        command 'H,S'
        notes "
```
goBack
```"
    end
    entry do
        name 'switch to tab <N>'
        command '<N>%'
        notes "
```
goToTab
```"
    end
    entry do
        name ':open <CURRENT URL>'
        command 'O'
        notes "
```
:open @%
```"
    end
    entry do
        name ':tabnew <CURRENT URL>'
        command 'T'
        notes "
```
:tabnew @%
```"
    end
    entry do
        name ':tabnew'
        command 't'
        notes "
```
:tabnew
```"
    end
    entry do
        name 'open the last closed tab'
        command 'X'
        notes "
```
lastClosedTab
```"
    end
    entry do
        name 'close all tabs to the right of the current tab'
        command 'gx$'
        notes "
```
closeTabsToRight
```"
    end
    entry do
        name 'close all tabs to the left of the current tab'
        command 'gx0'
        notes "
  ```
  closeTabsToLeft
```"
    end
    entry do
        name 'close the tab to the right of the current tab'
        command 'gxt'
        notes "
  ```
  closeTabRight
```"
    end
    entry do
        name 'close the tab to the left of the current tab'
        command 'gxT'
        notes "
  ```
  closeTabLeft
```"
    end
    entry do
        name 'close the current tab'
        command 'x'
        notes "
  ```
  closeTab
```"
    end
    entry do
        name 'open the next URL from the current tab\'s history in a new tab'
        command '<C-S-l>,gl'
        notes "
```
openNextLinkInTab
```"
    end
    entry do
        name 'open the last URL in the current tab\'s history in a new tab'
        command '<C-S-h>,gh'
        notes "
```
openLastLinkInTab
```"
    end
    entry do
        name 'go to the first/last tab'
        command 'g0,g$'
        notes "
```
firstTab, lastTab
```"
    end
    entry do
        name 'navigate to the previous tab'
        command 'gT,J,E'
        notes "
```
previousTab
```"
    end
    entry do
        name 'navigate to the next tab'
        command 'gt,K,R'
        notes "
```
nextTab
```"
    end

  end


  category do
    id 'Find Mode'

    entry do
        name 'clear search mode highlighting'
        command 'unmapped'
        notes "
  ```
  clearSearchHighlight
```"
    end
    entry do
        name 'enter visual line mode from caret mode/currently highlighted search'
        command 'V'
        notes "
  ```
  toggleVisualLineMode
```"
    end
    entry do
        name 'enter visual/caret mode '
        command 'v'
        notes "
```
highlight current search/selection, toggleVisualMode
```"
    end
    entry do
        name 'previous search result'
        command 'N'
        notes "
```
previousSearchResult
```"
    end
    entry do
        name 'next search result'
        command 'n'
        notes "
```
nextSearchResult
```"
    end

  end

  category do
    id 'Visual/Caret Mode'

    entry do
        name 'open highlighted text in new tab'
        command 'P'
    end
    entry do
        name 'open highlighted text in current tab'
        command 'p'
    end
    entry do
        name 'select the previous search result'
        command 'N'
    end
    entry do
        name 'select the next search result'
        command 'n'
    end
    entry do
        name 'copys the current selection'
        command 'y'
    end
    entry do
        name 'move the caret position/extend the visual selection'
        command 'h,j,k,l'
    end
    entry do
        name 'toggle between visual/caret mode'
        command 'v'
    end
    entry do
        name 'exit visual mode to caret mode/exit caret mode to normal mode'
        command '<Esc>'
    end
  end


  category do
      id 'Text boxes'

      entry do
          name 'select input text (equivalent to <C-a>)'
          command 'unmapped'
          notes "
```
selectAll
```"
      end
      entry do
          name 'move cursor back one line'
          command '<C-k>'
          notes "
```
backwardLine
```"
      end
      entry do
          name 'move cursor forward one line'
          command '<C-j>'
          notes "
```
forwardLine
```"
      end
      entry do
          name 'move cursor back one letter'
          command '<C-b>'
          notes "
```
backwardChar
```"
      end
      entry do
          name 'move cursor forward one letter'
          command '<C-f>'
          notes "
```
forwardChar
```"
      end
      entry do
          name 'move cursor forward one word'
          command '<C-l>'
          notes "
```
forwardWord
```"
      end
      entry do
          name 'move cursor back one word'
          command '<C-h>'
          notes "
```
backwardWord
```"
      end
      entry do
          name 'delete forward one character'
          command 'unmapped'
          notes "
```
deleteForwardChar
```"
      end
      entry do
          name 'delete back one character'
          command 'unmapped'
          notes "
```
deleteChar
```"
      end
      entry do
          name 'delete forward one word'
          command '<C-p>'
          notes "
```
deleteForwardWord
```"
      end
      entry do
          name 'delete back one word'
          command '<C-y>'
          notes "
```
deleteWord
```"
      end
      entry do
          name 'delete to the end of the line'
          command '<C-o>'
          notes "
```
deleteToEnd
```"
      end
      entry do
          name 'delete to the beginning of the line'
          command '<C-u>'
          notes "
```
deleteToBeginning
```"
      end
      entry do
          name 'move cursor to the end of the line'
          command '<C-e>'
          notes "
```
endOfLine
```"
      end
      entry do
          name 'move cursor to the beginning of the line'
          command '<C-i>'
          notes "
```
beginningOfLine
```"
      end

  end


  category do
    id 'Command Mode'

    entry do
        name 'unpin the current tab'
        command ':unpintab'
    end
    entry do
        name 'pin the current tab'
        command ':pintab'
    end
    entry do
        name 'toggle the pin state of the current tab'
        command ':togglepin'
    end
    entry do
        name 'run JavaScript on the current page'
        command ':script'
    end
    entry do
        name 'open the tabs from a saved session in a new window'
        command ':session (autocomplete)'
    end
    entry do
        name 'delete a saved session'
        command ':delsession (autocomplete)'
    end
    entry do
        name 'create a new session from the current tabs in the active window'
        command ':mksession'
    end
    entry do
        name 'change to a different tab'
        command ':buffer (autocomplete)'
    end
    entry do
        name 'execute a sequence of keys'
        command ':execute'
        notes <<-'END'
Useful for mappings. For example, "map j :execute 2j <CR>"
        END
    end
    entry do
        name 'clear the highlighted text from the last search'
        command ':nohlsearch'
    end
    entry do
        name 'open the settings page'
        command ':settings'
    end
    entry do
        name 'duplicate the current tab'
        command ':duplicate'
    end
    entry do
        name 'load a cVimrc file into memory'
        command ':source (autocomplete)'
        notes <<-'END'
this will overwrite the settings in the options page if the localconfig setting had been set previously
        END
    end
    entry do
        name 'open a local file'
        command ':file (autocomplete)'
    end
    entry do
        name 'move the current tab to a new window'
        command ':tabdetach'
    end
    entry do
        name 'move the current tab to another open window'
        command ':tabattach (autocomplete)'
    end
    entry do
        name 'restore a previously closed tab'
        command ':restore (autocomplete)'
        notes <<-'END'
newer versions of Chrome only
        END
    end
    entry do
        name 'close the current window'
        command ':qall'
    end
    entry do
        name 'close the current tab'
        command ':quit'
    end
    entry do
        name 'aliases : <NAME> to : <ACTION>'
        command ':command <NAME> <ACTION>'
    end
    entry do
        name 'browse the different history states of the current tab'
        command ':tabhistory (autocomplete)'
    end
    entry do
        name 'open a chrome:// URL'
        command ':chrome:// (autocomplete)'
    end
    entry do
        name 'temporarily change a cVim setting'
        command ':set (autocomplete)'
    end
    entry do
        name 'browse bookmarks by folder/open all bookmarks from folder'
        command ':bookmarks /<folder> (autocomplete)'
    end
    entry do
        name 'search through bookmarks'
        command ':bookmarks (autocomplete)'
    end
    entry do
        name 'search through browser history'
        command ':history (autocomplete)'
    end
    entry do
        name 'open the typed/completed URL/google search'
        command ':open (autocomplete)'
    end
    entry do
        name 'open a new window with the typed/completed search'
        command ':new (autocomplete)'
    end
    entry do
        name 'open a new tab with the typed/completed search'
        command ':tabnew (autocomplete)'
    end

  end

  notes <<-END
    * Created by [andrew](https://github.com/catesandrew).
  END

end
