# vim-emulation

Vim implemented in [Elm](https://elm-lang.org)

## Features

### Basic Movement

- <kbd>h</kbd> <kbd>l</kbd> <kbd>k</kbd> <kbd>j</kbd> character left, right; line up, down
- <kbd>w</kbd>/<kbd>W</kbd> word/WORD right
- <kbd>b</kbd>/<kbd>B</kbd> word/WORD left
- <kbd>e</kbd>/<kbd>E</kbd> end of word/WORD right
- <kbd>0</kbd> beginning of line
- <kbd>^</kbd> first character of line
- <kbd>$</kbd> last character of line
- <kbd>G</kbd> last line
- <kbd>gg</kbd> first line

### Insertion & replace

- <kbd>i</kbd> <kbd>a</kbd>	insert before, after cursor
- <kbd>I</kbd> <kbd>A</kbd>	insert at beginning, end of line
- <kbd>o</kbd> <kbd>O</kbd>	open a new line below, above the current line
- <kbd><<</kbd> <kbd>>></kbd> shift left, right
- <kbd>ciw</kbd> change text in word

## Further information

- [Vim documentation: help](http://vimdoc.sourceforge.net/htmldoc)
- [Vim Quick Reference Card](http://users.ece.utexas.edu/~adnan/vimqrc.html)
