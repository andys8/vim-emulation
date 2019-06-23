# vim-emulation

Vim implemented in [Elm](https://elm-lang.org)

## Features

### Basic Movement

- <kbd>h</kbd> <kbd>l</kbd> <kbd>k</kbd> <kbd>j</kbd>	character left, right; line up, down
- <kbd>b</kbd> <kbd>w</kbd> word left, right
- <kbd>B</kbd> <kbd>W</kbd> WORD left, right
- <kbd>e</kbd> end of word right
- <kbd>E</kbd> end of WORD right
- <kbd>0</kbd> beginning of line
- <kbd>^</kbd> first character of line
- <kbd>$</kbd> last character of line
- <kbd>G</kbd> last line
- <kbd>gg</kbd> first line

### TODO

<kbd>ge</kbd> end of word right
<kbd>n%</kbd> percentage n of the file (n must be provided)
<kbd>%</kbd> match of next brace, bracket, comment
nH nL	line n from start, bottom of window
M	middle line of window
{ }	beginning of previous, next paragraph
( )	beginning of previous, next sentence
<kbd>gm</kbd> middle of line

## Further information

- [Vim documentation: help](http://vimdoc.sourceforge.net/htmldoc)
- [Vim Quick Reference Card](http://users.ece.utexas.edu/~adnan/vimqrc.html)
