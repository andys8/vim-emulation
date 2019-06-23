# vim-emulation

Vim implemented in [Elm](https://elm-lang.org)

## Features

### Basic Movement

<kbd>h</kbd> `l` k j	character left, right; line up, down
b w	word/token left, right
ge e	end of word/token left, right
{Â  }	beginning of previous, next paragraph
( )	beginning of previous, next sentence
0 gm	beginning, middle of line
^Â  $	first, last character of line
nG ngg	line n, default the last, first
n%	percentage n of the file (n must be provided)
n|	column n of current line
%	match of next brace, bracket, comment, #define
nH nL	line n from start, bottom of window
M	middle line of window

## Further information

- [Vim documentation: help](http://vimdoc.sourceforge.net/htmldoc)
- [Vim Quick Reference Card](http://users.ece.utexas.edu/~adnan/vimqrc.html)
