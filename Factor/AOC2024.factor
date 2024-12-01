USING: io.encodings.utf8 math.parser ;
IN: AOC2024

! Common words
: read-txt ( path -- seq ) utf8 [ read-lines ] with-file-reader ;
: read-input ( n -- seq ) number>string 2 CHAR: 0 pad-head "inputs/" prepend ".txt" append read-txt ;
: split-words-multspace ( seq -- seq ) split-words harvest ;

! Day 1
: process-01 ( seq -- seq ) [ split-words-multspace [ string>number ] map ] map flip ;
: run-01-1 ( seq -- n ) [ sort ] map first2 v- [ abs ] map-sum ;
: run-01-2 ( seq -- n ) drop 0 ;
: run-01 ( -- ) 1 read-input process-01 [ run-01-1 . ] [ run-01-2 . ] bi ;
