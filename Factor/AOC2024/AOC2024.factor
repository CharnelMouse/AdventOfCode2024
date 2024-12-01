USING: io io.encodings.utf8 io.files kernel math math.parser
math.vectors prettyprint sequences sorting splitting ;
IN: AOC2024

! Common words
: read-txt ( path -- seq ) utf8 [ read-lines ] with-file-reader ;
: read-input ( n -- seq ) number>string 2 CHAR: 0 pad-head "inputs/" prepend ".txt" append read-txt ;
: split-words-multspace ( seq -- seq ) split-words harvest ;

! Day 1
: appearences ( seq el -- n ) [ = ] curry count ;
: all-appearences ( seq el-seq -- seq ) swap [ swap appearences ] curry map ;
: process-01 ( seq -- seq ) [ split-words-multspace [ string>number ] map ] map flip ;
: run-01-1 ( seq -- n ) [ sort ] map first2 v- [ abs ] map-sum ;
: run-01-2 ( seq -- n ) first2 swap [ all-appearences ] keep v* sum ;
: run-01 ( -- ) 1 read-input process-01 [ run-01-1 . ] [ run-01-2 . ] bi ;
