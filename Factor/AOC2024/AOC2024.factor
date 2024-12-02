USING: arrays grouping io io.encodings.utf8 io.files kernel math
math.order math.parser math.vectors prettyprint sequences
sorting splitting ;
IN: AOC2024

! Common words
: read-txt ( path -- seq ) utf8 [ read-lines ] with-file-reader ;
: read-input ( n -- seq ) number>string 2 CHAR: 0 pad-head "inputs/" prepend ".txt" append read-txt ;
: split-words-multspace ( seq -- seq ) split-words harvest ;

! Day 1
: appearences ( seq el -- n ) [ = ] curry count ;
: all-appearences ( seq el-seq -- seq ) swap [ swap appearences ] curry map ;
: process-01 ( seq -- seq ) [ split-words-multspace [ string>number ] map ] map flip ;
: solve-01-1 ( seq -- n ) [ sort ] map first2 v- [ abs ] map-sum ;
: solve-01-2 ( seq -- n ) first2 swap [ all-appearences ] keep v* sum ;
: solve-01 ( input -- pair ) process-01 [ solve-01-1 ] [ solve-01-2 ] bi 2array ;
: run-01 ( -- ) 1 read-input solve-01 . ;

! Day 2
: process-02 ( seq -- seq ) [ split-words [ string>number ] map ] map ;
: safe ( seq -- t/f )  2 clump [ first2 - ] map dup first 0 < [ [ neg ] map ] when [ infimum 0 > ] [ supremum 4 < ] bi and ;
: safe-without ( seq n -- t/f ) swap remove-nth safe ;
: damp-safe ( seq -- t/f ) dup length <iota> [ dupd safe-without ] find nip nip ;
: solve-02-1 ( seq -- n ) [ safe ] count ;
: solve-02-2 ( seq -- n ) [ damp-safe ] count ;
: solve-02 ( input -- pair ) process-02 [ solve-02-1 ] [ solve-02-2 ] bi 2array ;
: run-02 ( -- ) 2 read-input solve-02 . ;
