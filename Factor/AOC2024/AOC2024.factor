USING: arrays grouping io io.encodings.utf8 io.files kernel math
math.parser math.vectors prettyprint ranges sequences sets
sorting splitting ;
IN: AOC2024

! Common words
: read-txt ( path -- seq ) utf8 [ read-lines ] with-file-reader ;
: pad-00 ( str -- str ) 2 CHAR: 0 pad-head ;
: input-path ( str -- str ) "inputs/" prepend ".txt" append ;
: read-input ( n -- seq ) number>string pad-00 input-path read-txt ;
: split-words-multspace ( seq -- seq ) split-words harvest ;
: to-ints ( str -- seq ) split-words-multspace [ string>number ] map ;

! Day 1
: appearences ( seq el -- n ) [ = ] curry count ;
: all-appearences ( seq el-seq -- seq ) swap [ swap appearences ] curry map ;
: process-01 ( seq -- seq ) [ to-ints ] map flip ;
: solve-01-1 ( seq -- n ) [ sort ] map first2 v- l1-norm ;
: solve-01-2 ( seq -- n ) first2 swap [ all-appearences ] keep v* sum ;
: solve-01 ( input -- pair ) process-01 [ solve-01-1 ] [ solve-01-2 ] bi 2array ;
: run-01 ( -- ) 1 read-input solve-01 . ;

! Day 2
: process-02 ( seq -- seq ) [ to-ints ] map ;
: steps ( seq -- seq ) 2 clump [ first2 - ] map ;
: first-sign ( seq -- seq ) dup first 0 < [ [ neg ] map ] when ;
: in-range? ( seq inf sup -- t/f ) [a..b] diff empty? ;
: safe? ( seq -- t/f ) steps first-sign 1 3 in-range? ;
: safe-without? ( seq n -- t/f ) swap remove-nth safe? ;
: damp-safe? ( seq -- t/f ) dup length <iota> [ dupd safe-without? ] find 2nip ;
: solve-02-1 ( seq -- n ) [ safe? ] count ;
: solve-02-2 ( seq -- n ) [ damp-safe? ] count ;
: solve-02 ( input -- pair ) process-02 [ solve-02-1 ] [ solve-02-2 ] bi 2array ;
: run-02 ( -- ) 2 read-input solve-02 . ;
