USING: tools.test AOC2024 kernel arrays sequences ;
IN: AOC2024.tests

: test-example ( output input solver: ( x -- x ) -- ) curry swap 1array swap unit-test ;
: test-input ( output solver: ( x -- x ) n -- ) read-input swap curry unit-test ;

! tests for example inputs

{ 11 31 }
{
"3   4"
"4   3"
"2   5"
"1   3"
"3   9"
"3   3"
}
[ solve-01 ]
test-example

! tests for real inputs, delete if using different ones

{ { 2742123 21328497 } }
[ solve-01 ]
1
test-input
