BEGIN {
  print "@Tbl"
  print "  mv { 0.5vx }"
  print "  aformat { @Cell ml { 0i } indent { right } @Code A | @Cell B | @Cell |"
  print "            @Cell           indent { right } @Code C | @Cell D | @Cell |"
  print "            @Cell           indent { right } @Code E | @Cell mr { 0i } F }"
  print "{"
}
NR % 3 == 1 { printf "@Rowa\n"
              printf "  A { \"%s from { a } to { b }\" }\n", $1
              printf "  B { @Math { %s from { a } to { b } } }\n", $1 }
NR % 3 == 2 { printf "  C { \"%s from { a } to { b }\" }\n", $1
              printf "  D { @Math { %s from { a } to { b } } }\n", $1 }
NR % 3 == 0 { printf "  E { \"%s from { a } to { b }\" }\n", $1
              printf "  F { @Math { %s from { a } to { b } } }\n", $1 }
END { print "}" }
