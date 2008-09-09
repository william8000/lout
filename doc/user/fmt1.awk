BEGIN {
  print "@Tbl"
  print "  mv { 0.5vx }"
  print "  aformat { @Cell ml { 0i } indent { right } @Code A | @Cell B | @Cell |"
  print "            @Cell           indent { right } @Code C | @Cell D | @Cell |"
  print "            @Cell           indent { right } @Code E | @Cell mr { 0i } F }"
  print "{"
}
NR % 3 == 1 { printf "@Rowa\n  A { \"%s\" } B { @Math { %s } }\n", $1, $1 }
NR % 3 == 2 { printf "  C { \"%s\" } D { @Math { %s } }\n", $1, $1 }
NR % 3 == 0 { printf "  E { \"%s\" } F { @Math { %s } }\n", $1, $1 }
END { print "}" }
