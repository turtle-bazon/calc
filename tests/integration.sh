#!/usr/bin/env bash
set -euo pipefail

CALC="${CALC:-build/calc}"
PASS=0
FAIL=0
TOTAL=0

run_test() {
  local name="$1"
  local input="$2"
  local expected="$3"
  TOTAL=$((TOTAL + 1))
  local actual
  actual=$(echo "$input" | "$CALC" 2>&1 | grep "^= " | head -1 | sed 's/^= //')
  if [ "$actual" = "$expected" ]; then
    PASS=$((PASS + 1))
    echo "  PASS: $name"
  else
    FAIL=$((FAIL + 1))
    echo "  FAIL: $name (expected='$expected', got='$actual')"
  fi
}

run_test_last() {
  local name="$1"
  local input="$2"
  local expected="$3"
  TOTAL=$((TOTAL + 1))
  local actual
  actual=$(echo "$input" | "$CALC" 2>&1 | grep "^= " | tail -1 | sed 's/^= //')
  if [ "$actual" = "$expected" ]; then
    PASS=$((PASS + 1))
    echo "  PASS: $name"
  else
    FAIL=$((FAIL + 1))
    echo "  FAIL: $name (expected='$expected', got='$actual')"
  fi
}

run_test_contains() {
  local name="$1"
  local input="$2"
  local expected="$3"
  TOTAL=$((TOTAL + 1))
  local actual
  actual=$(echo "$input" | "$CALC" 2>&1 | grep "^= " | head -1 | sed 's/^= //')
  if echo "$actual" | grep -q "$expected"; then
    PASS=$((PASS + 1))
    echo "  PASS: $name"
  else
    FAIL=$((FAIL + 1))
    echo "  FAIL: $name (expected to contain='$expected', got='$actual')"
  fi
}

echo "=== Arithmetic ==="
run_test "addition"       "2 3 +"        "5"
run_test "subtraction"    "10 3 -"       "7"
run_test "multiplication" "4 5 *"        "20"
run_test "division"       "10 2 /"       "5"
run_test "exponent"       "2 10 ^"       "1024"
run_test "modulus"        "10 3 mod"     "1"
run_test "negation"       "5 neg"        "-5"
run_test "absolute"       "-7 abs"       "7"
run_test "nested"         "2 3 + 4 *"    "20"

echo ""
echo "=== Trig / Math ==="
run_test_contains "sin 0"          "0 sin"        "0"
run_test_contains "cos 0"          "0 cos"        "1"
run_test_contains "sqrt 9"         "9 sqrt"       "3"
run_test_contains "log e"          "E log"        "0.999"
run_test_contains "exp 1"          "1 exp"        "2.718"
run_test "round 4.6"      "5 round"      "5"
run_test "floor 4.6"      "4 floor"      "4"
run_test "ceil 4.2"       "5 ceil"       "5"
run_test "min"            "3 7 min"      "3"
run_test "max"            "3 7 max"      "7"
run_test "gcd"            "12 8 gcd"     "4"
run_test "lcm"            "4 6 lcm"      "12"

echo ""
echo "=== Comparisons ==="
run_test "5 > 3"          "5 3 >"        "T"
run_test "3 < 5"          "3 5 <"        "T"
run_test "3 = 3"          "3 3 ="        "T"
run_test "5 >= 5"         "5 5 >="       "T"
run_test "3 <= 5"         "3 5 <="       "T"
run_test "3 != 5"         "3 5 !="       "T"
run_test "3 > 5 false"    "3 5 >"        "NIL"
run_test "5 < 3 false"    "5 3 <"        "NIL"

echo ""
echo "=== Stack Operations ==="
run_test "dup"            "5 dup"        "5"
run_test "swap sub"       "1 2 swap -"   "1"
run_test "drop"           "5 drop 3"     "3"
run_test "over"           "5 3 over -"   "-2"
run_test "nip"            "1 2 nip"      "2"
run_test "rot"            "1 2 3 rot"    "1"
run_test "dup add"        "3 dup +"      "6"

echo ""
echo "=== Bitwise / Logic ==="
run_test "logand"         "5 3 logand"   "1"
run_test "logior"         "5 3 logior"   "7"
run_test "logxor"         "5 3 logxor"   "6"
run_test "shl"            "1 3 shl"      "8"
run_test "shr"            "8 2 shr"      "2"
run_test "bitnot"         "0 bitnot"     "-1"

echo ""
echo "=== Base Conversion ==="
run_test "hex 255"        "255 hex"      "FF"
run_test "bin 10"         "10 bin"       "1010"
run_test "dec"            "42 dec"       "42"

echo ""
echo "=== Factorial ==="
run_test "factorial 5"    "5 !"          "120"
run_test "factorial 0"    "0 !"          "1"
run_test "factorial 1"    "1 !"          "1"

echo ""
echo "=== Constants ==="
run_test_contains "pi"             "PI"           "3.141"
run_test_contains "e"              "E"            "2.718"
run_test "true"           "TRUE"         "T"
run_test "false"          "FALSE"        "NIL"
run_test "nil"            "NIL"          "NIL"

echo ""
echo "=== Variables ==="
run_test "assign"         "X = 42"       "42"
run_test_last "use variable"   "X = 10; X 5 +" "15"
run_test_last "overwrite"      "X = 1; X = 99; X" "99"

echo ""
echo "=== Chained expressions ==="
run_test_last "semicolon adds" "1 2 +; 3 4 +" "7"
run_test_last "semicolon mul"  "2 3 *; 4 5 *" "20"

echo ""
echo "=== Multi-line input ==="
TOTAL=$((TOTAL + 1))
result=$(printf "1 2 +\n3 4 +\n" | "$CALC" 2>&1 | grep "^= " | tail -1 | sed 's/^= //')
if [ "$result" = "7" ]; then
  PASS=$((PASS + 1))
  echo "  PASS: multi-line last result"
else
  FAIL=$((FAIL + 1))
  echo "  FAIL: multi-line last result (expected='7', got='$result')"
fi

echo ""
echo "=== Special commands continue session ==="
TOTAL=$((TOTAL + 1))
result=$(printf 'help\nvariables\nfunctions\nX = 5\nX\nquit\n' | "$CALC" 2>&1)
if echo "$result" | grep -q "Special commands:" && \
   echo "$result" | grep -q "No variables defined." && \
   echo "$result" | grep -q "No functions defined." && \
   echo "$result" | grep -q "= 5"; then
  PASS=$((PASS + 1))
  echo "  PASS: help/variables/functions print without exiting"
else
  FAIL=$((FAIL + 1))
  echo "  FAIL: special commands (got='$result')"
fi

echo ""
echo "=== Input handling ==="
TOTAL=$((TOTAL + 1))
result=$(echo "quit" | "$CALC" 2>&1)
if [ -z "$result" ]; then
  PASS=$((PASS + 1))
  echo "  PASS: quit exits cleanly (no output)"
else
  FAIL=$((FAIL + 1))
  echo "  FAIL: quit (expected no output, got='$result')"
fi

TOTAL=$((TOTAL + 1))
result=$(echo "" | "$CALC" 2>&1)
if [ -z "$result" ]; then
  PASS=$((PASS + 1))
  echo "  PASS: empty input produces no output"
else
  FAIL=$((FAIL + 1))
  echo "  FAIL: empty input (expected no output, got='$result')"
fi

echo ""
echo "================================="
echo "Results: $PASS/$TOTAL passed, $FAIL failed"
echo "================================="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
