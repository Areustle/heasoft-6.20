#!/bin/tcsh -f

if ( $# == 1 ) then
	set single=t
	goto $1
	exit
endif

comtst:
cat <<EOF
======================================
testing 'comtst'
======================================
EOF
./comtst <<EOF
one 1 two 2 three 3 four 4 five T
one 1 two 2 three 3.0 four 4.0 five F
ok
EOF

if ( $?single == 1 ) exit

clitest:
cat <<EOF
======================================
testing 'clitest'
======================================
EOF
./clitest <<EOF



















1

Y
q
EOF

if ( $?single == 1 ) exit

clitestc:
cat <<EOF
======================================
testing 'clitestc'
======================================
EOF
./clitestc <<EOF



















1

Y
q
EOF

if ( $?single == 1 ) exit

test-arg:
cat <<EOF
======================================
testing 'test-arg'
======================================
EOF
./txtrd <<EOF
@ test-arg.com
quit
EOF

if ( $?single == 1 ) exit

test-concat:
cat <<EOF
======================================
testing 'test-concat'
======================================
EOF
./txtrd <<EOF
@ test-concat.com
quit
EOF

if ( $?single == 1 ) exit

test-if:
cat <<EOF
======================================
testing 'test-if'
======================================
EOF
./txtrd <<EOF
@ test-if.com
quit
EOF

if ( $?single == 1 ) exit

test-label:
cat <<EOF
======================================
testing 'test-label'
======================================
EOF
./txtrd <<EOF
@ test-label.com
quit
EOF

if ( $?single == 1 ) exit

test-lf:
cat <<EOF
======================================
testing 'test-lf'
======================================
EOF
./txtrd <<EOF
@ test-lf.com
quit
EOF

if ( $?single == 1 ) exit

test-loop:
cat <<EOF
======================================
testing 'test-loop'
======================================
EOF
./txtrd <<EOF
@ test-loop.com
quit
EOF

if ( $?single == 1 ) exit

test-return:
cat <<EOF
======================================
testing 'test-return'
======================================
EOF
./txtrd <<EOF
@answer=?return(yes/no) ?
yes
@return_value=
@return_value=?value ?
20
@. return \$(return_value)
quit
EOF

if ( $?single == 1 ) exit

test-sub:
cat <<EOF
======================================
testing 'test-sub'
======================================
EOF
./txtrd <<EOF
@ test-sub.com
quit
EOF

if ( $?single == 1 ) exit

test-symbol:
cat <<EOF
======================================
testing 'test-symbol'
======================================
EOF
./txtrd <<EOF
@ test-symbol.com
quit
EOF
