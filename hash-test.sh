#!/bin/sh -e

fromscm() {
	csi -e "(import (hash)) (display (hash-file \"$0\"))"
}

fromb2sum() {
	../blake2/b2sum/b2sum -l256 $0 | xxd -p -r | base64 | tr '/+' '_-'
}

testf() {
	ref=$(fromb2sum $0)
	scm=$(fromscm $0)
	[ $ref = $scm ] || {
		echo $ref
		echo $scm
		echo "hashes don't match"
		exit 1
	}
	echo "hashes agree with b2sum"
}

testf $0
