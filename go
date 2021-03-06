#!/usr/bin/env bash

set -e

npm-script(){
  npm run-script $1
}

benchmark() {
	go clean
	go install
	# go build
	go package
	# go build-optimized
	hyperfine --min-runs 4  \
		'node ./dist/app.js --strategy graph' \
		'node ./dist/app.js --strategy frontier' \
		'stack run -- strategy graph' \
		'stack run -- strategy frontier' 
}


go() {
case $1 in
  help)
	echo "./go (install|build|clean|package|build:optimized|run:optimized|test|run*)"
	;;
  install)
	npm install
	;;
  build)
	npm-script $1
	;;
  run)
	npx spago run
    ;;
  run-strategy2)
	npm-script bundle
	node ./dist/app.js --strategy frontier
	;;
  profile)
	npm-script $1
    ;;

  clean)
	npm-script $1
	;;
  package)
	npm-script bundle
	;;
  build-optimized)
  	npm-script $1
	;;
  run-optimized)
	npm-script $1
	;;	
  profile-optimized)
	npm-script $1
    ;;
  test)
    npx spago test
	;;

  benchmark)
	benchmark
	;;
  *)
	npx spago run
	;;
esac
}

go $@
