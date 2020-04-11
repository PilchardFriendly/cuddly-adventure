#!/usr/bin/env bash

set -e

npm-script(){
  npm run-script $1
}

benchmark() {
	go clean
	go install
	go build
	go build-optimized
	hyperfine './go run' 'go run-optimized'
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
  profile)
	npm-script $1
    ;;

  clean)
	npm-script $1
	;;
  package)
	npm-script $1
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
