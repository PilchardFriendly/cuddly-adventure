#!/usr/bin/env bash

npm-script(){
  npm run-script $1
}

case $1 in
  install)
	npm install
	;;
  build)
	npm-script $1
	;;
  clean)
	npm-script $1
	;;
  package)
	npm-script $1
	;;
  test)
    npx spago test
	;;
  run)
	npx spago run
        ;;
  benchmark)
	time bash -c -p './go run'
	;;
  *)
	npx spago run
	;;
esac
