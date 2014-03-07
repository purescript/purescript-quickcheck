all: lib test

lib:
	mkdir -p js/Test/
	psc src/Test/QuickCheck.purs.hs \
	  -o js/Test/QuickCheck.js \
	  -e js/Test/QuickCheck.e.purs.hs \
	  --module Test.QuickCheck --tco --magic-do

test:
	mkdir -p js/
	psc src/Test/QuickCheck.purs.hs examples/Examples.purs.hs \
	  -o js/Examples.js \
	  --main --module Main --tco --magic-do
	psc src/Test/QuickCheck.purs.hs examples/Prelude.purs.hs \
	  -o js/Prelude.js\
	  --main --module Main --tco --magic-do

