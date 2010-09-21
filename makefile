#
# This file is part of FactorCSS
# Licensed under the MIT License,
#                http://www.opensource.org/licenses/mit-license
# Copyright 2004 James Bursa <james@semichrome.net>
#

factorcss: Tokeniser.hs Parser.hs CSS.hs FactorCSS.hs
	ghc -o $@ --make FactorCSS.hs

Tokeniser.hs: Tokeniser.x
	alex -g $<

Parser.hs: Parser.y
	happy -g $<

clean:
	-rm *.o *.hi Tokeniser.hs Parser.hs factorcss
