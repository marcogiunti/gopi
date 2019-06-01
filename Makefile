###########################################################################
#                               gopi                                      #
#                                                                         #
#  Copyright 2019 Marco Giunti. All rights reserved. This file is         #
#  distributed under the terms of the GNU Public License version 3.0.     #
#  This software is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          #
#  GNU General Public License for more details.                           #
#                                                                         #
###########################################################################

.PHONY: all clean native count

OCB_FLAGS = -r -use-ocamlfind -use-menhir
SRC = src
OCB = ocamlbuild $(OCB_FLAGS) -I $(SRC)

main: native
	cp main.native gopi

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

sanity:
	@which menhir > /dev/null || (echo "Please run 'opam install menhir'" && false)

clean:
	$(OCB) -clean
	rm -f gopi
