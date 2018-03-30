# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/03/29 23:46:00 by snadaras          #+#    #+#              #
#    Updated: 2018/03/29 23:46:05 by snadaras         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #


NAME = lucipher

SOURCES = uncipher.ml cipher.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = graphics.cmxa

all: depend $(NAME)


SRC = $(SOURCES)

OBJS = $(SRC:%.ml=%.cmo)

OPTOBJS = $(SRC:%.ml=%.cmx)

$(NAME): $(OPTOBJS)
	$(CAMLOPT) $(LIBS:.cma=.cmxa) $(OPTOBJS) -o $(NAME)


$(OBJS): %.cmo : %.ml
	$(CAMLC) $(LIBS)  -c $<

$(OPTOBJS): %.cmx :%.ml
	$(CAMLOPT) $(LIBS)  -c $<

clean:
	rm -f *.cm[iox]
	rm -f $(NAME).o

fclean: clean
	rm -rf 
	rm -f $(NAME)
	rm -f .depend

depend: .depend
	$(CAMLDEP) $(SOURCES) >.depend

re: fclean all

$(shell touch .depend)
include .depend