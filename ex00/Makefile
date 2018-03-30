# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/03/29 23:44:20 by snadaras          #+#    #+#              #
#    Updated: 2018/03/30 19:59:49 by snadaras         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME = ft_graphics

SOURCES = ft_graphics.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = graphics.cmxa


all: depend $(NAME)

#$(NAME): opt byt
#	ln -fs $(NAME).opt $(NAME)

#opt: $(NAME).opt
#byt: $(NAME).byt

SRC = $(SOURCES)

OBJS = $(SRC:%.ml=%.cmo)

OPTOBJS = $(SRC:%.ml=%.cmx)

$(NAME): $(OPTOBJS)
	$(CAMLOPT) $(LIBS:.cma=.cmxa) $(OPTOBJS) -o $(NAME)

#$(NAME).byt: $(OBJS)
#	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

#$(NAME).opt: $(OPTOBJS)
#	$(CAMLOPT) $(LIBS:.cma=.cmxa) $(OPTOBJS) -o $(NAME) 

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
