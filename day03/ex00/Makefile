NAME = ft_graphics
SOURCES = ft_graphics.ml

CAMLC = ocamlc.opt
CAMLOPT = ocamlopt.opt
CAMLDEP = ocamldep

LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = graphics.cma -cclib -lGraphics

all: depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)
export DISPLAY ":0"

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $@ $(LIBS) $^

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox]
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

depend: .depend

re: fclean all

include .depend
