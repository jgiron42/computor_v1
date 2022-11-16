NAME ?= $(shell basename $$(pwd))

SRCS_DIR ?= .

OBJS_DIR ?= obj

INCLUDE_DIR ?= .

SRCS ?= $(wildcard *.c)

OBJS_DIR_TREE := $(sort $(addprefix ${OBJS_DIR}/, $(dir ${SRCS})))

#LIB_ARG = $(foreach path, $(LIBS), -L $(dir $(path)) -l $(notdir $(path)))

#INCLUDE_ARG = $(addprefix -I ,${INCLUDE_DIR}) $(addprefix -I, $(dir ${LIBS}))

OBJS := $(SRCS:%.ml=${OBJS_DIR}/%.cmx)

INTERS := $(SRCS:%.ml=${OBJS_DIR}/%.cmi)

all: $(NAME)

$(NAME): ${OBJS_DIR} ${OBJS_DIR_TREE} $(INTERS) $(OBJS) $(MAKE_DEP)
	ocamlopt -o $@ $(LIBS) $(OBJS)

${OBJS_DIR}:
	mkdir -p $@
	[ -f .gitignore ] && (grep '^'"$@"'$$' .gitignore || echo "$@" >> .gitignore) || true

${OBJS_DIR_TREE}:
	mkdir -p $@

${OBJS_DIR}/%.cmx: ${SRCS_DIR}/%.ml
	ocamlopt -c $< -o $@ -I ${OBJS_DIR}

${OBJS_DIR}/%.cmi: ${SRCS_DIR}/%.ml
	ocamlopt -c $< -o $@ -I ${OBJS_DIR}

$(MAKE_DEP):
	make -C $(dir $@) $(notdir $@)

clean:
	rm -rf $(OBJS_DIR)

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: clean all fclean re

.PRECIOUS: ${OBJS_DIR}/%.d

-include ${IDEPS}