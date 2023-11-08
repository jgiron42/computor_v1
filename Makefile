NAME = computor

SRCS =	parser.ml \
		lexer.ml \
		mathexpr.ml \
		computor_v1.ml \

INCLUDES =	tree.mli \
			parser.mli

SRCS_DIR = 	srcs

OBJS_DIR = .objs

LIBS = str.cmxa

include template.mk