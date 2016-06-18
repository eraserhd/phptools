
SUBDIRS				= tests

TESTS				= lalrtest rgtest agtest

noinst_PROGRAMS			= $(TESTS)

bin_PROGRAMS			= phptools

rgtest_SOURCES			= rgtest.scm php-rg.scm testlib.scm

lalrtest_SOURCES		= lalrtest.scm php-lalr.scm php-rg.scm\
				  testlib.scm

agtest_SOURCES			= agtest.scm php-rg.scm php-lalr.scm\
				  analyze-globals.scm testlib.scm

phptools_SOURCES		= phptools.scm php-lalr.scm php-rg.scm\
				  analyze-globals.scm

EXTRA_DIST			= scm-header

SUFFIXES			= .scm .o

.scm.o:
	@BIGLOO@ -c -I $(srcdir) -o $@ $<

test: check-TESTS
