ERLSOURCES = $(notdir $(wildcard esrc/*.erl))
BEAMS = $(patsubst %.erl,ebin/%.beam,$(ERLSOURCES))

PROXYCLASSES = $(wildcard eqcProxy/*.java)

all: bin/classes ebin $(BEAMS) DomesticRobot.mas2j bin/classes/eqcProxy/eqcProxy.class bin/classes/time/check.class 

# .PHONY: bin/classes/eqcProxy/eqcProxy.class bin/classes/time/check.class

JINTERFACE_PRIV_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:priv_dir(jinterface)]).")

JAVAERLANG_PRIV_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:priv_dir(java_erlang)]).")

bin/classes/eqcProxy/eqcProxy.class: $(PROXYCLASSES)
	 javac -d bin/classes -cp ../../build/libs/jason-2.3.jar:$(JAVAERLANG_PRIV_DIR)/JavaErlang.jar:$(JINTERFACE_PRIV_DIR)/OtpErlang.jar eqcProxy/*.java

bin/classes/time/check.class: time/*.java
	 javac -d bin/classes -cp ../../build/libs/jason-2.3.jar:$(JAVAERLANG_PRIV_DIR)/JavaErlang.jar:$(JINTERFACE_PRIV_DIR)/OtpErlang.jar time/*.java

DomesticRobot.mas2j: DomesticRobot_masj2
	sed -e "s|JavaErlang.jar|$(JAVAERLANG_PRIV_DIR)/JavaErlang.jar|g" DomesticRobot_masj2 > DomesticRobot.mas2j
	sed -i "s|OtpErlang.jar|$(JINTERFACE_PRIV_DIR)/OtpErlang.jar|g" DomesticRobot.mas2j

bin/classes:
	mkdir -p bin
	mkdir -p bin/classes

ebin:
	mkdir -p ebin

ebin/%.beam: esrc/%.erl
	erlc +debug_info $(EFLAGS) -o ebin $<

