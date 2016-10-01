ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM) test

test:
	./run.sh server_not_centralized test

benchmark: $(BEAM)
	./run_dec_benchmark.sh

clean:
	rm -f *.beam erl_crash.dump

%.beam: %.erl
	erlc +debug_info $*.erl
