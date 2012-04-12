.PHONY: all doc clean test
REBAR:=$(shell which rebar || echo ./rebar)
REBAR_URL:="https://github.com/downloads/basho/rebar/rebar"

all: $(REBAR)
	@$(REBAR) compile

$(REBAR):
	@echo "No rebar was found so a copy will be downloaded in 5 seconds."
	@echo "Source: ${REBAR_URL} Destination: ${REBAR}"
	@sleep 5
	@echo "Commencing download... "
	@erl -noshell -eval "\
[ application:start(X) || X <- [crypto,public_key,ssl,inets]],\
Request = {\"${REBAR_URL}\", []},\
HttpOpts = [],\
Opts = [{stream, \"$(REBAR)\"}],\
Result = httpc:request(get, Request, HttpOpts, Opts),\
Status = case Result of {ok, _} -> 0; _ -> 1 end,\
init:stop(Status)."
	@chmod u+x ./rebar
	@echo "ok"

doc: $(REBAR)
	@$(REBAR) doc skip_deps=true

clean: $(REBAR)
	@$(REBAR) clean
	@rm -fr doc/*

test: $(REBAR)
	@$(REBAR) eunit
