.PHONY:all makeapp clean debug local

define erl_fun
	erl -noinput -pa ./ebin -eval "case catch makeapp:make([$1,{outdir, \"ebin\"}]) of up_to_date -> halt(0); error -> halt(1) end."
endef

RELEASE_MACRO={d,'LINUX'},{d,'RELEASE_MODE'}
DEBUG_MACRO={d,'LINUX'},{d,'DEBUG_MODE'}
LOCAL={d,'LINUX'},{d,'DEBUG_MODE'},{d,'LOCAL_MODE'}

all: makeapp
	$(call erl_fun,$(RELEASE_MACRO))

debug: makeapp
	$(call erl_fun,$(DEBUG_MACRO))

local: makeapp
	$(call erl_fun,$(LOCAL))

makeapp:
	erl -noinput -eval "case make:files([\"src/kernel/makeapp.erl\"],[{outdir, \"ebin\"}]) of error -> halt(1); _ -> halt(0) end"

clean:
	(rm -f ./ebin/*.beam)
	(rm -f *.dump)