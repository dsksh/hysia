MAKE = make
MAKE_DIRS = src_cpp src_ocaml

default:
	@for subdir in $(MAKE_DIRS) ; do \
        (cd $$subdir && $(MAKE)) ;\
    done

clean:
	@for subdir in $(MAKE_DIRS) ; do \
        (cd $$subdir && $(MAKE) clean) ;\
    done
