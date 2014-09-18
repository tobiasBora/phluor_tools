# This Makefile is aimed to compile the whole project
# ===============================================
# You can edit this part but usually it's useless
# ===============================================

# Here is the list of all bricks loaded
BRICKS_LIST=$(shell cat bricks_included.txt)

include package/info.dico
# Configure server conf 
include server_conf.test.dico

# ========================================
# Shoudn't be edited by user
# ========================================

include Makefile.conf

PWD=$(shell pwd)

# The tree :
all: compil_bricks client_part/main_js.js new_www update_static update_client_part update_server_part update_config

# Compile each dependance
compil_bricks:
	@echo "#################################"
	@echo "########## BUILDING... ##########"
	@echo "#################################"
	$(foreach brick, $(BRICKS_LIST), cd src/$(brick) && make && cd $(PWD) &&) true

client_part/main_js.js: compil_bricks _dep _dep_lib
	mkdir -p client_part/bricks/
# We create a js file from all cmo files
	$(JS_OF_ELIOM) -o client_part/main_js.js $(foreach brick_cmo, $(shell cat _dep_lib), $(brick_cmo))

# Create a new www/ folder and save the older
new_www:
	@echo "A saving of the old version has been put in .www_bak"
# Save
	rm -rf .www_bak/
	mv www/ .www_bak/ > /dev/null 2>&1; exit 0 # Does a proper way exists ?
# Copy
	mkdir -p www/modules
	mkdir -p www/bin_js
	mkdir -p www/static/bricks/
	mkdir -p www/static/phluoruser/
	ln -s var/ www/static/var
	mkdir -p www/config
	mkdir -p www/local/var/log/$(PROJ_NAME)/
	mkdir -p www/local/var/data/$(PROJ_NAME)/
	mkdir -p www/local/var/run/

# TODO : make only one
www:
	mkdir -p www/modules
	mkdir -p www/bin_js
	mkdir -p www/static/bricks/
	mkdir -p www/static/phluoruser/
	mkdir -p www/config
	mkdir -p www/local/var/log/$(PROJ_NAME)/
	mkdir -p www/local/var/data/$(PROJ_NAME)/
	mkdir -p www/local/var/run/

### STATIC
# Maybe link could be better if it's in a subfolder ?
update_static: _dep _dep_lib | www/
	cp -r static/* www/static/phluoruser/
	$(foreach brick, $(shell cat _dep), echo "copying static $(brick)..."; mkdir -p www/static/bricks/$(brick); $(PHLUOR_TOOLS) copy src/$(brick)/static/ www/static/bricks/$(brick)/ --copy_inside --dico_content src/$(brick)/package/name.dico --avoid_error;) exit 0


### CLIENT PART
www/static/js/main_js.js: client_part/main_js.js | www/
	mkdir -p www/static/js
	cp client_part/main_js.js www/static/js/
update_client_part: www/static/js/main_js.js

### SERVER PART : JUST COPY THE BRICKS, make sure to compile them before
update_server_part: _dep _dep_lib | www/
	$(foreach brick, $(shell cat _dep), echo "copying $(brick)..."; mkdir -p www/modules/$(brick); cp -r src/$(brick)/_exec/* www/modules/$(brick)/;) exit 0

### RUNSERVER CONFIGURATION
# This is a little function to avoid typing to much text
## This next lines insert the bricks conf in run_server.conf.
# The order of include is
# - Include files of src/<brick name>/config/
# - Include files of config/<brick name> <--- Make sure you know what you are doing, don't create files already present in src/.../config because if would create a new entry
# - Include text from dico src/<brick name>/package/name.dico (only BRICK_NAME)
# - Include text from dico config/<brick name>/main.conf
# - Include text from dico src/<brick name>/config/main.conf
config_brick = echo $(1) && $(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --inc --keep_inc --prefix src/$(1)/config/ --avoid_error && $(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --inc --keep_inc --prefix config/$(1)/ --avoid_error && $(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --dico_content src/$(2)/package/name.dico --avoid_error && $(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --dico_content config/$(1)/main.dico --avoid_error && $(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --dico_content src/$(1)/config/main.dico --avoid_error

update_config: _dep _dep_lib | www/
	@echo "--- Server configuration..."
	cp run_server.conf www/run_server.conf
# If the brick is only the service part of another brick (presence of
# a file in package/new_brick_conf.txt containing the parent brick),
# the configuration first goes in the service brick, and then in the
# non service one.
	$(foreach brick, $(shell cat _dep), if [ -s "src/$(brick)/package/new_brick_conf.txt" ]; then $(call config_brick,$$(cat "src/$(brick)/package/new_brick_conf.txt"),$(brick)); else $(call config_brick,$(brick),$(brick)); fi && ) true
	$(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --dico_content server_conf.test.dico --avoid_error
# Remove all resident files.
	$(PHLUOR_TOOLS) copy www/run_server.conf www/run_server.conf --rem_only --avoid_error


# Copy each brick in the www/ folder
# copy_bricks: compil_bricks _dep _dep_lib
# @echo "#################################"
# @echo "########## COPYING...  ##########"
# @echo "#################################"

_dep: $(foreach brick, $(shell cat bricks_included.txt), src/$(brick)/_dep)
	@echo "####################################################"
	@echo "########## Listing brick dependencies...  ##########"
	@echo "####################################################"
	echo "" > _dep_no_uniq
	$(foreach brick, $(BRICKS_LIST), echo "$(brick)" >> _dep_no_uniq && cat src/$(brick)/_dep >> _dep_no_uniq &&) true
	ocaml reverse_uniq.ml _dep_no_uniq _dep

_dep_lib: $(foreach brick, $(shell cat _dep), src/$(brick)/_dep_lib)
	rm -rf _dep_lib
	$(foreach brick, $(shell cat _dep), cat src/$(brick)/_dep_lib >> _dep_lib &&) true



run:
	@echo "#################################"
	@echo "########## Running... ##########"
	@echo "#################################"
	@echo "The website is available in port $(PORT) (for example http://localhost:$(PORT)/ )"
	@echo "---------------------------------"
	cd www/; ocsigenserver -c run_server.conf -v

# Compil and run
r: all run

clean:
# with this make clean_all, several libraries may be clean several times,
# and an infinite loop could appear if the inclusion is cyclic, but a simple
# clean could forgot some bricks if _dep is bad due to a wrong configuration
# so this could avoid lot's of problems.
	$(foreach brick, $(shell cat bricks_included.txt) $(shell cat _dep), echo "--- Cleaning $(brick)"; cd src/$(brick)/; make clean_all; cd $(PWD);)
	rm -rf www/ _dep _dep_lib

clean_only_current:
	rm -rf www/ _dep _dep_lib

PHONY: all compil_bricks copy_bricks run r clean clean_all update_static update_client_part update_server_part update_config
