# This Makefile is aimed to compile the whole project
# ===============================================
# You can edit this part but usually it's useless
# ===============================================

# Here is the list of all bricks loaded
BRICKS_LIST ?= $(shell cat bricks_included.txt)
# The file in modele_server_conf.txt will be user
# to choose the caracteristics of the server (such
# as the port). For example the file
# server_conf.prod.dico
# allow you to run the server on port 80
# If you want to try a server conf file for a temp
# specify it with something like this :
# make r MODELE_SERVER_CONF=<server_conf.prod.dico or something else>
MODELE_SERVER_CONF ?= $(shell (cat link_server_conf.txt | grep -v "^\#"))

include package/info.dico
# Configure server conf 
# include server_conf.test.dico
include $(MODELE_SERVER_CONF)

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
# We create a js file from all cmo files. The first look include all -packages, while the second one include all .cmo
	$(JS_OF_ELIOM) -linkall -o client_part/main_js.js $(foreach brick_cmo, $(shell cat _dep), $(shell if [ ! -z $(shell cat "src/$(brick_cmo)/package/lib_depends.txt") ]; then echo -n "-package "; cat "src/$(brick_cmo)/package/lib_depends.txt"; fi )) $(foreach brick_cmo, $(shell cat _dep_lib), $(brick_cmo)) 

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
## This next lines insert the bricks conf in run_server.xml.
# The order of include is
# - Include files of src/<brick name>/config/ if config/<brick name> doesn't exists
# - Include files of config/<brick name> else
# - Include text from dico src/<brick name>/package/name.dico (only BRICK_NAME)
# - Include text from dico config/<brick name>/main.conf if exists
# - Include text from dico src/<brick name>/config/main.conf
config_brick = echo "--$(1)$(2)--" && if [ -d config/$(1) ]; then $(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --inc --keep_inc --prefix config/$(1)/ --avoid_error; else $(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --inc --keep_inc --prefix src/$(1)/config/ --avoid_error; fi && $(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --dico_content src/$(2)/package/name.dico --avoid_error && $(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --dico_content config/$(1)/main.dico --avoid_error && $(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --dico_content src/$(1)/config/main.dico --avoid_error

update_config: _dep _dep_lib link_server_conf.txt | www/
	@echo "--- Server configuration..."
	cp run_server.xml www/run_server.xml
# Edit the file www/run_server with the system dico
	$(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --dico_content $(MODELE_SERVER_CONF) --avoid_error
# If the brick is only the service part of another brick (presence of
# a file in package/new_brick_conf.txt containing the parent brick),
# the configuration first goes in the service brick, and then in the
# non service one.
	$(foreach brick, $(shell cat _dep), if [ -s "src/$(brick)/package/new_brick_conf.txt" ]; then $(call config_brick,$$(cat "src/$(brick)/package/new_brick_conf.txt"),$(brick)); else $(call config_brick,$(brick),$(brick)); fi && ) true
	$(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --dico_content $(MODELE_SERVER_CONF) --avoid_error
# Remove all resident files.
	$(PHLUOR_TOOLS) copy www/run_server.xml www/run_server.xml --rem_only --avoid_error


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
# sudo chmod 500 /etc/authbind/byport/80
# sudo chown leo /etc/authbind/byport/80
# cd www/; su --preserve-environment -c "ocsigenserver -c run_server.xml -v"
	@echo "$(ERROR_MESSAGE)"
	cd www/; $(OCSIGENSERVER) -c run_server.xml -v
# Compil and run
r: all run

# A fast version of clean, may miss some libraries. To be sure there isn't any problem please use "make clean"
cleanf:
	$(foreach brick, $(shell cat _dep), echo "--- Cleaning $(brick)"; cd src/$(brick)/; make clean_all; cd $(PWD);)
	rm -rf www/ _dep _dep_lib _client _dep_no_uniq

clean:
# with this make clean_all, several libraries may be clean several times,
# and an infinite loop could appear if the inclusion is cyclic, but a simple
# clean could forgot some bricks if _dep is bad due to a wrong configuration
# so this could avoid lot's of problems.
	$(foreach brick, $(shell cat bricks_included.txt) $(shell cat _dep), echo "--- Cleaning $(brick)"; cd src/$(brick)/; make clean_all; cd $(PWD);)
	rm -rf www/ _dep _dep_lib _client _dep_no_uniq

clean_only_current:
	rm -rf www/ _dep _dep_lib _client _dep_no_uniq


PHONY: all compil_bricks copy_bricks run r clean clean_all update_static update_client_part update_server_part update_config
