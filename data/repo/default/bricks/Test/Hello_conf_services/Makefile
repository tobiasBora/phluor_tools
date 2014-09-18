# This file compile the client and server part of a brick

# ==============================================
# You can edit this part, but it's often useless
# ==============================================

# See this file to configure the name of the library
include package/name.dico

CLIENT_SOURCES=$(wildcard *.eliom)
SERVER_SOURCES=$(wildcard *.ml) $(wildcard *.eliom)

# Bricks needed : give the name of the main folders (from root)
# Il you have server/client only depends, you should write here the
# common depends and write every depends in package/brick_depends
# The last dependance is usefull to load the services but don't
# require it's installation as another package during the main package
CLIENT_SERVER_DEPENDS=$(shell cat package/brick_depends.txt) Test/Hello_conf_services/services
SERVER_ONLY_DEPENDS=
CLIENT_ONLY_DEPENDS=

# Ocaml packages : separate it with a comma
# If client packages are different from server packages, save as above
SERVER_PACKAGES=$(shell cat package/lib_depends.txt)
CLIENT_PACKAGES=$(shell cat package/lib_depends.txt)

SERVER_INCLUDE_DIRS=
CLIENT_INCLUDE_DIRS=

# Points in the src folder (../../ if two sub directories)
SRC=../../

include $(SRC)../Makefile.conf
include $(SRC)../Makefile.brick_comp
