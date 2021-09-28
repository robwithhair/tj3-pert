.DEFAULT_GOAL := build
.PHONY : clean build check-passphrase-env

example: 
	"${CURDIR}/build/tj3-pert" < "${CURDIR}/project.tjp" > "${CURDIR}/build/taskjuggler/project.tjp"

