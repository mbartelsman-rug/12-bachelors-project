lib_folder := lib/
sem_folder := lib/semantics/
sem_target_name := __tmp.sk
ocaml_target_name := semantics.ml

sem_files  := $(sem_folder)*.sk
sem_target := $(sem_folder)$(sem_target_name)
ocaml_target := $(lib_folder)$(ocaml_target_name)

.DELETE_ON_ERROR:

semantics: $(sem_target)

$(sem_target):
	cat     $(sem_files) > $(sem_target)
	necroml $(sem_target) -o $(ocaml_target)
	rm      $(sem_target)
