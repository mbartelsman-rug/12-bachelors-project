.DELETE_ON_ERROR:

all: semantics dune_proj

dune_proj:
	dune build

semantics: actors_semantics channels_semantics act2ch_semantics ch2act_semantics

actors_dir := lib/actors/
actors_sem_dir := $(actors_dir)semantics/
actors_sem_tgt := $(actors_sem_dir)_tmp.sk
actors_ml_tgt := $(actors_dir)semantics.ml

$(actors_sem_tgt):
	cat $(actors_sem_dir)*.sk > $(actors_sem_tgt)

$(actors_ml_tgt): $(actors_sem_tgt)
	necroml $(actors_sem_tgt) -o $(actors_ml_tgt)

actors_semantics: $(actors_ml_tgt)
	rm $(actors_sem_tgt)

channels_dir := lib/channels/
channels_sem_dir := $(channels_dir)semantics/
channels_sem_tgt := $(channels_sem_dir)_tmp.sk
channels_ml_tgt := $(channels_dir)semantics.ml

$(channels_sem_tgt):
	cat $(channels_sem_dir)*.sk > $(channels_sem_tgt)

$(channels_ml_tgt): $(channels_sem_tgt)
	necroml $(channels_sem_tgt) -o $(channels_ml_tgt)

channels_semantics: $(channels_ml_tgt)
	rm $(channels_sem_tgt)

act2ch_dir := lib/act2ch/
act2ch_sem_dir := $(act2ch_dir)semantics/
act2ch_sem_tgt := $(act2ch_sem_dir)_tmp.sk
act2ch_ml_tgt := $(act2ch_dir)semantics.ml

$(act2ch_sem_tgt):
	cat $(act2ch_sem_dir)*.sk > $(act2ch_sem_tgt)

$(act2ch_ml_tgt): $(act2ch_sem_tgt)
	necroml $(act2ch_sem_tgt) -o $(act2ch_ml_tgt)

act2ch_semantics: $(act2ch_ml_tgt)
	rm $(act2ch_sem_tgt)

ch2act_dir := lib/ch2act/
ch2act_sem_dir := $(ch2act_dir)semantics/
ch2act_sem_tgt := $(ch2act_sem_dir)_tmp.sk
ch2act_ml_tgt := $(ch2act_dir)semantics.ml

$(ch2act_sem_tgt):
	cat $(ch2act_sem_dir)*.sk > $(ch2act_sem_tgt)

$(ch2act_ml_tgt): $(ch2act_sem_tgt)
	necroml $(ch2act_sem_tgt) -o $(ch2act_ml_tgt)

ch2act_semantics: $(ch2act_ml_tgt)
	rm $(ch2act_sem_tgt)


