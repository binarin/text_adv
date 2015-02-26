#!/bin/sh
set -e
cd src
erl -pa ../_build/lib/sync/ebin -eval 'sync:go()' -eval 'compile:file(text_adv_game)' -eval 'text_adv_game:console_play()' -eval 'init:stop()' -noshell
