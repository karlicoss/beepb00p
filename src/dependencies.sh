#!/bin/bash -eux
apt update
apt full-upgrade -y
apt install -y entr # watching changes
apt install -y emacs-nox pandoc  # TODO use latest emacs maybe?
apt install -y elpa-org elpa-dash elpa-s  # TODO use latest org-mode maybe?
apt install -y graphviz
