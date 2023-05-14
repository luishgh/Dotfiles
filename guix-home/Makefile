home:
	GUILE_LOAD_PATH=".." \
	guix home reconfigure ./home.scm

test:
	GUILE_LOAD_PATH=".." \
	guix home build ./home.scm -K

test-no-subs:
	GUILE_LOAD_PATH=".." \
	guix home build ./home.scm -K --no-substitutes

emacs-profile:
	guix package -m ~/.config/guix/manifests/emacs.scm -p ~/.guix-extra-profiles/emacs/emacs
