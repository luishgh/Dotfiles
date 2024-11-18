home:
	GUILE_LOAD_PATH=".." \
	guix home reconfigure ./home.scm
	make extra

test:
	GUILE_LOAD_PATH=".." \
	guix home build ./home.scm -K

test-no-subs:
	GUILE_LOAD_PATH=".." \
	guix home build ./home.scm -K --no-substitutes

emacs-profile:
	guix package -m ~/.config/guix/manifests/emacs.scm -p ~/.guix-extra-profiles/emacs/emacs

mail-profile:
	guix package -m ~/.config/guix/manifests/mail.scm -p ~/.guix-extra-profiles/mail/mail

extra:
	make -C "$$(guix build browserpass-native)"/lib/browserpass hosts-chromium-user
	make -C "$$(guix build browserpass-native)"/lib/browserpass hosts-firefox-user
