home:
	GUILE_LOAD_PATH=".." \
	guix home reconfigure ./home.scm

test:
	GUILE_LOAD_PATH=".." \
	guix home build ./home.scm -K

test-no-subs:
	GUILE_LOAD_PATH=".." \
	guix home build ./home.scm -K --no-substitutes
