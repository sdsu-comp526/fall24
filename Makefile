build:
	jupyter-book build .

publish:
	ghp-import -n -p -f _build/html

clean:
	rm -r _build
