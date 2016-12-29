all:
	elm-make Main.elm --output schlangi.html

clean:
	rm -f schlangi.html
