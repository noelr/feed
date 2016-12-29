all:
	elm-make Main.elm --output index.html

clean:
	rm -f index.html
