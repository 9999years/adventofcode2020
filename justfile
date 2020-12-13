day n:
	ghc Day{{n}}.hs
	./Day{{n}} < input-{{n}}.txt

clean:
	fd . \
		--no-ignore-vcs \
		--type f \
		-e "hi" \
		-e "o" \
		-X rm {} \;
	fd 'Day\d+$' \
		--no-ignore-vcs \
		--type x \
		-X rm {} \;
