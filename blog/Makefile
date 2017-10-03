# --------------------------------------------------------------------------------

# [make billet] creates the blog entry.

.PHONY: billet clean

billet: billet.html

clean:
	rm -f billet.html

%.html: %.markdown
	pandoc -s $< -c style.css > $@
