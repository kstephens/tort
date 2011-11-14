SCARLET=../../scarlet/bin/scarlet

slides_src=metadecomp.slides.textile
slides_html=metadecomp.slides/index.html

all : $(slides_html)

$(slides_html): $(slides_src)
	mkdir -p $(dir $(slides_html))
	$(SCARLET) -f html -g $(dir $(slides_html))
	$(SCARLET) -f html $(slides_src) > $@
	open $@


