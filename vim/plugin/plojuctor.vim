aug Plojuctor
	au!

	" loader
	au FileType clojure map <leader>1 <esc>G$a(use 'plojuctor.core :reload-all)<cr><esc>

	" move to first slide
	au FileType clojure map <leader>0 <esc>G$a(move-page! 0)<cr><esc>
	" move to previous slide
	au FileType clojure map <leader>k <esc>G$a(prev-page!)<cr><esc>
	" move to next slide
	au FileType clojure map <leader>j <esc>G$a(next-page!)<cr><esc>

aug END
