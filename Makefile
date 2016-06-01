serve:
	bundle exec jekyll server --watch --safe

build: clean
	JEKYLL_ENV=production bundle exec jekyll build

debug: clean
	JEKYLL_ENV=production bundle exec jekyll build --verbos

clean:
	rm -rf _site

.PHONY: \
	clean \
	serve
