config[:enviroment] = :development

activate :livereload,
  no_swf: true

activate :syntax

config[:site_url] = 'http://lpil.uk'
config[:title] = 'lpil'
config[:description] = 'The website of a code monkey named Louis Pilfold.'

config[:sass_assets_paths] = ['scss']

config[:markdown_engine] = :redcarpet
config[:markdown] = {
  fenced_code_blocks: true,
  smartypants: true,
}

configure :build do
  config[:enviroment] = :production
  activate :relative_assets
  activate :minify_css
  activate :minify_javascript
  activate :minify_html
  activate :gzip
end

activate :blog do |blog|
  blog.layout = 'blog'
  blog.permalink = 'blog/:title'
  blog.sources = 'posts/:year-:month-:day-:title.html'
end
activate :directory_indexes # Must go after blog

page 'feed.xml', layout: false
